# Load packages
library(tidyverse)  
library(readr)      
library(shiny)
library(ggplot2)
library(lubridate)
library(DT)
library(leaflet)
library(plotly)


setwd("~/Documents/finalproject")  

mh <- read.csv("mental_health_finaldata_1 (1).csv")
suicide <- read.csv("master.csv")
china <- read.csv("SuicideChina.csv")


# JOIN Mental Health + Suicide Data

# Step 1: Normalize age columns in mh
mh <- mh %>% mutate(AgeGroup = case_when(
  Age == "16-20" ~ "15-24 years",
  Age == "20-25" ~ "15-24 years",
  Age == "25-30" ~ "25-34 years",
  Age == "30-Above" ~ "35-54 years"
))

# Step 2: Summarize suicide data by age and sex (pivot table)
suicide_summary <- suicide %>%
  filter(country == "United States") %>%
  mutate(age = as.character(age),
         sex = tolower(sex)) %>%
  group_by(age, sex) %>%
  summarise(
    avg_suicides = mean(suicides_no, na.rm = TRUE),
    total_suicides = sum(suicides_no, na.rm = TRUE),
    avg_suicides_per_100k = mean(`suicides/100k pop`, na.rm = TRUE),
    avg_gdp = mean(as.numeric(gsub(",", "", ` gdp_for_year ($) `)), na.rm = TRUE),
    avg_hdi = mean(`HDI for year`, na.rm = TRUE),
    .groups = "drop"
  )

# Step 3: Lowercase mh Gender to match suicide_summary sex
mh$Gender <- tolower(mh$Gender)

# Step 4: Join mental health data to aggregated suicide data
joined_data <- inner_join(mh, suicide_summary, by = c("AgeGroup" = "age", "Gender" = "sex"))

# Clean Joined Data
joined_data$AgeGroup <- factor(joined_data$AgeGroup,
                               levels = c("15-24 years", "25-34 years", "35-54 years"), ordered = TRUE)

cols_to_factor <- c('Occupation', 'Growing_Stress', 'Quarantine_Frustrations', 
                    'Changes_Habits', 'Mental_Health_History', 'Weight_Change', 'Mood_Swings', 
                    'Coping_Struggles', 'Work_Interest', 'Social_Weakness')

# Only apply factor conversion to columns that actually exist in joined_data
cols_to_factor <- cols_to_factor[cols_to_factor %in% colnames(joined_data)]
joined_data[cols_to_factor] <- lapply(joined_data[cols_to_factor], as.factor)



# UI

ui <- navbarPage("Mental Health & Suicide Trends", 
                 
                 tabPanel("Home",
                          titlePanel("Final Project: Understanding Mental Health & Suicide"),
                          p("This project merges survey data on quarantine-related mental health struggles with global suicide 
       statistics from the WHO to explore patterns in stress, mood, and mental health history. We also present
       a case study from China with method-based suicide breakdown.")
                 ),
                 
                 tabPanel("Survey & Suicide Trends",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("agegrp", "Select Age Group:", choices = levels(joined_data$AgeGroup))
                            ),
                            mainPanel(
                              plotOutput("stressPlot"),
                              plotOutput("suicideRatePlot")
                            )
                          )
                 ),
                 
                 tabPanel("China Case Study",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("method", "Choose Method:", choices = unique(china$method))
                            ),
                            mainPanel(
                              plotOutput("chinaPlot")
                            )
                          )
                 )
)

tabPanel("Suicide Rate Boxplot",
         mainPanel(
           plotOutput("boxPlotSuicideRate")
         )
)

tabPanel("Coping Strategies",
         mainPanel(
           plotOutput("copingPie")
         )
)


# Server

server <- function(input, output) {
  
  output$stressPlot <- renderPlot({
    data <- joined_data %>% filter(AgeGroup == input$agegrp)
    ggplot(data, aes(x = Growing_Stress, fill = Growing_Stress)) +
      geom_bar() +
      labs(title = "Stress Levels by Age Group", x = "Stress", y = "Count") +
      theme_minimal()
  })
  
  output$suicideRatePlot <- renderPlot({
    data <- joined_data %>% filter(AgeGroup == input$agegrp)
    ggplot(data, aes(x = year, y = suicides_no, color = generation)) +
      geom_line(stat = "summary", fun = sum) +
      labs(title = "Suicides Over Time (Same Age Group)", y = "Suicides", x = "Year") +
      theme_minimal()
  })

  output$genderMoodPlot <- renderPlot({
    ggplot(joined_data, aes(x = Gender, fill = Mood_Swings)) +
      geom_bar(position = "fill") +
      labs(title = "Mood Swings by Gender", y = "Proportion", x = "Gender") +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal()
  })
  
  output$chinaPlot <- renderPlot({
    data <- china %>% filter(method == input$method)
    ggplot(data, aes(x = Year, fill = Sex)) +
      geom_bar(position = "dodge") +
      labs(title = paste("China Suicide Method Trends:", input$method),
           x = "Year", y = "Cases") +
      theme_minimal()
  }) 
  
}

output$boxPlotSuicideRate <- renderPlot({
  ggplot(joined_data, aes(x = AgeGroup, y = avg_suicides_per_100k, fill = AgeGroup)) +
    geom_boxplot() +
    labs(title = "Suicide Rate per 100k by Age Group", x = "Age Group", y = "Avg Suicides per 100k") +
    theme_minimal()
})

output$copingPie <- renderPlot({
  coping_counts <- joined_data %>%
    count(Coping_Struggles)
  
  ggplot(coping_counts, aes(x = "", y = n, fill = Coping_Struggles)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar(theta = "y") +
    labs(title = "Coping Struggles Distribution") +
    theme_void()
})

output$heatMapCountryYear <- renderPlot({
    heat_data <- suicide %>%
      group_by(country, year) %>%
      summarise(rate = sum(suicides_no, na.rm = TRUE) / sum(population, na.rm = TRUE) * 100000, .groups = "drop")

    ggplot(heat_data, aes(x = year, y = reorder(country, -rate), fill = rate)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "C") +
      labs(title = "Suicide Rate per 100k by Country and Year", x = "Year", y = "Country", fill = "Rate") +
      theme_minimal()
  })
}

output$modelSummary <- renderPrint({
    model <- lm(avg_suicides_per_100k ~ avg_gdp + avg_hdi, data = joined_data)
    summary(model)
  })

  output$predictionPlot <- renderPlot({
    model <- lm(avg_suicides_per_100k ~ avg_gdp + avg_hdi, data = joined_data)
    joined_data$predicted <- predict(model, joined_data)

    ggplot(joined_data, aes(x = avg_suicides_per_100k, y = predicted)) +
      geom_point(color = "tomato", alpha = 0.6) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "steelblue") +
      labs(title = "Predicted vs Actual Suicide Rates",
           x = "Actual Suicide Rate per 100k",
           y = "Predicted Suicide Rate") +
      theme_minimal()
  })
}

# Run the App

shinyApp(ui = ui, server = server)

