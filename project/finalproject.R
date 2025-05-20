# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(DT)
library(plotly)

# Load Data
mh <- read.csv("mental_health_finaldata_1 (1).csv")
suicide <- read.csv("master.csv") 
china <- read.csv("SuicideChina.csv")

# --- Data Preprocessing ---
mh <- mh %>% mutate(AgeGroup = case_when(
  Age == "16-20" ~ "15-24 years",
  Age == "20-25" ~ "15-24 years",
  Age == "25-30" ~ "25-34 years",
  Age == "30-Above" ~ "35-54 years"
))

suicide <- suicide %>%
  filter(country == "United States") %>%
  mutate(age = as.character(age), sex = tolower(sex))

mh$Gender <- tolower(mh$Gender)

joined_data <- inner_join(mh, suicide, by = c("AgeGroup" = "age", "Gender" = "sex"))

joined_data$AgeGroup <- factor(joined_data$AgeGroup,
                               levels = c("15-24 years", "25-34 years", "35-54 years"),
                               ordered = TRUE)

cols_to_factor <- c('Occupation', 'Growing_Stress', 'Quarantine_Frustrations',
                    'Changes_Habits', 'Mental_Health_History', 'Weight_Change', 'Mood_Swings',
                    'Coping_Struggles', 'Work_Interest', 'Social_Weakness')
joined_data[cols_to_factor] <- lapply(joined_data[cols_to_factor], as.factor)

# Seasonal average table for prediction
seasonal_suicide <- joined_data %>%
  group_by(year, AgeGroup) %>%
  summarise(avg_rate = mean(suicides_no / population * 100000, na.rm = TRUE), .groups = "drop")

predict_suicide_rate <- function(year_input, age_group_input) {
  rate <- seasonal_suicide %>%
    filter(year == year_input, AgeGroup == age_group_input) %>%
    pull(avg_rate)
  
  if (length(rate) == 0) {
    return("No data for that year/age group")
  } else {
    return(paste0("Estimated Suicide Rate: ", round(rate, 1), " per 100k"))
  }
}

# UI
ui <- navbarPage("Mental Health & Suicide Trends",
                 
                 tabPanel("Home",
                          titlePanel("Final Project: Understanding Mental Health & Suicide"),
                          
                          h3("Project Documentation"),
                          
                          h4("Scope"),
                          p("This Shiny app explores mental health indicators during the COVID-19 pandemic and correlates them with suicide statistics from WHO data."),
                          p("It integrates three datasets: survey-based mental health data, global suicide data, and China-specific data. Visualizations aim to uncover trends by age, gender, geography, and coping mechanisms."),
                          
                          h4("Methodology"),
                          tags$ol(
                            tags$li("Mental health survey responses categorized into age groups."),
                            tags$li("Suicide statistics aggregated by age and gender."),
                            tags$li("Coping strategies, trends, and predictive modeling analyzed."),
                            tags$li("Seasonal suicide rate prediction implemented."),
                            tags$li("Case study visualized by method and gender.")
                          ),
                          
                          h4("Features"),
                          tags$ul(
                            tags$li("Mood swings by gender (bar plot)"),
                            tags$li("Coping struggles (pie chart)"),
                            tags$li("Suicide heat map (tile chart)"),
                            tags$li("Rate predictions by age/year (custom model)"),
                            tags$li("China case by suicide method (bar)")
                          ),
                          
                          h4("Backlog of Ideas"),
                          tags$ul(
                            tags$li("Adding more demographic filters (e.g., education, region)"),
                            tags$li("Expanding prediction model features"),
                            tags$li("Dynamic map filtering and tooltip enhancements"),
                            tags$li("Enhanced interactivity with plotly or drill-down visualizations")
                          ) 
                 ), 
                 
                 
                 
                 tabPanel("Gender & Mood Swings", mainPanel(plotOutput("genderMoodPlot"))),
                 
                 tabPanel("Coping Strategies", mainPanel(plotOutput("copingPie"))),
                 
                 tabPanel("Suicide Rate Heat Map", mainPanel(plotOutput("heatMapCountryYear"))),
                 
                 tabPanel("Suicide Rate Boxplot", mainPanel(plotOutput("boxPlotSuicideRate"))),
                 
                 tabPanel("Prediction",
                          fluidRow(
                            column(4,
                                   selectInput("pred_agegrp", "Age Group:", choices = unique(joined_data$AgeGroup)),
                                   sliderInput("pred_year", "Year:", min = min(joined_data$year), max = max(joined_data$year), value = 2014)
                            ),
                            column(8,
                                   tags$h4("Predicted Suicide Rate"),
                                   verbatimTextOutput("suicide_prediction")
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
      labs(title = "Suicides Over Time", y = "Suicides", x = "Year") +
      theme_minimal()
  })
  
  output$genderMoodPlot <- renderPlot({
    ggplot(joined_data, aes(x = Gender, fill = Mood_Swings)) +
      geom_bar(position = "fill") +
      labs(title = "Mood Swings by Gender", y = "Proportion", x = "Gender") +
      theme_minimal()
  })
  
  output$copingPie <- renderPlot({
    coping_counts <- joined_data %>%
      count(Coping_Struggles) %>%
      mutate(percent = round(100 * n / sum(n), 1),
             label = paste0(Coping_Struggles, ": ", percent, "%"))
    
    ggplot(coping_counts, aes(x = "", y = n, fill = Coping_Struggles)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white") +
      scale_fill_manual(values = c("Yes" = "#00BFC4", "No" = "#F8766D")) +
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
  
  output$boxPlotSuicideRate <- renderPlot({
    ggplot(joined_data, aes(x = AgeGroup, y = suicides_no / population * 100000, fill = AgeGroup)) +
      geom_boxplot() +
      labs(title = "Suicide Rate per 100k by Age Group", x = "Age Group", y = "Rate") +
      theme_minimal()
  })
  
  output$suicide_prediction <- renderText({
    req(input$pred_year, input$pred_agegrp)
    predict_suicide_rate(input$pred_year, input$pred_agegrp)
  })
  
  output$chinaPlot <- renderPlot({
    china_filtered <- china %>%
      filter(method == input$method) %>%
      filter(Sex %in% c("male", "female"))
    
    ggplot(china_filtered, aes(x = Year, fill = Sex)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("female" = "#F8766D", "male" = "#00BFC4")) +
      labs(title = paste("China Suicide Method:", input$method), y = "Cases", fill = "Gender") +
      theme_minimal()
  })
}

# Run the App
shinyApp(ui = ui, server = server)
