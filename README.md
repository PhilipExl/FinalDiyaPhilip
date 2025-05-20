# Analysis and Prediction of Mental Health Conditions and Suicide
https://github.com/PhilipExl/FinalDiyaPhilip/blob/main/images/suicide.png

## Authors
**Diya Adhikari** & **Philip**

---

##  Shiny App Link
[Click here to view the app](https://diya11.shinyapps.io/finalproject/)

---

## Overview

This project presents an interactive **Shiny dashboard** that analyzes how mental health indicators during the COVID-19 pandemic relate to global and country-specific suicide statistics. By integrating multiple datasets, the app visualizes patterns based on age, gender, and coping behaviors and offers a predictive model for suicide rates.

---

## Data Sources

- `mental_health_finaldata_1 (1).csv` – Mental health survey data (COVID-19 era)
- `master.csv` – WHO global suicide statistics
- `SuicideChina.csv` – Gender and method-specific suicide data from China

---

##  Key Features

- Mood swings by gender (bar plot)
- Coping strategies visualization (pie chart)
- Heat map of global suicide rates
- Suicide rate prediction by age group and year
- China case study by suicide method and gender

---

##  Code Snippets

### 1. Data Preprocessing

```r
mh <- mh %>% mutate(AgeGroup = case_when(
  Age == "16-20" ~ "15-24 years",
  Age == "20-25" ~ "15-24 years",
  Age == "25-30" ~ "25-34 years",
  Age == "30-Above" ~ "35-54 years"
))
```
### 2. Joining Datasets

```r
joined_data <- inner_join(mh, suicide, by = c("AgeGroup" = "age", "Gender" = "sex"))
```

### 3. Suicide Rate Prediction Function

```r
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
```
### 4. Coping Struggles Pie Chart

```r
ggplot(coping_counts, aes(x = "", y = n, fill = Coping_Struggles)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void()

```
### 5. Global Heat Map

```r
ggplot(heat_data, aes(x = year, y = reorder(country, -rate), fill = rate)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c()
```
### 6.  Gender-Based Mood Swing Plot

```r
ggplot(joined_data, aes(x = Gender, fill = Mood_Swings)) +
  geom_bar(position = "fill") +
  theme_minimal()

```
### Prediction Model

   ### Build Seasonal Table
```r
   seasonal_suicide <- joined_data %>%
  group_by(year, AgeGroup) %>%
  summarise(avg_rate = mean(suicides_no / population * 100000, na.rm = TRUE), .groups = "drop")
```
### Prediction Function
```r
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

```

### China Case Study – Suicide Method by Gender
```r
china_filtered <- china %>%
  filter(method == input$method, Sex %in% c("male", "female"))

ggplot(china_filtered, aes(x = Year, fill = Sex)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("female" = "#F8766D", "male" = "#00BFC4")) +
  labs(title = paste("China Suicide Method:", input$method), y = "Cases", fill = "Gender") +
  theme_minimal()
```

### Future Improvements
- Add more demographic filters (e.g., education, region)

- Expand the prediction model with more variables

- Include interactive drill-down capability

- Improve user experience with dynamic tooltips and filtering


### Technologies Used

- shiny for layout and interactivity

- ggplot2 for data visualization

- dplyr and lubridate for data wrangling

- plotly for optional enhancements


