# Analysis and Prediction of Mental Health Conditions and Suicide

## Authors: Diya, Philip    shiny link: https://diya11.shinyapps.io/finalproject/

# Overview

This project presents an interactive **Shiny dashboard** analyzing how mental health challenges during the COVID-19 pandemic may relate to suicide trends across age groups and countries. The app merges mental health survey data with WHO suicide statistics and includes a case study from China. Built in R, the dashboard includes bar charts, pie charts, heatmaps, a seasonal prediction model, and gender-based method analysis.

---

## Data Source

The dataset includes three separate CSV files:

- `mental_health_finaldata_1 (1).csv`
- `master.csv`
- `SuicideChina.csv`

Each file contains:
- Age, gender, and stress-related survey responses
- WHO suicide data by country, year, sex, and age
- Method-specific suicide data from China by year and gender

These datasets were cleaned and merged using R before being visualized in the Shiny app.

---

## Data Preparation

Before building the dashboard, the following steps were taken:

### Load and Clean Data

```r
mh <- read.csv("mental_health_finaldata_1 (1).csv")
suicide <- read.csv("master.csv")
china <- read.csv("SuicideChina.csv")
