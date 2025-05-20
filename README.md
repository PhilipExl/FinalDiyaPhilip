# Analysis and Prediction of Mental Health Conditions and Suicide


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




