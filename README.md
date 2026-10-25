# Startup Funding Success Prediction & Investor Pattern Analysis

## Overview
This project analyzes startup funding records using R and Shiny to understand funding trends across industries, cities, years, and investment types.

## Objectives
- Analyze startup funding patterns.
- Identify leading industries and cities.
- Explore investor and investment-type patterns.
- Predict funding amount categories using Random Forest.

## Technologies Used
- R
- Shiny
- Random Forest
- ggplot2
- dplyr

## Dashboard Features
- Funding overview and summary statistics.
- Funding trends by year.
- Industry and city analysis.
- Investor pattern analysis.
- Funding amount category prediction.
- Model evaluation metrics.

## Prediction Model
The Random Forest model classifies funding records as Higher or Lower than the median funding amount. It does not directly predict whether a startup will succeed.

## How to Run
1. Install R and RStudio.
2. Install the packages required by `app.R`.
3. Open the project folder in RStudio.
4. Run this command:

```r
shiny::runApp()
```

## Limitations
Historical funding patterns do not guarantee future startup success. Results depend on the dataset and model settings.

## References
- R: https://www.r-project.org/
- Shiny: https://shiny.posit.co/
