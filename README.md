# Energy Usage Prediction: Project Summary

## Introduction
This project aims to develop a forecast model for predicting residential energy usage during July, considering various factors such as household characteristics, historical energy consumption patterns, and weather conditions. The goal is to provide actionable insights to the energy company (eSC) for strategic planning and encouraging energy-saving practices among customers.

## Research Objectives
1. Analyze the impact of household income on energy consumption.
2. Investigate the effects of weather conditions and climate zones on energy consumption.
3. Examine how the size of the house, number of occupants, and number of bedrooms influence energy consumption.
4. Explore the relationship between wall insulation types and energy consumption.
5. Identify cities with the highest levels of energy consumption.

## Data Sources
The project utilizes three distinct datasets:
1. Static Data: Contains information about households, such as income, house size, and insulation types.
2. Weather Data: Includes weather conditions and climate zones.
3. Energy Usage Data: Provides hourly energy consumption data.

## Data Preparation
The data was refined to focus on hourly energy usage and weather data for the month of July. The energy usage data was merged with the static data based on the 'bldg_id' column, and the weather data was merged based on the 'in.county' column.

## Exploratory Analysis
The exploratory analysis involved:
- Identifying relevant fields for each research question.
- Analyzing the correlation between income levels and energy consumption.
- Examining the impact of climate zones and weather conditions on energy consumption.
- Investigating the relationship between house characteristics (size, occupants, bedrooms) and energy consumption.
- Exploring the effect of wall insulation types on energy consumption.

## Model Selection and Predictive Modeling
Three models were considered for predicting energy usage:
1. Linear Regression Models: Provided insights into the contribution of individual factors but had low accuracy.
2. Gradient Boosting Model: Captured non-linear relationships but still exhibited low accuracy.
3. Decision Tree Model: Performed best in capturing non-linear relationships and achieved the highest accuracy.

## Conclusion
This project aims to provide eSC with a predictive model for residential energy usage during July, considering various factors. The findings will enable eSC to implement targeted initiatives for encouraging energy-saving practices, contributing to grid stability and environmental sustainability.
