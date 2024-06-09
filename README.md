# Overview

This Shiny app allows users to visualize relationships between various factors affecting property prices in York, England, and predict the price of a property based on user inputs. The app uses a dataset on property prices to generate interactive visualizations and predictive models.

## Features
- Variable Relationships: Explore the relationships between different variables such as price, postcode, floor area, number of rooms, energy efficiency, and environmental impact. Select variables for the X-axis and Y-axis to generate dynamic visualizations, including scatter plots, box plots, and stacked bar charts.
- House Price Predictions: Enter property details to predict its current value. Adjust the year, floor area, energy efficiency, and postcode to see how these factors impact the predicted price. The app also visualizes predicted changes in house prices over time.

## Installation

To run this Shiny app, you need to install the following R packages:
- shiny
- ggplot2
- dplyr
- tidyverse
- mgcv
- viridis
- scales

## Data

The dataset used in this app contains information on property prices in York, England, including:

- Year: The year the property was sold.
- Price: The sale price of the property.
- Postcode: The postcode of the property.
- Floor Area: The total floor area of the property in square meters.
- Number of Rooms: The number of rooms in the property.
- Energy Efficiency: The current energy efficiency rating of the property.
- Environmental Impact: The current environmental impact rating of the property.

## Predictive Model

The app uses a Generalized Additive Model (GAM) with a response distributed by the gamma distribution and a log link function. The model aims to predict property prices based on the year, postcode, floor area, and energy efficiency.




