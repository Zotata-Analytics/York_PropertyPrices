## Visualise and Predict Property Prices in York, England ##

# Install required libraries

# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("mgcv")
# install.packages("viridis")
# install.packages("scales")

# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(mgcv)
library(viridis)
library(scales)

# Set working directory and read in data

# setwd("~/York_PropertyPrices")
property_prices <- read.csv("YORK.csv", fileEncoding = "UTF-8-BOM")

# Data cleaning and prep
property_prices <- property_prices %>%
  select(year, price, postcode, tfarea, numberrooms, CURRENT_ENERGY_EFFICIENCY, ENVIRONMENT_IMPACT_CURRENT) %>%
  rename(
    Year = year,
    Price = price,
    Postcode = postcode,
    Floor_Area = tfarea,
    Number_of_Rooms = numberrooms,
    Energy_Efficiency = CURRENT_ENERGY_EFFICIENCY,
    Environmental_Impact = ENVIRONMENT_IMPACT_CURRENT) %>%
  mutate(Postcode = substr(Postcode, 1, 3),
         Price = as.numeric(Price),
         Energy_Efficiency = as.numeric(Energy_Efficiency),
         Floor_Area = as.numeric(Floor_Area),
         Environmental_Impact = as.numeric(Environmental_Impact),
         Number_of_Rooms = as.factor(Number_of_Rooms))

# Defining the GLM model for house price predictions
glm_model <- gam(Price ~ Year + Postcode + poly(Floor_Area, 4) + poly(Energy_Efficiency, 3),
                 data = property_prices, family = Gamma(link = "log"), method = "REML", k = 20)

# App UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #warning_message {
        color: red;
        font-size: 16px;
        font-weight: bold;}
      #predicted_price_output {
        color: black;
        font-size: 18px;
        font-weight: bold;}"))),
  titlePanel("Property Price Analysis", windowTitle = "Property Prices App"),
  tabsetPanel(
    # Tab 1: Variable Relationships
    tabPanel("Variable Relationships",
             sidebarLayout(
               sidebarPanel(
                 # Input controls for X-axis variable
                 selectInput("x_var", "Select X-axis Variable", 
                             choices = c("Price" = "Price", "Postcode" = "Postcode",
                                         "Floor Area" = "Floor_Area", "Number of Rooms" = "Number_of_Rooms",
                                         "Energy Efficiency" = "Energy_Efficiency", 
                                         "Environmental Impact" = "Environmental_Impact")),
                 # Input controls for Y-axis variable
                 selectInput("y_var", "Select Y-axis Variable", 
                             choices =c("Price" = "Price", "Postcode" = "Postcode",
                                        "Floor Area" = "Floor_Area", "Number of Rooms" = "Number_of_Rooms",
                                        "Energy Efficiency" = "Energy_Efficiency", 
                                        "Environmental Impact" = "Environmental_Impact")),
                 verbatimTextOutput("warning_message"),
                 # Selectize input for filtering by postcode
                 selectizeInput("postcode", "Select Postcode",
                                choices = c("All", unique(property_prices$Postcode))),
                 textOutput("selected_postcodes")),
               # Main title, description and plot for Tab 1
               mainPanel(
                 hr(),
                 h3("Visualise Variable Relationships", style = "text-align: center; font-weight: bold;"),
                 p("This tab allows you to explore the relationship between different variables within the property prices dataset."),
                 p("Select variables for the X-axis and Y-axis, then choose specific/all postcodes, and the graph will generate the visualization accordingly."),
                 plotOutput("visualise_plot", width = "100%", height = 500)))),
    # Tab 2: House Price Predictions
    tabPanel("House Price Predictions",
             sidebarLayout(
               sidebarPanel(width = 4,
                            # Input controls for house details
                            numericInput("year_input", "Enter Year (From 2010-2030):", 
                                         value = 2019, min = 2010, max = 2030),
                            numericInput("floor_area_input", "Enter Floor Area (From 20-1000 sq. m):", 
                                         value = 80, min = 20, max = 1000),
                            numericInput("energy_efficiency_input", "Enter Energy Efficiency (From 1-100):", 
                                         value = 60, min = 1, max = 100),
                            selectInput("postcode_input", "Select Postcode",
                                        choices = c(unique(property_prices$Postcode)),selected = "YO1"),
                            verbatimTextOutput("predicted_price_output")),
               # Main title, description and plot for Tab 2
               mainPanel(width = 8,
                         h3("House Price Predictions", style = "text-align: center; font-weight: bold;"),
                         p("Enter details about your property to see the estimated current value and visualisation of predicted changes."),
                         p("Increase the input values to see the impact a hypothetical improvement would have on the value of your property."),
                         plotOutput("predicted_plot", width = "100%", height = 500))))))

# App server logic
server <- function(input, output) {
  # filter data based on postcode selection
  postcode_filter <- reactive({
    if ("All" %in% input$postcode) {
      data <- property_prices
    } else {
      data <- property_prices %>% filter(Postcode %in% input$postcode)
    }
    data
  })
  
  # check if x and y variables are the same
  are_variables_equal <- reactive({
    input$x_var == input$y_var
  })
  
  # check if both x and y variables are continuous
  are_both_continuous <- reactive({
    is.numeric(property_prices[[input$x_var]]) && is.numeric(property_prices[[input$y_var]])
  })
  
  # check if y variable is continuous
  is_y_continuous <- reactive({
    is.numeric(property_prices[[input$y_var]])
  })
  
  # check if x variable is continuous
  is_x_continuous <- reactive({
    is.numeric(property_prices[[input$x_var]])
  })
  
  # Output: Variables relationship visualisations
  output$visualise_plot <- renderPlot({
    if (are_variables_equal()) {
      return(NULL)
    }
    
    # Stacked bar chart of x = Postcode, y = Number of Rooms
    if (input$x_var == "Postcode" && input$y_var == "Number_of_Rooms") {
      custom_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                          "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
                          "#cab2d6", "#ffff99", "#8da0cb", "#66c2a5", "#b2182b",
                          "#d6604d", "#f4a582", "#92c5de", "#d1e5f0", "#fee08b")
      
      ggplot(postcode_filter(), aes(x = factor(Postcode), fill = factor(Number_of_Rooms, levels = 20:1))) +
        geom_bar(position = "stack") + scale_fill_manual(name = "Number of Rooms", values = custom_palette) +
        labs(title = "Stacked Bar Chart of Number of Rooms by Postcode", x = "Postcode", y = "Count") +
        theme_minimal() + theme(text = element_text(size = 14), axis.text = element_text(size = 12, face = "bold"),
                                plot.title = element_text(hjust = 0.5, face = "bold"), 
                                axis.title = element_text(face = "bold"))
      # Stacked bar chart of x = Number of Rooms, y = Postcode
    } else if (input$x_var == "Number_of_Rooms" && input$y_var == "Postcode") {
      custom_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a",
                          "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f",
                          "#cab2d6", "#ffff99", "#8da0cb", "#66c2a5", "#b2182b",
                          "#d6604d", "#f4a582", "#92c5de", "#d1e5f0", "#fee08b")
      
      ggplot(postcode_filter(), aes(x = factor(Number_of_Rooms, levels = 1:20), fill = factor(Postcode))) +
        geom_bar(position = "stack") + scale_fill_manual(name = "Postcode", values = custom_palette) +
        labs(title = "Stacked Bar Chart of Postcode by Number of Rooms", x = "Number of Rooms", y = "Count") +
        theme_minimal() + theme(text = element_text(size = 14), axis.text = element_text(size = 12, face = "bold"),
                                plot.title = element_text(hjust = 0.5, face = "bold"),
                                axis.title = element_text(face = "bold"))
      # Scatterplot of selected continuous variables
    } else if (are_both_continuous()) {
      ggplot(postcode_filter(), aes_string(x = input$x_var, y = input$y_var)) +
        geom_point(color = "tomato2") + labs(title = gsub("_", " ", paste("Scatterplot", input$y_var, "by", input$x_var)),
                                             x = gsub("_", " ", input$x_var), y = gsub("_", " ", input$y_var)) +
        scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma) + 
        theme_minimal() + theme(text = element_text(size = 14), axis.text = element_text(size = 12, face = "bold"),
                                plot.title = element_text(hjust = 0.5, face = "bold"), 
                                axis.title = element_text(face = "bold"))
      # Boxplot of x = categorical variable, y = continuous variable
    } else if (!is_x_continuous() && is_y_continuous()) {
      ggplot(postcode_filter(), aes_string(x = input$x_var, y = input$y_var)) +
        geom_boxplot(fill = "tomato2") + labs(title = gsub("_", " ", paste("Box Plot of", input$y_var, "by", input$x_var)),
                                              x = gsub("_", " ", input$x_var), y = gsub("_", " ", input$y_var)) +
        scale_y_continuous(labels = scales::comma) + theme_minimal() +
        theme(text = element_text(size = 14), axis.text = element_text(size = 12, face = "bold"),
              plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.title = element_text(face = "bold"))
      # Boxplot of x = continuous variable, y = categorical variable
    } else {
      ggplot(postcode_filter(), aes_string(x = input$x_var, y = input$y_var)) +
        geom_boxplot(fill = "tomato2") + labs(title = gsub("_", " ", paste("Box Plot of", input$y_var, "by", input$x_var)),
                                              x = gsub("_", " ", input$x_var), y = gsub("_", " ", input$y_var)) +
        scale_x_continuous(labels = scales::comma) + theme_minimal() +
        theme(text = element_text(size = 14), axis.text = element_text(size = 12, face = "bold"),
              plot.title = element_text(hjust = 0.5, face = "bold"), 
              axis.title = element_text(face = "bold"))
    }
  })
  
  # Output: Warning message for when both variables are the same
  output$warning_message <- renderText({
    if (are_variables_equal()) {
      "Please select different variables for X-axis and Y-axis."
    } else {
      ""
    }
  })
  
  # Output: Estimated house price message
  output$predicted_price_output <- renderText({
    house_details <- data.frame(
      Year = input$year_input,
      Postcode = input$postcode_input,
      Floor_Area = input$floor_area_input,
      Energy_Efficiency = input$energy_efficiency_input)
    
    predicted_price <- predict(glm_model, house_details, type = "response")
    
    paste("Estimated House Price:", comma(round(predicted_price, 2)), "GBP")
  })
  
  # Output: Estimated house price over time plot
  output$predicted_plot <- renderPlot({
    all_years <- c(min(property_prices$Year):max(property_prices$Year), seq(2020, 2030, by = 1))
    
    plot_data <- data.frame(
      Year = all_years,
      Postcode = rep(input$postcode_input, length(all_years)),
      Floor_Area = input$floor_area_input,
      Energy_Efficiency = input$energy_efficiency_input)
    
    plot_data$Predicted_Price <- predict(glm_model, plot_data, type = "response")
    
    ggplot(plot_data, aes(x = Year, y = Predicted_Price)) +
      geom_line(color = "tomato2") + labs(title = "Predicted House Prices Over Time", x = "Year", y = "Predicted Price") +
      scale_y_continuous(labels = scales::comma) + theme_minimal() +
      theme(text = element_text(size = 14), axis.text = element_text(size = 12, face = "bold"),
            plot.title = element_text(hjust = 0.5, face = "bold"), axis.title = element_text(face = "bold"))
  })
}

# Run the Shiny App
shinyApp(ui, server)