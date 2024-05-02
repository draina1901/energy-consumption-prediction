library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)
library(reshape2)
library(tidyr)
library(ggmap)
library(grid)
library(shinyjs)
library(rpart)



# Set your Google Maps API key
api_key <- "AIzaSyBhR5cUtbHi0S22QlCI4N_YbfiUSX7Y27A"
register_google(key = api_key)

# Read data from the CSV files
df <- read.csv("data/merged_city_weather.csv")
df2 <- read.csv("data/merged_city_weather+5.csv")
df3 <- read.csv("data/Insulation1_merged.csv")
df3merged <- read.csv("data/Insulation1_merged+5.csv")
df4 <- read.csv("data/grouped_data.csv")
map_data <- read.csv("data/grouped_data.csv")
grouped_data_date<-read.csv("data/grouped_data_date_pred.csv")
model3 <- rpart(Total_Energy_Consumption_Kwh ~ in.sqft + in.bedrooms + 
               in.building_america_climate_zone + in.refrigerator + in.lighting+
               in.misc_extra_refrigerator + in.county + in.income + 
               Average_Dry_Bulb_Temperature, data = grouped_data_date)

# Define your training data (replace this with your actual data)
train <- grouped_data_date

# UI

ui <- fluidPage(

  tags$h1("Energy Consumption Prediction App", style = "font-size: 25px; font-family: 'Consolas', monospace;color: #6A9B9F;"),
  tags$style(HTML("
    .nav-tabs > li > a {
      font-size: 16px;
      font-family: 'Consolas', monospace;
      color: #6A9B9F;
    font-weight: bold;
    }
  ")),
  theme = bs_theme(bootswatch = "sketchy"),
  
  # Create tabs
  tabsetPanel(
    tabPanel("Home",
             fluidPage(
               mainPanel(
                 tags$img(src = "index.png", 
                          height = "500px", width = "500px"),
                 tags$p(
                   "This app provides visualizations and predictions of energy consumption based on various factors.",
                   style = "font-size: 17px; font-family: 'Consolas', monospace; color:  #444444;" 
                 ),
                 tags$p("Navigate through the tabs to explore different aspects of energy consumption.", style = "font-size: 17px;font-family: 'Consolas', monospace; color: #444444;")
               )
             )
    ),
    
    tabPanel("Energy Predictor",
  sidebarLayout(
    sidebarPanel(
      # Add input controls for each predictor
      sliderInput("in_sqft", "Square Footage", min = min(train$in.sqft), max = max(train$in.sqft), value = mean(train$in.sqft)),
      sliderInput("in_bedrooms", "Number of bedrooms", min = round(min(train$in.bedrooms)), max = round(max(train$in.bedrooms)), value = round(mean(train$in.bedrooms))),
      selectInput("in_building_america_climate_zone", "Climate Zone", choices = unique(train$in.building_america_climate_zone), selected = (unique(train$in.building_america_climate_zone)[1])),
      sliderInput("Average_Dry_Bulb_Temperature", "Temperature", min = round(min(train$Average_Dry_Bulb_Temperature)), max = round(max(train$Average_Dry_Bulb_Temperature)), value = round(mean(train$Average_Dry_Bulb_Temperature))),
      selectInput("in_lighting", "Lighting Type", choices = (unique(train$in.lighting)), selected = (unique(train$in.lighting)[1])),
      selectInput("in_refrigerator", "Refrigerator Type", choices = (unique(train$in.refrigerator)), selected = (unique(train$in.refrigerator)[1])),
      selectInput("misc_extra_refrigerator", "Extra refrigerator", choices = (unique(train$in.misc_extra_refrigerator)), selected = (unique(train$in.misc_extra_refrigerator)[1])),
      selectInput("in_county", "County", choices = (unique(train$in.county)), selected = (unique(train$in.county)[1])),
      selectInput("in_income_level", "Income Level", choices = (unique(train$in.income)), selected = (unique(train$in.income)[1])),
      
     # We have selected these variables for presentation in the app, and the prediction assumes that all remaining variables remain constant.
      
      # Add more controls as needed
      actionButton("predictButton", "Predict")
    ),
    mainPanel(
      # Display the predicted total usage
      h4("Predicted Total Energy Usage:"),
      textOutput("prediction"),
      
      # Display the prediction plot
      plotOutput("predictionPlot"),
      column(10,offset = 0, align = "top",verbatimTextOutput("insightsTextpred"))
    ))),
    
    
    tabPanel("Predicted Energy Comparison(City-Wise)",
             
             sidebarLayout(
               sidebarPanel(
                 # Dropdown menu for selecting cities
                 selectInput("city", "Select City", unique(df$V1))
                 
               ),
               mainPanel(
                 # Plot output
                 plotOutput("linePlot"),
                 column(10,offset = 0, align = "top",verbatimTextOutput("insightsText3"))
               )
             )
    ),
    
    
    tabPanel("Predicted Energy Comparison(InsulationType-Wise)", 
             sidebarLayout(
               sidebarPanel(
                 # Dropdown menu for selecting insulation type
                 selectInput("Insulation_Type", "Select Insulation Type", unique(df3$Insulation_Type))
               ),
               mainPanel(
                 # Plot output
                 plotOutput("insulationPlot"),
                 column(8,offset = 0, align = "top",verbatimTextOutput("insightsText1"))
               )
             )
    ),
    tabPanel("Energy Usage Bar Chart", 
             sidebarLayout(
               sidebarPanel(
                 # Dropdown menu for selecting Category
                 selectInput("x_axis", "Select Category", choices = c("Lighting" = "in.lighting", 
                                                                      "Climate Zone" = "in.building_america_climate_zone")),
               ),
               mainPanel(
                 # Output for the bar graph
                 plotOutput("energy_plot"),
                 column(11,offset = 0, align = "top",verbatimTextOutput("insightsBar")),
                 column(11,offset = 0, align = "top",verbatimTextOutput("insightsBar1"))
               )
             )
    ),
    tabPanel("Energy Usage ScatterPlot", 
             sidebarLayout(
               sidebarPanel(
                 # Dropdown menu for selecting Category
                 selectInput("scatter_category", "Select Category for Scatterplot", choices = c("Occupants"="in.occupants","Bedrooms"= "in.bedrooms"))
               ),
               
               mainPanel(
                 # Output for the scatterplot
                 plotOutput("scatter_plot"),
                 column(11,offset = 0, align = "top",verbatimTextOutput("insights_SP1")),
                 column(11,offset = 0, align = "top",verbatimTextOutput("insights_SP"))
               )
             )
    ),
    tabPanel("Energy Usage Map", 
             mainPanel(
               plotOutput("energyMap"),
               column(12,offset = 0, align = "top",verbatimTextOutput("insightsmap"))
             )
    )
  )
)


# Server
server <- function(input, output) {
  # Reactive function to filter data based on city selection
  filtered_data <- reactive({
    if (input$city == "All") {
      rowSums(df[, -1])  # Sum across all cities
    } else {
      df %>%
        filter(V1 == input$city) %>%
        select(-V1) %>%
        as.numeric()  # Ensure numeric type
    }
  })
  
  # Function to run the model and make predictions
  predict_energy <- eventReactive(input$predictButton, {
    # Create a new data frame with user inputs
    newdata <- data.frame(
      in.sqft = input$in_sqft,
      in.bedrooms = input$in_bedrooms,
      in.building_america_climate_zone = input$in_building_america_climate_zone,
      Average_Dry_Bulb_Temperature = input$Average_Dry_Bulb_Temperature,
      in.lighting = input$in_lighting,
      in.refrigerator = input$in_refrigerator,
      in.misc_extra_refrigerator = input$misc_extra_refrigerator,
      in.county = input$in_county,
      in.income = input$in_income_level
      # Add more variables as needed
    )
    
    # Make predictions
    predictions <- predict(model3, newdata)
    return(predictions)
  })
  
  # Reactive function for df2 data
  filtered_data2 <- reactive({
    if (input$city == "All") {
      rowSums(df2[, -1])  # Sum across all cities
    } else {
      df2 %>%
        filter(V1 == input$city) %>%
        select(-V1) %>%
        as.numeric()  # Ensure numeric type
    }
  })
  
  # Reactive function for df3 data
  filtered_data3 <- reactive({
    if (input$Insulation_Type == "All") {
      rowSums(df3[, -1])  # Sum across all Insulation
    } else {
      df3 %>%
        filter(Insulation_Type == input$Insulation_Type) %>%
        select(-Insulation_Type) %>%
        as.numeric()  # Ensure numeric type
    }
  })
  
  # Reactive function for df3merged data
  filtered_data5 <- reactive({
    if (input$Insulation_Type == "All") {
      rowSums(df3merged[, -1])  # Sum across all Insulation
    } else {
      df3merged %>%
        filter(Insulation_Type == input$Insulation_Type) %>%
        select(-Insulation_Type) %>%
        as.numeric()  # Ensure numeric type
    }
  })
  
  # Reactive function for df4 data
  filtered_data4 <- reactive({
    df4
  })
  
  
  # Output the predicted total usage
  output$prediction <- renderText({
    paste("Predicted Total Energy Usage: ", round(predict_energy(), 2))
  })
  
  output$insightsTextpred <- renderText({
    paste0(
      "Note: We have selected these variables for presentation in the app, and the prediction assumes ", "\n",
      "that all remaining variables remain constant.", "\n"
    )
  })
  
  # Plotting the line graph
  output$linePlot <- renderPlot({
    # Plotting df1
    plot(filtered_data(), type = "l", col = "blue", lwd = 2,
         main = "Hourly Energy Consumption Prediction",
         xlab = "Hour", ylab = "Predicted Energy Consumption",
         ylim = c(min(c(filtered_data(), filtered_data2())),
                  max(c(filtered_data(), filtered_data2()))))
    
    # Adding df2 to the same plot
    lines(filtered_data2(), col = "red", lwd = 2)
    
    # Adding legend
    legend("topright", legend = c("Predicted Consumption with temp 5 degree warmer", "Predicted Consumption"), col = c("blue", "red"), lty = 1:1)
  })
  
  
  output$insightsText3 <- renderText({
    paste0(
      "Energy consumption peak is seen between 10am to 3pm acorss all the cities.", "\n",
      "Energy consumption is same between 1am to 5 am regardless of an increase in 5 degree temperature", "\n"
    )
  })
  
  
  
  
  
  # Plotting the line graph for climate zone-wise energy
  output$climateZonePlot <- renderPlot({
    # Plotting df4 - Climate Zone
    plot(filtered_data4_zone(), type = "l", col = "purple", lwd = 2,
         main = "Predicted Hourly Energy Consumption by Climate Zone",
         xlab = "Hour", ylab = "Predicted Energy Consumption")
  })
  
  
  
  # Plotting the line graph
  output$insulationPlot <- renderPlot({
    # Plotting df3
    plot(filtered_data3(), type = "l", col = "blue", lwd = 2,
         main = "Hourly Energy Consumption Prediction",
         xlab = "Hour", ylab = "Predicted Energy Consumption",
         ylim = c(min(c(filtered_data3(), filtered_data5())),
                  max(c(filtered_data3(), filtered_data5()))))
    
    # Adding df2 to the same plot
    lines(filtered_data5(), col = "red", lwd = 2)
    
    # Adding legend
    legend("topright", legend = c("Predicted Consumption","Predicted Consumption with temp 5 degree warmer"), col = c("blue", "red"), lty = 1:1)
  })
  
  
  output$insightsText1 <- renderText({
    
    paste0(
      "Energy consumption peak is seen between 3pm to 8pm for all insulation type.", "\n",
      "Energy consumption is high for Insulation Type Brick,12-in", "\n"
    )
  })
  
  
  
  output$energy_plot <- renderPlot({
    ggplot(filtered_data4(), aes_string(x = input$x_axis, y = "Total_Energy_Consumption_Kwh", fill = input$x_axis)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Actual Energy Consumption by Climate Zone and Lighting",
           x = input$x_axis,
           y = "Actual Energy Consumption (KWh)")
  })
  
  output$insightsBar <- renderText({
    paste0(
      "The Lighting bar chart indicates that the actual energy consumption in kWh is highest with 100% Incandescent,","\n",
      "followed by 100% LED lighting, and is lowest with 100% CFL lighting.","\n"
      
    )
  })
  
  output$insightsBar1 <- renderText({
    paste0(
      "The Climate Bar chart depicts energy consumption in kilowatt-hours (kWh) for two climate zones labeled as Hot-Humid & Mixed-Humid.","\n",
      "The Mixed-Humid zone shows significantly higher energy consumption compared to the Hot-Humid zone.", "\n"
      
    )
  })
  
  
  output$scatter_plot <- renderPlot({
    data <- filtered_data4()
    
    if (input$scatter_category == "in.occupants") {
      ggplot(data, aes_string(x = ifelse(input$scatter_category == "10+", "10+", input$scatter_category), y = "Total_Energy_Consumption_Kwh")) +
        geom_point() +
        labs(title = paste("Scatterplot of Actual Energy Consumption by", input$scatter_category),
             x = input$scatter_category, y = "Actual Energy Consumption (KWh)") +
        scale_x_discrete(limits = c(1:9, "10+"))
    } else {
      ggplot(data, aes_string(x = input$scatter_category, y = "Total_Energy_Consumption_Kwh")) +
        geom_point() +
        labs(title = paste("Scatterplot of Actual Energy Consumption by", input$scatter_category),
             x = input$scatter_category, y = "Actual Energy Consumption (KWh)")
      # You don't need to set x-axis limits for other categories
    }
  })
  
  
  
  
  
  
  
  
  
  output$insights_SP <- renderText({
    paste0(
      "The Bedroom scatterplot shows that energy consumption tends to increase with the number of bedrooms,","\n",
      "with the highest variability in consumption seen in homes with five bedrooms.","\n"
      
    )
  })
  
  output$insights_SP1 <- renderText({
    paste0(
      "The Occupants Scatterplot suggests that occupants with 3,4,6 in number consume energy slightly higher than others","\n"
      
    )
  })
  
  
  
  # Extract only rows with the maximum energy consumption for each building
  max_energy_df <- map_data %>%
    group_by(bldg_id) %>%
    filter(Total_Energy_Consumption_Kwh == max(Total_Energy_Consumption_Kwh)) %>%
    ungroup()
  
  # Define breaks and labels for energy consumption categories
  breaks <- c(25, 50, 100, 150, 200, Inf)
  labels <- c("25-50", "51-100", "101-150", "151-200", "200+")
  
  # Create energy consumption categories
  energy_categories <- cut(max_energy_df$Total_Energy_Consumption_Kwh, breaks = breaks, labels = labels)
  
  # Get the base map
  base_map <- get_map(location = c(lon = mean(max_energy_df$in.weather_file_longitude), lat = mean(max_energy_df$in.weather_file_latitude)),
                      zoom = 7, maptype = "terrain", scale = 1)
  
  
  # Render the map
  output$energyMap <- renderPlot({
    ggmap(base_map) +
      geom_point(data = max_energy_df, aes(x = in.weather_file_longitude, y = in.weather_file_latitude, color = energy_categories, size = Total_Energy_Consumption_Kwh),
                 show.legend = TRUE) +
      scale_color_manual(values = c("25-50" = "green", "51-100" = "yellow", "101-150" = "orange", "151-200" = "red", "200+" = "darkred"),
                         guide = "legend", name = "Energy Usage Category") +
      labs(title = "Building Locations and Energy Usage") +
      scale_size_continuous(name = "Energy Consumption (KWh)")
  })
  
  output$insightsmap <- renderText({
    paste0(
      "The map depicts building energy usage in a specific region, with the Charleston & Summerville area showing a cluster of buildings with","\n",
      "higher energy consumption (in the 200+ kWh range).While the surrounding areas like FayetVille, RockHill generally show lower energy usage,","\n",
      "with many buildings consuming less than 50 kWh.Areas like Charlotte,Myrtle Beach and columbia have a mix of energy consumption levels,","\n",
      "indicating a varied range of building efficiencies or uses.","\n"
      
    )
    
    
  })
  
}


# Run the Shiny app
shinyApp(ui, server)
