---
title: "GIS"
author: "Xiuchen Lu"
date: "2023-06-07"
output: html_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(viridis)
library(readxl)
library(tidyverse)
library(lubridate)
library(readr)
library(googlesheets4)
library(data.table)
library(shiny)
library(RColorBrewer)
library(colorspace)
library(dplyr)
library(gstat)
```

```{r}
# Set working directory to the Downloads folder
setwd("/Users/luxiuchen/Downloads")

# Read excel file into R
### read_excel but the excel is not correct
santa_rita <- read_excel("Yearly Utilization by XL.xlsx", sheet = "Utilization")

# Define the variables of interest from the Santa Rita dataset
santa_rita_GIS <- c("Pasture", "Transect", "Transect_Name", 
                    "UTM start X", "UTM start Y", "UTM end X", "UTM end Y")

# Extract the specified variables
santa_rita_data <- santa_rita[, santa_rita_GIS]

# Remove any rows with missing data
santa_rita_data_clean <- na.omit(santa_rita_data)

# Convert the cleaned data to a simple features object with UTM coordinates
santa_rita_data_plot <- st_as_sf(santa_rita_data_clean, 
                                 coords = c("UTM start X", "UTM start Y"), 
                                 crs = "+proj=utm +zone=12 +datum=WGS84")

# Plot the data as blue points
plot(santa_rita_data_plot, col = "blue", pch = 16, cex = 2)

# Change Transect_Name to a factor variable
santa_rita_data$Transect_Name <- as.factor(santa_rita_data$Transect_Name)

# Plot Santa Rita geometry graph
ggplot() +
  geom_sf(data = santa_rita_data_plot, aes(fill = Transect_Name)) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(fill = "Transect Name") +
  theme(legend.position = "none")

# Save the plot
ggsave("santa_rita_plot.png", width = 20, height = 8)

# Read shp file into R
santa_rita_1 <- read_sf("/Users/luxiuchen/Desktop/project research data/UpdatedSRER_04_27_2023.shp")

# Plot basic Santa Rita file
ggplot() +
  geom_sf(data = santa_rita_1) +
  theme_minimal()

# Change character to factor
santa_rita_1$PastureNam <- as.factor(santa_rita_1$PastureNam)

# Check the column names of Santa Rita data
col_names <- names(santa_rita)
print(col_names)

# Check the CRS of both datasets
print(st_crs(santa_rita_1))
print(st_crs(santa_rita_data_plot))

# Transform the CRS of Santa Rita data to match Santa Rita 1
santa_rita_data_plot <- st_transform(santa_rita_data_plot, st_crs(santa_rita_1))

# Join Santa Rita data to Santa Rita 1
result <- st_join(santa_rita_1, santa_rita_data_plot)
print(head(result))
# Save the plot
ggsave("santa_rita_plot.png", width = 20, height = 8)

# Check the column names of Santa Rita data
col_names <- names(santa_rita)
print(col_names)

# Check the CRS of both datasets
print(st_crs(santa_rita_1))
print(st_crs(santa_rita_data_plot))

# Transform the CRS of Santa Rita data to match Santa Rita 1
santa_rita_data_plot <- st_transform(santa_rita_data_plot, st_crs(santa_rita_1))

# Join Santa Rita data to Santa Rita 1
result <- st_join(santa_rita_1, (head(result)))
print(head(result))

# Plot the result with Transect Name
ggplot() +
  geom_sf(data = result, aes(fill = Transect_Name)) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(fill = "Transect Name") +
  theme(legend.position = "none") +
  geom_sf(data = santa_rita_data_plot)
```

```{r}
# Reading the data from a Google Sheet
santa_rita <- read_sheet("https://docs.google.com/spreadsheets/d/13UBBNXL4JfQbFNUmtU0sdcR-s1p4nayElb7o1Emh6oQ/edit?usp=sharing")

# Ensure "% Use 2020" column is numeric
santa_rita$`% Use 2020` <- as.numeric(santa_rita$`% Use 2020`)

# Create a sequence of years
years <- 2010:2023

# Loop through each year to convert the respective columns to numeric
for (year in years) {
  column_name <- paste0("% Use ", year)
  santa_rita[[column_name]] <- as.numeric(santa_rita[[column_name]])
}

# Joining the santa_rita_1 and santa_rita dataframes, reshaping the data, and calculating the average use by year
test_srer <- santa_rita_1 %>%
  left_join(santa_rita,by = c("PastureNam"="Pasture")) %>%
  pivot_longer(cols = starts_with("% Use"),
               names_to = "Year",
               values_to = "% Use") %>%
  group_by(PastureNam, geometry, Year) %>%
  summarize(Average = mean(`% Use`, na.rm = TRUE), .groups="drop")

# Create a plot with a continuous color gradient
ggplot() +
  geom_sf(data = test_srer, aes(fill = Average)) +
  theme_minimal() +
  scale_fill_gradient(low = "blue", high = "red") + 
  labs(fill = "T") +
  theme(legend.position = "none")
```

```{r}
# Shiny UI with a selection input for year and a plot output
ui <- fluidPage(
  titlePanel("% Use by Year"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectYear", 
                  label = "Choose a Year", 
                  choices = unique(test_srer$Year),
                  selected = unique(test_srer$Year)[1]
      )),
    mainPanel(
      plotOutput("mapPlot")  # Add this line to create a spot for your map
    )
  )
)


# Shiny server function that creates a map plot based on the selected year
server <- function(input, output, session) {
  
  output$mapPlot <- renderPlot({
    one_year <- test_srer %>% 
      filter(Year == input$selectYear)
    ggplot() +
      geom_sf(data = one_year, aes(fill = Average)) +
      theme_minimal() +
      scale_fill_gradient(low = "blue", high = "red") +  
      labs(fill = "T") +
      theme(legend.position = "none")
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
```

```{r}
str(test_srer$`% Use`)
```
```{r}
class(test_srer$Year)
test_srer <- test_srer[!is.na(test_srer$Year), ]
```



```{r}
## If we need input function, We can use codes from these
# Shiny UI
ui <- fluidPage(
  titlePanel("Average % Use over Selected Years"),
  sidebarLayout(
    sidebarPanel(
      numericInput("startYear", 
                   label = "Enter your starting year", 
                   value = min(as.numeric(unique(test_srer$Year))),
                   min = min(as.numeric(unique(test_srer$Year))),
                   max = max(as.numeric(unique(test_srer$Year)))
      ),
      numericInput("endYear", 
                   label = "Enter your ending year", 
                   value = min(as.numeric(unique(test_srer$Year)))+2,
                   min = min(as.numeric(unique(test_srer$Year))),
                   max = max(as.numeric(unique(test_srer$Year)))
      )
    ),
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

# Shiny server function
server <- function(input, output, session) {
  
  selected_years_data <- reactive({
    selected_years <- test_srer %>%
      filter(Year >= input$startYear & Year <= input$endYear)
    
    selected_years %>%
      group_by(PastureNam, geometry, Year) %>%
      summarise_at(vars(`% Use`), list(mean = mean, max = max, sd = sd), na.rm = TRUE)
  })
  
  output$mapPlot <- renderPlot({
    data_for_plot <- selected_years_data()
    ggplot() +
      geom_sf(data = data_for_plot, aes(fill = `% Use_mean`)) +
      theme_minimal() +
      scale_fill_gradient(low = "blue", high = "red") +  
      labs(fill = "T") +
      theme(legend.position = "none")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
```



```{r}
### Same as the last function we have 
# Shiny UI
ui <- fluidPage(
  titlePanel("Average % Use over Selected Years"),
  sidebarLayout(
    sidebarPanel(
      numericInput("startYear", 
                   label = "Enter your starting year", 
                   value = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                   min = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                   max = max(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))
      ),
      numericInput("endYear", 
                   label = "Enter your ending year", 
                   value = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))+2,
                   min = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                   max = max(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))
      ),
      actionButton("goButton", "Go"),
      selectInput("selectYear", 
                  label = "Choose a Year for Map Display", 
                  choices = unique(test_srer$Year),
                  selected = unique(test_srer$Year)[1]
      )
    ),
    mainPanel(
      tableOutput("displayData"),
      textOutput("avgUse"),
      plotOutput("mapPlot")
    )
  )
)

# Shiny server function
server <- function(input, output, session) {
  
  test_srer$Year <- as.numeric(gsub("^% Use ", "", test_srer$Year))
  
  selected_years_data <- eventReactive(input$goButton, {
    if (input$startYear > input$endYear) {
      return(NULL)
    }
    
    selected_years <- test_srer %>%
      filter(Year >= input$startYear & Year <= input$endYear)
    
    selected_years %>%
      group_by(PastureNam, Year) %>%
      summarise_at(vars(Average), list(mean = mean, max = max, sd = sd), na.rm = TRUE)
  })
  
  output$displayData <- renderTable({
    selected_years_data()
  })
  
  output$avgUse <- renderText({
    data_for_avg <- selected_years_data()
    avg_use <- mean(data_for_avg$Average_mean, na.rm = TRUE)
    paste("Average % use from", input$startYear, "to", input$endYear, "is", round(avg_use, 2))
  })
  
  output$mapPlot <- renderPlot({
    one_year <- test_srer %>% 
      filter(Year == as.numeric(gsub("^% Use ", "", input$selectYear)))
    three_year <- test_srer %>% 
      filter(Year == as.numeric(gsub("^% Use ", "", input$selectYear)))
    ggplot() +
      geom_sf(data = one_year, aes(fill = Average)) +
      theme_minimal() +
      scale_fill_gradient(low = "blue", high = "red") +  
      labs(fill = "T") +
      theme(legend.position = "none")
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

```


```{r}
### Step 2
# Shiny UI
ui <- fluidPage(
  titlePanel("Average % Use over Selected Years"),
  sidebarLayout(
    sidebarPanel(
      numericInput("selectYear", 
                   label = "Enter a Year", 
                   value = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                   min = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                   max = max(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))
      )
    ),
    mainPanel(
      plotOutput("mapPlot"),
      plotOutput("avgMapPlot")
    )
  )
)

# Shiny server function
server <- function(input, output, session) {
  
  test_srer$Year <- as.numeric(gsub("^% Use ", "", test_srer$Year))
  
  # Reactively get the selected year's data
  selected_year_data <- reactive({
    test_srer %>% 
      filter(Year == input$selectYear)
  })
  
  # Reactively get the average data of past three years
  avg_three_years_data <- reactive({
    test_srer %>% 
      filter(Year >= (input$selectYear - 2) & Year <= input$selectYear) %>%
      group_by(PastureNam) %>%
      summarise(Average = mean(Average, na.rm = TRUE))
  })
  
  # Display the map for the selected year
  output$mapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = selected_year_data(), aes(fill = Average)) +
      labs(title = paste("Map for Year", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_gradient(low = "blue", high = "red") +
      theme(legend.position = "none")
  })
  
  # Display the map for the average of past three years
  output$avgMapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = avg_three_years_data(), aes(fill = Average)) +
      labs(title = paste("Average Map for Past Three Years from", input$selectYear - 2, "to", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_gradient(low = "blue", high = "red") +
      theme(legend.position = "none")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
```


```{r}
### Now we use 
# Shiny UI
ui <- fluidPage(
  titlePanel("Average % Use over Selected Years"),
  fluidRow(
    column(12, 
           numericInput("selectYear", 
                        label = "Enter a Year", 
                        value = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        min = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        max = max(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))
           )
    )
  ),
  fluidRow(
    column(6, plotOutput("mapPlot")),
    column(6, plotOutput("avgMapPlot"))
  )
)

# Rest of the code remains same

# Shiny server function
server <- function(input, output, session) {
  
  test_srer$Year <- as.numeric(gsub("^% Use ", "", test_srer$Year))
  
  # Reactively get the selected year's data
  selected_year_data <- reactive({
    test_srer %>% 
      filter(Year == input$selectYear)
  })
  
  # Prepare the label data for the selected year
  label_data_year <- reactive({
    selected_year_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Reactively get the average data of past three years
  avg_three_years_data <- reactive({
    test_srer %>% 
      filter(Year >= (input$selectYear - 2) & Year <= input$selectYear) %>%
      group_by(PastureNam) %>%
      summarise(Average = mean(Average, na.rm = TRUE))
  })
  
  # Prepare the label data for the average of past three years
  label_data_avg <- reactive({
    avg_three_years_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Display the map for the selected year
  output$mapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = selected_year_data(), aes(fill = Average)) +
      geom_text(data = label_data_year(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Map for Year", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_gradient(low = "blue", high = "red") +
      theme(legend.position = "none")
  })
  
  # Display the map for the average of past three years
  output$avgMapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = avg_three_years_data(), aes(fill = Average)) +
      geom_text(data = label_data_avg(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Average Map for Past Three Years from", input$selectYear - 2, "to", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_gradient(low = "blue", high = "red") +
      theme(legend.position = "none")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
```

```{r}
# Shiny UI
ui <- fluidPage(
  titlePanel("Average % Use over Selected Years"),
  fluidRow(
    column(12, 
           numericInput("selectYear", 
                        label = "Enter a Year", 
                        value = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        min = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        max = max(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))
           )
    )
  ),
  fluidRow(
    column(6, plotOutput("mapPlot")),
    column(6, plotOutput("avgMapPlot"))
  )
)

# Shiny server function
server <- function(input, output, session) {
  
  test_srer$Year <- as.numeric(gsub("^% Use ", "", test_srer$Year))
  
  # Reactively get the selected year's data
  selected_year_data <- reactive({
    test_srer %>% 
      filter(Year == input$selectYear)
  })
  
  # Prepare the label data for the selected year
  label_data_year <- reactive({
    selected_year_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Reactively get the average data of past three years
  avg_three_years_data <- reactive({
    test_srer %>% 
      filter(Year >= (input$selectYear - 2) & Year <= input$selectYear) %>%
      group_by(PastureNam) %>%
      summarise(Average = mean(Average, na.rm = TRUE))
  })
  
  # Prepare the label data for the average of past three years
  label_data_avg <- reactive({
    avg_three_years_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Color breaks and labels
  color_breaks <- c(-Inf, 5, 20, 40, 60, Inf)
  color_labels <- c("0-5", "5-20", "20-40", "40-60", "60+")
  
  # Display the map for the selected year
  output$mapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = selected_year_data(), aes(fill = cut(Average, breaks = color_breaks))) +
      geom_text(data = label_data_year(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Map for Year", input$selectYear), fill = "Average") +
      theme_minimal() +
      scale_fill_brewer(palette = "RdYlBu", breaks = color_labels, na.value = "white", guide = "legend") +
      theme(legend.position = "bottom")
  })
  
  # Display the map for the average of past three years
  output$avgMapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = avg_three_years_data(), aes(fill = cut(Average, breaks = color_breaks))) +
      geom_text(data = label_data_avg(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Average Map for Past Three Years from", input$selectYear - 2, "to", input$selectYear), fill = "Average") +
      theme_minimal() +
      scale_fill_brewer(palette = "RdYlBu", breaks = color_labels, na.value = "white", guide = "legend") +
      theme(legend.position = "bottom")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
```

```{r}
library(shiny)
library(sf)
library(dplyr)
library(ggplot2)

# Define the color labels
color_labels <- c('0-5', '5-20', '20-40', '40-60', '60+')

# Define the color palette
color_palette <- c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15')

# Shiny UI
ui <- fluidPage(
  titlePanel("Average % Use over Selected Years"),
  fluidRow(
    column(12, 
           numericInput("selectYear", 
                        label = "Enter a Year", 
                        value = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        min = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        max = max(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))
           )
    )
  ),
  fluidRow(
    column(6, plotOutput("mapPlot")),
    column(6, plotOutput("avgMapPlot"))
  )
)

# Shiny server function
server <- function(input, output, session) {
  
  test_srer$Year <- as.numeric(gsub("^% Use ", "", test_srer$Year))
  
  # Reactively get the selected year's data
  selected_year_data <- reactive({
    test_srer %>% 
      filter(Year == input$selectYear) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
  })
  
  # Prepare the label data for the selected year
  label_data_year <- reactive({
    selected_year_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Reactively get the average data of past three years
  avg_three_years_data <- reactive({
    test_srer %>% 
      filter(Year >= (input$selectYear - 2) & Year <= input$selectYear) %>%
      group_by(PastureNam) %>%
      summarise(Average = mean(Average, na.rm = TRUE)) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
  })
  
  # Prepare the label data for the average of past three years
  label_data_avg <- reactive({
    avg_three_years_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Display the map for the selected year
  output$mapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = selected_year_data(), aes(fill = Average_Bins)) +
      geom_text(data = label_data_year(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Map for Year", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
  
  # Display the map for the average of past three years
  output$avgMapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = avg_three_years_data(), aes(fill = Average_Bins)) +
      geom_text(data = label_data_avg(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Average Map for Past Three Years from", input$selectYear - 2, "to", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
```

```{r}

# 定义颜色标签
color_labels <- c('0-5', '5-20', '20-40', '40-60', '60+')

# 定义颜色调色板
color_palette <- c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15')

# Shiny UI
ui <- fluidPage(
  titlePanel("Average % Use over Selected Years"),
  fluidRow(
    column(12, 
           numericInput("selectYear", 
                        label = "Enter a Year", 
                        value = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        min = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        max = max(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))
           )
    )
  ),
  fluidRow(
    column(6, plotOutput("mapPlot")),
    column(6, plotOutput("avgMapPlot"))
  )
)

# Shiny server function
server <- function(input, output, session) {
  
  test_srer$Year <- as.numeric(gsub("^% Use ", "", test_srer$Year))
  
  # Reactively get the selected year's data
  selected_year_data <- reactive({
    data <- test_srer %>% 
      filter(Year == input$selectYear) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
    
    # Convert to SpatialPointsDataFrame
    coordinates(data) <- ~long+lat  # Change to your actual column names
    proj4string(data) <- CRS("+init=epsg:4326")  # Use appropriate EPSG code
    
    # Create an empty grid
    grd <- spsample(data, n = 50000, type = "regular")
    gridded(grd) <- TRUE
    
    # Interpolate
    data.idw <- gstat::idw(formula = Average ~ 1, locations = data, newdata = grd)
    
    # Convert back to sf for ggplot
    data <- st_as_sf(as(data.idw, "SpatialPixelsDataFrame"))
    
    data
  })
  
  # Prepare the label data for the selected year
  label_data_year <- reactive({
    selected_year_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Reactively get the average data of past three years
  avg_three_years_data <- reactive({
    test_srer %>% 
      filter(Year >= (input$selectYear - 2) & Year <= input$selectYear) %>%
      group_by(PastureNam) %>%
      summarise(Average = mean(Average, na.rm = TRUE)) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
  })
  
  # Prepare the label data for the average of past three years
  label_data_avg <- reactive({
    avg_three_years_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Display the map for the selected year
  output$mapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = selected_year_data(), aes(fill = Average_Bins)) +
      geom_text(data = label_data_year(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Map for Year", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
  
  # Display the map for the average of past three years
  output$avgMapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = avg_three_years_data(), aes(fill = Average_Bins)) +
      geom_text(data = label_data_avg(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Average Map for Past Three Years from", input$selectYear - 2, "to", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
}

# 运行Shiny应用程序
shinyApp(ui = ui, server = server)

```
```{r}
library(shiny)
library(sf)
library(dplyr)
library(ggplot2)

# Define the color labels
color_labels <- c('0-5', '5-20', '20-40', '40-60', '60+')

# Define the color palette
color_palette <- c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15')

# Shiny UI
ui <- fluidPage(
  titlePanel("Average % Use over Selected Years"),
  fluidRow(
    column(12, 
           numericInput("selectYear", 
                        label = "Enter a Year", 
                        value = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        min = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        max = max(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))
           ),
           selectInput("selectPasture", 
                       label = "PastureNam select", 
                       choices = unique(test_srer$PastureNam))
    )
  ),
  fluidRow(
    column(6, plotOutput("mapPlot")),
    column(6, plotOutput("avgMapPlot")),
    column(12, plotOutput("pasturePlot"))
  )
)

# Shiny server function
server <- function(input, output, session) {
  
  test_srer$Year <- as.numeric(gsub("^% Use ", "", test_srer$Year))
  
  # Reactively get the selected year's data
  selected_year_data <- reactive({
    test_srer %>% 
      filter(Year == input$selectYear) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
  })
  
  # Prepare the label data for the selected year
  label_data_year <- reactive({
    selected_year_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Reactively get the average data of past three years
  avg_three_years_data <- reactive({
    test_srer %>% 
      filter(Year >= (input$selectYear - 2) & Year <= input$selectYear) %>%
      group_by(PastureNam) %>%
      summarise(Average = mean(Average, na.rm = TRUE)) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
  })
  
  # Prepare the label data for the average of past three years
  label_data_avg <- reactive({
    avg_three_years_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Reactively get the selected pasture's data
  selected_pasture_data <- reactive({
    test_srer %>% 
      filter(Year == input$selectYear, PastureNam == input$selectPasture) %>% 
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
  })
  
  # Prepare the label data for the selected pasture
  label_data_pasture <- reactive({
    selected_pasture_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Display the map for the selected year
  output$mapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = selected_year_data(), aes(fill = Average_Bins)) +
      geom_text(data = label_data_year(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Map for Year", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
  
  # Display the map for the average of past three years
  output$avgMapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = avg_three_years_data(), aes(fill = Average_Bins)) +
      geom_text(data = label_data_avg(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Average Map for Past Three Years from", input$selectYear - 2, "to", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
  
  # Display the map for the selected pasture
  output$pasturePlot <- renderPlot({
    ggplot() +
      geom_sf(data = selected_pasture_data(), aes(fill = Average_Bins)) +
      geom_text(data = label_data_pasture(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Map for Pasture", input$selectPasture, "in Year", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)

```

```{r}
library(shiny)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# Define the color labels
color_labels <- c('0-5', '5-20', '20-40', '40-60', '60+')

# Define the color palette
color_palette <- c('#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15')

# Shiny UI
ui <- fluidPage(
  titlePanel("Average % Use over Selected Years"),
  fluidRow(
    column(12, 
           numericInput("selectYear", 
                        label = "Enter a Year", 
                        value = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        min = min(as.numeric(gsub("^% Use ", "", unique(test_srer$Year)))),
                        max = max(as.numeric(gsub("^% Use ", "", unique(test_srer$Year))))
           ),
           selectInput("selectPasture", 
                       label = "PastureNam select", 
                       choices = unique(test_srer$PastureNam))
    )
  ),
  fluidRow(
    column(6, plotOutput("mapPlot")),
    column(6, plotOutput("avgMapPlot")),
    column(12, plotOutput("pasturePlot"))
  )
)

# Shiny server function
server <- function(input, output, session) {
  
  test_srer$Year <- as.numeric(gsub("^% Use ", "", test_srer$Year))
  
  # Reactively get the selected year's data
  selected_year_data <- reactive({
    test_srer %>% 
      filter(Year == input$selectYear) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
  })
  
  # Prepare the label data for the selected year
  label_data_year <- reactive({
    selected_year_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Reactively get the average data of past three years
  avg_three_years_data <- reactive({
    test_srer %>% 
      filter(Year >= (input$selectYear - 2) & Year <= input$selectYear) %>%
      group_by(PastureNam) %>%
      summarise(Average = mean(Average, na.rm = TRUE)) %>%
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
  })
  
  # Prepare the label data for the average of past three years
  label_data_avg <- reactive({
    avg_three_years_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Reactively get the selected pasture's data
  selected_pasture_data <- reactive({
    test_srer %>% 
      filter(Year == input$selectYear, PastureNam == input$selectPasture) %>% 
      mutate(Average_Bins = cut(Average, breaks = c(-Inf, 5, 20, 40, 60, Inf), labels = color_labels))
  })
  
  # Prepare the label data for the selected pasture
  label_data_pasture <- reactive({
    selected_pasture_data() %>% 
      st_centroid() %>% 
      st_as_sf() %>% 
      cbind(st_coordinates(.), Average = .$Average)
  })
  
  # Display the map for the selected year
  output$mapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = selected_year_data(), aes(fill = Average_Bins)) +
      geom_sf(data = santa_rita_data_plot) +
      geom_text(data = label_data_year(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Map for Year", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
  
  # Display the map for the average of past three years
  output$avgMapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = avg_three_years_data(), aes(fill = Average_Bins)) +
      geom_sf(data = santa_rita_data_plot) +
      geom_text(data = label_data_avg(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Average Map for Past Three Years from", input$selectYear - 2, "to", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
  
  # Display the map for the selected pasture
  output$pasturePlot <- renderPlot({
    ggplot() +
      geom_sf(data = selected_pasture_data(), aes(fill = Average_Bins)) +
      geom_sf(data = santa_rita_data_plot) +
      geom_text(data = label_data_pasture(), aes(x = X, y = Y, label = round(Average, 1)), size = 3, check_overlap = TRUE) +
      labs(title = paste("Map for Pasture", input$selectPasture, "in Year", input$selectYear), fill = "T") +
      theme_minimal() +
      scale_fill_manual(values = color_palette, drop = FALSE) +
      guides(fill = guide_legend(title = "Average % Use"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
```

