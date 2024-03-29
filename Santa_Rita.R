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
santa_rita <- read_sheet("https://docs.google.com/spreadsheets/d/13UBBNXL4JfQbFNUmtU0sdcR-s1p4nayElb7o1Emh6oQ/edit?usp=sharing")
santa_rita$`% Use 2020` <- as.numeric(santa_rita$`% Use 2020`)  # ensure %Use 2020 is numeric

test_srer <- santa_rita_1 %>% left_join(santa_rita,by = c("PastureNam"="Pasture")) %>% group_by(PastureNam,geometry) %>% select(PastureNam,geometry,`% Use 2020`) %>% summarize(Average=mean(`% Use 2020`))
test_srer <- santa_rita_1 %>% 
  left_join(santa_rita,by = c("PastureNam"="Pasture")) %>% 
  group_by(PastureNam,geometry) %>% 
  select(PastureNam,geometry,`% Use 2020`) %>% 
  summarize(Average=mean(`% Use 2020`), .groups="drop")


ggplot() +
  geom_sf(data = test_srer, aes(fill = Average)) +
  theme_minimal() +
  scale_fill_viridis(discrete = F) +
  labs(fill = "T") +
  theme(legend.position = "none")

# Shiny UI
ui <- fluidPage(
  titlePanel("% Use 2020"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectVariable", 
                  label = "Choose a transect", 
                  choices = unique(santa_rita$Transect_Name),
                  selected = unique(santa_rita$Transect_Name)[1]
      )
    ),
    mainPanel(
      plotOutput("barPlot"),
      plotOutput("totalBarPlot")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  output$barPlot <- renderPlot({
    selected_data <- santa_rita[santa_rita$Transect_Name == input$selectVariable,] # replace by select [line134]
    
    ggplot(selected_data, aes(x = Transect_Name, y = `% Use 2020`, fill = Transect_Name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = rainbow(length(unique(santa_rita$Transect_Name)))) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("% Use 2020") +
      xlab("")
  })
  
  output$totalBarPlot <- renderPlot({
    ggplot(santa_rita, aes(x = Transect_Name, y = `% Use 2020`, fill = Transect_Name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = rainbow(length(unique(santa_rita$Transect_Name)))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab("% Use 2020") +
      xlab("")
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)
```

```{r}
ui <- fluidPage(
  titlePanel("% Use 2020"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectVariable", 
                  label = "Choose a transect", 
                  choices = unique(santa_rita$Transect_Name),
                  selected = unique(santa_rita$Transect_Name)[1]
      )
    ),
    mainPanel(
      plotOutput("mapPlot"),  # Add this line to create a spot for your map
      plotOutput("barPlot"),
      plotOutput("totalBarPlot")
    )
  )
)
server <- function(input, output, session) {
  
  output$mapPlot <- renderPlot({
    ggplot() +
      geom_sf(data = test_srer, aes(fill = Average)) +
      theme_minimal() +
      scale_fill_viridis(discrete = F) +
      labs(fill = "T") +
      theme(legend.position = "none")
  })
  
 
}
shinyApp(ui = ui, server = server)
```

```{r}
# Shiny UI
ui <- fluidPage(
  titlePanel("% Use by Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("selectYear", 
                  label = "Choose a year", 
                  min = 2010, max = 2023, value = 2020)
    ),
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  output$mapPlot <- renderPlot({
    # Adjust the data based on the selected year
    selected_column <- paste0("% Use ", input$selectYear)
    
    selected_data <- test_srer  %>%
      group_by(Pasture) %>%
      summarize(Average = mean(.data[[selected_column]], na.rm = TRUE)) %>%
      left_join(santa_rita_1, by = "Pasture")
    
    ggplot() +
      geom_sf(data = selected_data, aes(fill = Average)) +
      theme_minimal() +
      scale_fill_viridis(discrete = FALSE) +
      labs(fill = paste0("Average % Use in ", input$selectYear)) +
      theme(legend.position = "none")
  })
  
}

# Run Shiny app
shinyApp(ui = ui, server = server)
```

```{r}
# Shiny UI
ui <- fluidPage(
  titlePanel("% Use by Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("selectYear", 
                  label = "Choose a year", 
                  min = 2010, max = 2023, value = 2020)
    ),
    mainPanel(
      plotOutput("mapPlot")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  output$mapPlot <- renderPlot({
    # Adjust the data based on the selected year
    selected_column <- paste0("% Use ", input$selectYear)
    
    selected_data <- santa_rita %>%
      group_by(Pasture) %>%
      summarize(Average = mean(.data[[selected_column]], na.rm = TRUE)) %>%
      left_join(santa_rita_1, by = "Pasture")
    
    ggplot() +
      geom_sf(data = selected_data, aes(fill = Average)) +
      theme_minimal() +
      scale_fill_viridis(discrete = FALSE) +
      labs(fill = paste0("Average % Use in ", input$selectYear)) +
      theme(legend.position = "none")
  })
  
}

# Run Shiny app
shinyApp(ui = ui, server = server)
```

```{r}
# Shiny UI
ui <- fluidPage(
  titlePanel("% Use by Year"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("selectYear", 
                  label = "Choose a year", 
                  min = 2010, max = 2023, value = 2020)
    ),
    mainPanel(
      plotOutput("mapPlot"),
      plotOutput("barPlot"),
      plotOutput("totalBarPlot")
    )
  )
)

# Shiny Server
server <- function(input, output, session) {
  
  output$mapPlot <- renderPlot({
    # Adjust the data based on the selected year
    selected_column <- paste0("% Use ", input$selectYear)
    
    selected_data <- test_srer  %>%
      group_by(Pasture) %>%
      summarize(Average = mean(.data[[selected_column]], na.rm = TRUE)) %>%
      left_join(santa_rita_1, by = "Pasture")
    
    ggplot() +
      geom_sf(data = selected_data, aes(fill = Average)) +
      theme_minimal() +
      scale_fill_viridis(discrete = FALSE) +
      labs(fill = paste0("Average % Use in ", input$selectYear)) +
      theme(legend.position = "none")
  })

  output$barPlot <- renderPlot({
    selected_data <- santa_rita[santa_rita$Transect_Name == input$selectVariable,]
    
    ggplot(selected_data, aes_string(x = "Transect_Name", y = selected_column, fill = "Transect_Name")) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = rainbow(length(unique(santa_rita$Transect_Name)))) + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab(selected_column) +
      xlab("")
  })
  
  output$totalBarPlot <- renderPlot({
    ggplot(santa_rita, aes_string(x = "Transect_Name", y = selected_column, fill = "Transect_Name")) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = rainbow(length(unique(santa_rita$Transect_Name)))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ylab(selected_column) +
      xlab("")
  })
}

# Run Shiny app
shinyApp(ui = ui, server = server)
```




```{r}
# Not ready
santa_rita_data$Num_Years <- apply(!is.na(santa_rita[, 8:29]), 1, sum, na.rm = TRUE)
print(santa_rita_data$Num_Years)

santa_rita_data$Adjusted_Use <- rowMeans(santa_rita[, 8:29], na.rm = TRUE)
print(santa_rita_data$Adjusted_Use)


print(st_crs(santa_rita_1))
print(st_crs(santa_rita_sf))
```



