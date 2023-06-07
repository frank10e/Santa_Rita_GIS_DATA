---
title: "Untitled"
author: "Xiuchen Lu"
date: "2023-05-28"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
getwd()
setwd("/Users/luxiuchen/Downloads")
library(sf)
library(ggplot2)
library(rgdal)
library(sp)
library(viridis)
library(readxl)
```
```{r}
# read excel file
santa_rita <- read_excel("/Users/luxiuchen/Downloads/Yearly Utilization by XL.xlsx", sheet = "Utilization")
# read shp file
santa_rita_1 <- read_sf("/Users/luxiuchen/Desktop/project research data/UpdatedSRER_04_27_2023.shp")
santa_rita_GIS<- c("Pasture", "Transect", "Transect_Name", "UTM start X", "UTM start Y", "UTM end X", "UTM end Y")
santa_rita_data <- santa_rita[, santa_rita_GIS]
```
```{r}
santa_rita_data_clean <- na.omit(santa_rita_data)
santa_rita_data_plot <- st_as_sf(santa_rita_data_clean, coords = c("UTM start X", "UTM start Y"), crs = "+proj=utm +zone=12 +datum=WGS84")
plot(santa_rita_data_plot, col = "blue", pch = 16, cex = 2)
santa_rita_sf <- santa_rita_data_plot

# change Transect_Name to factor variable
santa_rita_data$Transect_Name <- as.factor(santa_rita_data$Transect_Name)


# plot santa rita geomtry graph
ggplot() +
  geom_sf(data = santa_rita_data_plot, aes(fill = Transect_Name)) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(fill = "Transect Name") +
  theme(legend.text = element_text(size = 3))
ggsave("santa_rita_plot.png", width = 20, height = 8)


# plot basic santa_rita file
ggplot() +
  geom_sf(data = santa_rita_1) +
  theme_minimal()
# change character to factor
santa_rita_1$PastureNam <- as.factor(santa_rita_1$PastureNam)
# plot the Santa_Rita pastureNam with color
ggplot() +
  geom_sf(data = santa_rita_1, aes(fill = PastureNam)) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(fill = "PastureNam") +
  theme(legend.text = element_text(size = 1)) +
  geom_sf(data = santa_rita_sf)



ggsave("santa_rita_plot.png", width = 20, height = 8)
unique(santa_rita_1$PastureNam)



col_names <- names(santa_rita)
print(col_names)

print(st_crs(santa_rita_1))
print(st_crs(santa_rita_sf))

santa_rita_sf <- st_transform(santa_rita_sf, st_crs(santa_rita_1))
result <- st_join(santa_rita_1, santa_rita_sf)
print(head(result))


ggplot() +
  geom_sf(data = result, aes(fill = Transect_Name)) +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  labs(fill = "Transect Name") +
  theme(legend.position = "none")


santa_rita_data$Num_Years <- apply(!is.na(santa_rita[, 8:29]), 1, sum, na.rm = TRUE)
print(santa_rita_data$Num_Years)

santa_rita_data$Adjusted_Use <- rowMeans(santa_rita[, 8:29], na.rm = TRUE)
print(santa_rita_data$Adjusted_Use)


print(st_crs(santa_rita_1))
print(st_crs(santa_rita_sf))




