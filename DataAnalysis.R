library(readr)
library(tidyr)
library(dplyr)
library(stringr)

FFdata_original <- read_delim("~/AnalysisProject/FastFoodNutritionMenuV3.csv")
diet_original <- read_delim("~/AnalysisProject/Diet.csv")
activity_original <- read_delim("~/AnalysisProject/ActivityCategories.csv")

caloriesAllowed <- 700
cal_Per_GrOfProtein <- 4
cal_Per_GrOfFat <- 9
cal_Per_GrOfCarbs <- 4


data <- FFdata_original %>% convert_to_numeric %>%
  mutate(across(-c("Company", "Item"), ~ifelse(is.na(.) , 0, .))) %>%
  mutate(across(-c("Company", "Item"), ~ifelse( . == "<5" , 5, .)))  %>%
  mutate(across(-c("Company", "Item"), ~ifelse( . == "<1" , 1, .)))