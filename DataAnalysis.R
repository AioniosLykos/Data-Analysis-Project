library(readr)
library(tidyr)
library(dplyr)
FFdata_original <- read_delim("~/AnalysisProject/FastFoodNutritionMenuV3.csv")
diet_original <- read_delim("~/AnalysisProject/Diet.csv")
activity_original <- read_delim("~/AnalysisProject/ActivityCategories.csv")

caloriesAllowed <- 700
cal_Per_GrOfProtein <- 4
cal_Per_GrOfFat <- 9
cal_Per_GrOfCarbs <- 4

data <- FFdata_original %>%
  mutate_at(vars(-column1, -column2), ~ifelse(is.na(.), 0, .)) %>%
  mutate_at(vars(-column1, -column2), ~ifelse(. == "", 0, .))

# Replace non-integer values < 5 with 5
data <- data %>%
  mutate_at(vars(-column1, -column2), ~ifelse(!is.na(.) & !is.integer(.) & . < 5, 5, .))











