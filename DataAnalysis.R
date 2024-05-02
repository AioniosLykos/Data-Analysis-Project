library(readr)
library(tidyr)
library(dplyr)
library(stringr)

FFdata_original <- read_delim("~/AnalysisProject/FastFoodNutritionMenuV3.csv")
diet_original <- read_delim("~/AnalysisProject/Diet.csv")
activity_original <- read_delim("~/AnalysisProject/ActivityCategories.csv")


caloriesAllowed <- 700
#constants proven by science
cal_Per_GrOfProtein <- 4
cal_Per_GrOfFat <- 9
cal_Per_GrOfCarbs <- 4
cal_Per_GrOfFiber <- 2

#preserve the original data files
diet <- diet_original
activityCategory <- activity_original

#Cleaning data
data <- FFdata_original %>%
  mutate(across(-c("Company", "Item"), ~ifelse(is.na(.) , -1, .))) %>%
  mutate(across(-c("Company", "Item"), ~ifelse( . == "<5" , 5, .)))  %>%
  mutate(across(-c("Company", "Item"), ~ifelse( . == "<1" , 1, .))) %>%
  mutate(across(-c("Company", "Item"), ~ifelse( . == "" , -1, .))) 

data<- data %>%
  mutate(across(-c("Company", "Item"), ~{ifelse(grepl("\\d+\\.\\d+", .), .,
                                                ifelse(grepl("[^A-Za-z0-9.]", .), -1, .))
                                        } ) )

#change necessary column types to numeric
data <- data %>%
  mutate_at(vars(-c("Company", "Item")), as.numeric)

sapply(diet, class)

#Remove spaces from column names
names(data) <- gsub("\\s+", "", names(data))
names(diet) <- gsub("\\s+", "", names(diet))

#new dataset where we only see calories, total fat,protein and carbs as well as the % of each
macro_col <- c("Company","Item","Calories","TotalFat(g)","Carbs(g)","Protein(g)")
macros <- data %>% select(all_of(macro_col))
macros <- macros %>%
         #calFromFat_S means we are using scientific value and not data$CaloriesfromFat (which is not too far off)
                mutate(calFromFat_S = .$`TotalFat(g)`*cal_Per_GrOfFat,
                       calFromCarbs_S = .$`Carbs(g)`*cal_Per_GrOfCarbs,
                       calFromProtein_S = .$`Protein(g)`*cal_Per_GrOfProtein) %>%
                mutate(TotalCalories_S = .$calFromProtein_S + .$calFromCarbs_S + .$calFromFat_S)

# Mean Absolute Error (MAE)
mae <- mean(abs(macros$TotalCalories_S - macros$Calories))

# Mean Squared Error (MSE)
mse <- mean((macros$TotalCalories_S - macros$Calories)^2)

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mse)

# Coefficient of Determination (R-squared)
r_squared <- 1 - (sum((macros$Calories - macros$TotalCalories_S)^2) / sum((macros$Calories - mean(macros$Calories))^2))

# Percent Error
# Check if denominator (real) is not zero before calculating percent error
if (all(macros$Calories != 0)) {
  percent_error <- mean(abs((macros$TotalCalories_S - macros$Calories) / macros$Calories)) * 100
} else {
  # Exclude cases where actual values are zero from the percent error calculation
  percent_error <- mean(abs((macros$TotalCalories_S - macros$Calories) / macros$Calories[macros$Calories != 0])) * 100
}

# Calculate variance of absolute differences
variance <- var(abs(macros$Calories - macros$TotalCalories_S))

#Calculate Standard Deviation
deviation <- sqrt(variance)

# Print the results
print(paste("MAE:", mae))
print(paste("MSE:", mse))
print(paste("RMSE:", rmse))
print(paste("R-squared:", r_squared))
print(paste("Percent Error:", percent_error, "%"))
print(paste("Variance:", variance))
print(paste("Standard Deviation:", deviation))






