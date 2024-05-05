library(readr)
library(tidyr)
library(dplyr)
library(stringr)

FFdata_original <- read_delim("~/AnalysisProject/FastFoodNutritionMenuV3.csv")
diet_original <- read_delim("~/AnalysisProject/Diet.csv")
activity_original <- read_delim("~/AnalysisProject/ActivityCategories.csv")


calorieBudget <- 700
#constants proven by science
cal_Per_GrOfProtein <- 4
cal_Per_GrOfFat <- 9
cal_Per_GrOfCarbs <- 4
cal_Per_GrOfFiber <- 2

#preserve the original data files
diet <- diet_original
activityCategory <- activity_original

#Cleaning data

#remove unused row: weightwatchers
data<- data %>% subset(.,select = -c(WeightWatchersPnts))
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

sapply(data, class)

#Remove spaces from column names
names(data) <- gsub("\\s+", "", names(data))
names(diet) <- gsub("\\s+", "", names(diet))

#Remove items that have no effect on the calorie budget or we have no information on it.
data <- data %>% filter(.$Calories != -1 ) %>% filter(.$Calories != 0 )
#Remove items that are not named or do not have the company name 
data <- data %>% filter(.$Item != "", .$Company != "")

#need to remove these rows that only have calories coming from color additives.
color_additives <- data %>% filter( .$`TotalFat(g)` == 0 , .$`Carbs(g)` == 0 , .$`Protein(g)` == 0 )
data<- data %>% filter( .$`TotalFat(g)` != 0 , .$`Carbs(g)` != 0 , .$`Protein(g)` != 0 )

#Remove dublicated data
data <- data[!duplicated(data),]


#----------------FIRST METHOD-----------------------------------------------------------------

#new dataset where we only see calories, total fat,protein and carbs as well as the % of each
macro_col <- c("Company","Item","Calories","TotalFat(g)","Carbs(g)","Protein(g)")
macros_1 <- data %>% select(all_of(macro_col))
macros_1 <- macros_1 %>%
  #calFromFat_S means we are using scientific value and not data$CaloriesfromFat (which is not too far off)
  mutate(calFromFat_S = .$`TotalFat(g)`*cal_Per_GrOfFat,
         calFromCarbs_S = .$`Carbs(g)`*cal_Per_GrOfCarbs,
         calFromProtein_S = .$`Protein(g)`*cal_Per_GrOfProtein) %>%
  mutate(TotalCalories_S = .$calFromProtein_S + .$calFromCarbs_S + .$calFromFat_S)

# Mean Absolute Error (MAE)
mae_1 <- mean(abs(macros_1$TotalCalories_S - macros_1$Calories))

# Mean Squared Error (MSE)
mse_1 <- mean((macros_1$TotalCalories_S - macros_1$Calories)^2)

# Root Mean Squared Error (RMSE)
rmse_1 <- sqrt(mse_1)

# Coefficient of Determination (R-squared)
r_squared_1 <- 1 - (sum((macros_1$Calories - macros_1$TotalCalories_S)^2) / sum((macros_1$Calories - mean(macros_1$Calories))^2))

# Percent Error
percent_error_1 <- mean(abs((macros_1$TotalCalories_S - macros_1$Calories) / macros_1$Calories)) * 100


# Calculate variance of absolute differences
variance_1 <- var(abs(macros_1$Calories - macros_1$TotalCalories_S))

#Calculate Standard Deviation
deviation_1 <- sqrt(variance_1)

# Print the results
print(paste("MAE:", mae_1))
print(paste("MSE:", mse_1))
print(paste("RMSE:", rmse_1))
print(paste("R-squared:", r_squared_1))
print(paste("Percent Error:", percent_error_1, "%"))
print(paste("Variance:", variance_1))
print(paste("Standard Deviation:", deviation_1))

#-----------------------SECOND METHOD----------------------------------------------

#new dataset where we only see calories, total fat,protein and carbs as well as the % of each
macro_col <- c("Company","Item","Calories","TotalFat(g)","Carbs(g)","Protein(g)", "Fiber(g)")
macros_2 <- data %>% select(all_of(macro_col))
macros_2 <- macros_2 %>%
  #calFromFat_S means we are using scientific value and not data$CaloriesfromFat (which is not too far off)
  mutate(calFromFat_S = .$`TotalFat(g)`*cal_Per_GrOfFat,
         calFromCarbs_S = .$`Carbs(g)`*cal_Per_GrOfCarbs,
         calFromProtein_S = .$`Protein(g)`*cal_Per_GrOfProtein,
         calFromFiber_S = .$`Fiber(g)`*cal_Per_GrOfFiber) %>%
  mutate(TotalCalories_S = .$calFromProtein_S + .$calFromCarbs_S + .$calFromFat_S + .$calFromFiber_S)

# Mean Absolute Error (MAE)
mae_2 <- mean(abs(macros_2$TotalCalories_S - macros_2$Calories))

# Mean Squared Error (MSE)
mse_2 <- mean((macros_2$TotalCalories_S - macros_2$Calories)^2)

# Root Mean Squared Error (RMSE)
rmse_2 <- sqrt(mse_2)

# Coefficient of Determination (R-squared)
r_squared_2 <- 1 - (sum((macros_2$Calories - macros_2$TotalCalories_S)^2) / sum((macros_2$Calories - mean(macros_2$Calories))^2))

# Percent Error
percent_error_2 <- mean(abs((macros_2$TotalCalories_S - macros_2$Calories) / macros_2$Calories)) * 100


# Calculate variance of absolute differences
variance_2 <- var(abs(macros_2$Calories - macros_2$TotalCalories_S))

#Calculate Standard Deviation
deviation_2 <- sqrt(variance_2)

# Print the results
print(paste("MAE:", mae_2))
print(paste("MSE:", mse_2))
print(paste("RMSE:", rmse_2))
print(paste("R-squared:", r_squared_2))
print(paste("Percent Error:", percent_error_2, "%"))
print(paste("Variance:", variance_2))
print(paste("Standard Deviation:", deviation_2))


#-------------------THIRD METHOD----------------------------------------------------

#new dataset where we only see calories, total fat,protein and carbs as well as the % of each
macro_col <- c("Company","Item","Calories","TotalFat(g)","Carbs(g)","Protein(g)", "Fiber(g)")
macros_3 <- data %>% select(all_of(macro_col))
macros_3 <- macros_3 %>%
  #calFromFat_S means we are using scientific value and not data$CaloriesfromFat (which is not too far off)
  mutate(calFromFat_S = .$`TotalFat(g)`*cal_Per_GrOfFat,
         calFromCarbs_S = .$`Carbs(g)`*cal_Per_GrOfCarbs,
         calFromProtein_S = .$`Protein(g)`*cal_Per_GrOfProtein,
         calFromFiber_S = .$`Fiber(g)`*cal_Per_GrOfFiber) %>%
  mutate(TotalCalories_S = .$calFromProtein_S + .$calFromCarbs_S + .$calFromFat_S - .$calFromFiber_S)

# Mean Absolute Error (MAE)
mae_3 <- mean(abs(macros_3$TotalCalories_S - macros_3$Calories))

# Mean Squared Error (MSE)
mse_3 <- mean((macros_3$TotalCalories_S - macros_3$Calories)^2)

# Root Mean Squared Error (RMSE)
rmse_3 <- sqrt(mse_3)

# Coefficient of Determination (R-squared)
r_squared_3 <- 1 - (sum((macros_3$Calories - macros_3$TotalCalories_S)^2) / sum((macros_3$Calories - mean(macros_3$Calories))^2))

# Percent Error
percent_error_3 <- mean(abs((macros_3$TotalCalories_S - macros_3$Calories) / macros_3$Calories)) * 100


# Calculate variance of absolute differences
variance_3 <- var(abs(macros_3$Calories - macros_3$TotalCalories_S))

#Calculate Standard Deviation
deviation_3 <- sqrt(variance_3)

# Print the results
print(paste("MAE:", mae_3))
print(paste("MSE:", mse_3))
print(paste("RMSE:", rmse_3))
print(paste("R-squared:", r_squared_3))
print(paste("Percent Error:", percent_error_3, "%"))
print(paste("Variance:", variance_3))
print(paste("Standard Deviation:", deviation_3))







