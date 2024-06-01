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


#remove unused column: weightwatchers
data<- data %>% subset(.,select = -c(WeightWatchersPnts))

#Remove items that have no effect on the calorie budget or we have no information on it.
data <- data %>% filter(.$Calories != -1 ) %>% filter(.$Calories != 0 )
#Remove items that are not named or do not have the company name 
data <- data %>% filter(.$Item != "", .$Company != "")

#need to remove these rows that only have calories coming from color additives.
color_additives <- data %>% filter( .$`TotalFat(g)` == 0 , .$`Carbs(g)` == 0 , .$`Protein(g)` == 0 )
data<- data %>% filter( .$`TotalFat(g)` != 0 , .$`Carbs(g)` != 0 , .$`Protein(g)` != 0 ) %>% 
  filter(.$`TotalFat(g)` != -1 , .$`Carbs(g)` != -1 , .$`Protein(g)` != -1 )

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


#-----------------CHOSEN: METHOD 1----------------------------------------------------

#Using Mean Absolute Error (MAE) would be appropriate in this case. 
#MAE directly measures the average magnitude of errors in a set of predictions, 
#which aligns well with comparing the total calorie real value versus the estimated calorie value. 
#MAE provides a straightforward interpretation of the average discrepancy between the estimated and actual values,
#making it a suitable metric for assessing the accuracy of my estimation model in terms of calorie prediction.

macros_1 <- macros_1 %>% 
                      mutate("Fat%"= (.$calFromFat_S/.$TotalCalories_S)*100) %>%
                      mutate("Carbs%"= (.$calFromCarbs_S/.$TotalCalories_S)*100) %>%
                      mutate("Protein%"= (.$calFromProtein_S/.$TotalCalories_S)*100) 

# no item conforms completely to a diet group => need to create a ranking system.
LC_items <- macros_1 %>% filter(.$`Protein%` <= (pull(diet[1,2]) - 2.5 ), .$`Protein%` >= ( pull(diet[1,2]) + 2.5  ), macros_1$`Carbs%`<= (pull(diet[1,3]) - 2.5 ) , .$`Carbs%` >= ( pull(diet[1,3]) + 2.5  ) , .$`Fat%` <= (pull(diet[1,4]) - 2.5 ) , .$`Fat%`>= ( pull(diet[1,4]) + 2.5  )  )

LF_items <- macros_1 %>% filter(.$`Protein%` <= (pull(diet[2,2]) - 2.5 ), .$`Protein%` >= ( pull(diet[2,2]) + 2.5  ), macros_1$`Carbs%`<= (pull(diet[2,3]) - 2.5 ) , .$`Carbs%` >= ( pull(diet[2,3]) + 2.5  ) , .$`Fat%` <= (pull(diet[2,4]) - 2.5 ) , .$`Fat%`>= ( pull(diet[2,4]) + 2.5  )  )

B1_items <- macros_1 %>% filter(.$`Protein%` <= (pull(diet[3,2]) - 2.5 ), .$`Protein%` >= ( pull(diet[3,2]) + 2.5  ), macros_1$`Carbs%`<= (pull(diet[3,3]) - 2.5 ) , .$`Carbs%` >= ( pull(diet[3,3]) + 2.5  ) , .$`Fat%` <= (pull(diet[3,4]) - 2.5 ) , .$`Fat%`>= ( pull(diet[3,4]) + 2.5  )  )

B2_items <- macros_1 %>% filter(.$`Protein%` <= (pull(diet[4,2]) - 2.5 ), .$`Protein%` >= ( pull(diet[4,2]) + 2.5  ), macros_1$`Carbs%`<= (pull(diet[4,3]) - 2.5 ) , .$`Carbs%` >= ( pull(diet[4,3]) + 2.5  ) , .$`Fat%` <= (pull(diet[4,4]) - 2.5 ) , .$`Fat%`>= ( pull(diet[4,4]) + 2.5  )  )

HC_items <- macros_1 %>% filter(.$`Protein%` <= (pull(diet[5,2]) - 2.5 ), .$`Protein%` >= ( pull(diet[5,2]) + 2.5  ), macros_1$`Carbs%`<= (pull(diet[5,3]) - 2.5 ) , .$`Carbs%` >= ( pull(diet[5,3]) + 2.5  ) , .$`Fat%` <= (pull(diet[5,4]) - 2.5 ) , .$`Fat%`>= ( pull(diet[5,4]) + 2.5  )  )

## create a conformity score by calculating how off the macro percentages are
##dont count if the carbs are lower than indicated value
##punish more heavily for high carbs deviation. 
#normalizing the deviations proportionally to the diet macros.
macros_1 <- macros_1 %>% mutate(ConformityScore_LC = 100 - (0.2*abs(pull(diet[1,2]) - macros_1$`Protein%`) + 0.6*ifelse(macros_1$`Carbs%` > pull(diet[1,3]), abs(pull(diet[1,3]) -macros_1$`Carbs%`), 0)+ 0.2*abs(pull(diet[1,4]) - macros_1$`Fat%`)))
                                                      
macros_1 <- macros_1 %>% mutate(ConformityScore_LF = 100 - (0.2*abs(pull(diet[2,2]) - macros_1$`Protein%`) + 0.2*abs(pull(diet[2,3]) -macros_1$`Carbs%`)+ 0.6*ifelse(macros_1$`Fat%` > pull(diet[2,4]), abs(pull(diet[2,4]) - macros_1$`Fat%`),0)))

macros_1 <- macros_1 %>% mutate(ConformityScore_B1 = 100 - (0.34*abs(pull(diet[3,2]) - macros_1$`Protein%`) + 0.34*abs(pull(diet[3,3]) -macros_1$`Carbs%`)+ 0.32*abs(pull(diet[3,4]) - macros_1$`Fat%`)))

macros_1 <- macros_1 %>% mutate(ConformityScore_B2 = 100 - (0.34*abs(pull(diet[4,2]) - macros_1$`Protein%`) + 0.32*abs(pull(diet[4,3]) -macros_1$`Carbs%`)+ 0.34*abs(pull(diet[4,4]) - macros_1$`Fat%`)))

macros_1 <- macros_1 %>% mutate(ConformityScore_HC = 100 - (0.45*ifelse(macros_1$`Protein%` > pull(diet[5,2]) , abs(pull(diet[5,2]) - macros_1$`Protein%`) ,0 ) + 0.1*abs(pull(diet[4,3]) -macros_1$`Carbs%`)+ 0.45*ifelse(macros_1$`Fat%` > pull(diet[5,4]), abs(pull(diet[5,4]) - macros_1$`Fat%`),0)))

#overwrite the empty already existing data frames

LC_items <- macros_1 %>% select("Company" , "Item" , "ConformityScore_LC")%>%
  arrange(desc(ConformityScore_LC))

LF_items <- macros_1 %>% select("Company" , "Item", "ConformityScore_LF")%>%
  arrange(desc(ConformityScore_LF))

B1_items <- macros_1 %>% select("Company" , "Item", "ConformityScore_B1")%>%
  arrange(desc(ConformityScore_B1))

B2_items <- macros_1 %>% select("Company" , "Item", "ConformityScore_B2")%>%
  arrange(desc(ConformityScore_B2))

HC_items <- macros_1 %>% select("Company" , "Item", "ConformityScore_HC")%>%
  arrange(desc(ConformityScore_HC))


                                                
                                                      
