library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)
library(plotly)
library(DT)
library(htmlwidgets)
library(IRdisplay)
library(flexdashboard) 
library(patchwork)
library(gridExtra)

FFdata_original <- read_delim("~/AnalysisProject/FastFoodNutritionMenuV3.csv")
diet_original <- read_delim("~/AnalysisProject/Diet.csv")
activity_original <- read_delim("~/AnalysisProject/ActivityCategories.csv")

calorieMinBudget <- 300
calorieIntervalDiff <- 200
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


calorieCategories <- c("0-300 calories","300-500 calories","500-700 calories","700-900 calories","900+ calories")

macros_1 <- macros_1 %>% 
  mutate(CalorieCategory = ifelse(.$Calories <=300, calorieCategories[1], ifelse(.$Calories <=500,calorieCategories[2],  ifelse(.$Calories <=700,calorieCategories[3],  ifelse(.$Calories <=900,calorieCategories[4], calorieCategories[5])))))

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

macros_1 <- macros_1 %>% mutate(ConformityScore_B1 = 100 - (0.30*abs(pull(diet[3,2]) - macros_1$`Protein%`) + 0.37*abs(pull(diet[3,3]) -macros_1$`Carbs%`)+ 0.33*abs(pull(diet[3,4]) - macros_1$`Fat%`)))

macros_1 <- macros_1 %>% mutate(ConformityScore_B2 = 100 - (0.30*abs(pull(diet[4,2]) - macros_1$`Protein%`) + 0.33*abs(pull(diet[4,3]) -macros_1$`Carbs%`)+ 0.37*abs(pull(diet[4,4]) - macros_1$`Fat%`)))

macros_1 <- macros_1 %>% mutate(ConformityScore_HC = 100 - (0.45*ifelse(macros_1$`Protein%` > pull(diet[5,2]) , abs(pull(diet[5,2]) - macros_1$`Protein%`) ,0 ) + 0.1*abs(pull(diet[4,3]) -macros_1$`Carbs%`)+ 0.45*ifelse(macros_1$`Fat%` > pull(diet[5,4]), abs(pull(diet[5,4]) - macros_1$`Fat%`),0)))

#menu count per company
McD_menuCount <- macros_1 %>% filter(.$Company == "McDonald’s") %>% count() %>% pull()  %>% as.numeric()
KFC_menuCount <- macros_1 %>% filter(.$Company == "KFC") %>% count() %>% pull() %>% as.numeric()
BK_menuCount <- macros_1 %>% filter(.$Company == "Burger King") %>% count() %>% pull() %>% as.numeric()
Wendys_menuCount <- macros_1 %>% filter(.$Company == "Wendy’s") %>% count() %>% pull() %>% as.numeric()
TB_menuCount <- macros_1 %>% filter(.$Company == "Taco Bell") %>% count() %>% pull() %>% as.numeric()
PH_menuCount <- macros_1 %>% filter(.$Company == "Pizza Hut") %>% count() %>% pull() %>% as.numeric()

menu_counts <- c(
  "McDonald’s" = McD_menuCount,
  "KFC" = KFC_menuCount,
  "Burger King" = BK_menuCount,
  "Wendy’s" = Wendys_menuCount,
  "Taco Bell" = TB_menuCount,
  "Pizza Hut" = PH_menuCount
)
#overwrite the empty already existing data frames

LC_items <- macros_1 %>% select("Company" , "Item" , "ConformityScore_LC", "Calories","CalorieCategory","Protein(g)", "TotalFat(g)","Carbs(g)" )%>%
  arrange(desc(ConformityScore_LC))

LF_items <- macros_1 %>% select("Company" , "Item", "ConformityScore_LF", "Calories","CalorieCategory", "Protein(g)", "TotalFat(g)","Carbs(g)" )%>%
  arrange(desc(ConformityScore_LF))

B1_items <- macros_1 %>% select("Company" , "Item", "ConformityScore_B1", "Calories", "CalorieCategory", "Protein(g)", "TotalFat(g)","Carbs(g)" )%>%
  arrange(desc(ConformityScore_B1))

B2_items <- macros_1 %>% select("Company" , "Item", "ConformityScore_B2", "Calories","CalorieCategory", "Protein(g)", "TotalFat(g)","Carbs(g)" )%>%
  arrange(desc(ConformityScore_B2))

HC_items <- macros_1 %>% select("Company" , "Item", "ConformityScore_HC", "Calories", "CalorieCategory","Protein(g)", "TotalFat(g)","Carbs(g)" )%>%
  arrange(desc(ConformityScore_HC))

#Showing most conforming items to the diet

LC_table <- LC_items %>% 
  filter(ConformityScore_LC >= 90 ) %>% 
  datatable(caption = htmltools::HTML("<b><big>Most conforming items to Low Carbs Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))

LF_table <- LF_items %>%  filter(.$ConformityScore_LF >= 90 ) %>% 
  datatable(caption = htmltools::HTML("<b><big>Most conforming items to Low Fat Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))

B1_table <- B1_items %>%  filter(.$ConformityScore_B1 >= 90 ) %>% 
  datatable(caption = htmltools::HTML("<b><big>Most conforming items to Balanced1 Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))

B2_table <- B2_items %>%  filter(.$ConformityScore_B2 >= 90 ) %>% 
  datatable(caption = htmltools::HTML("<b><big>Most conforming items to Balanced2 Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))

HC_table <- HC_items %>%  filter(.$ConformityScore_HC >= 90 ) %>% 
  datatable(caption = htmltools::HTML("<b><big>Most conforming items to High Carbs Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))


LC_best <- LC_items %>%  filter(ConformityScore_LC >= 90 ) %>% count(Company, CalorieCategory) %>% mutate(MenuC = menu_counts[Company]) %>%
  ggplot(aes(x = Company, y = n, fill = Company)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round((n / MenuC) * 100, 2), "%")), vjust = -.5, size = 10/.pt) +
  labs(title = "Best Conforming,>=90, Item Count by each Company to Low Carbs Diet",
       x = "Company",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(.~CalorieCategory,scales = "free")

LF_best <- LF_items %>%  filter(ConformityScore_LF >= 90 ) %>% count(Company, CalorieCategory) %>% mutate(MenuC = menu_counts[Company]) %>%
  ggplot(aes(x = Company, y = n, fill = Company)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round((n / MenuC) * 100, 2), "%")), vjust = -.5, size = 10/.pt) +
  labs(title = "Best Conforming,>=90, Item Count by each Company to Low Fat Diet",
       x = "Company",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(.~CalorieCategory,scales = "free")

B1_best <- B1_items %>%  filter(ConformityScore_B1 >= 90 ) %>% count(Company, CalorieCategory) %>% mutate(MenuC = menu_counts[Company]) %>%
  ggplot(aes(x = Company, y = n, fill = Company)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round((n / MenuC) * 100, 2), "%")), vjust = -.5, size = 10/.pt) +
  labs(title = "Best Conforming,>=90, Item Count by each Company to Balanced1 Diet",
       x = "Company",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(.~CalorieCategory,scales = "free") 

B2_best <- B2_items %>%  filter(ConformityScore_B2 >= 90 ) %>% count(Company, CalorieCategory) %>% mutate(MenuC = menu_counts[Company]) %>%
  ggplot(aes(x = Company, y = n, fill = Company)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round((n / MenuC) * 100, 2), "%")), vjust = -.5, size = 10/.pt) +
  labs(title = "Best Conforming,>=90, Item Count by each Company to Balanced2 Diet",
       x = "Company",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(.~CalorieCategory,scales = "free")

HC_best <- HC_items %>%  filter(ConformityScore_HC >= 90) %>%  count(Company, CalorieCategory) %>% mutate(MenuC = menu_counts[Company]) %>%
  ggplot(aes(x = Company, y = n, fill = Company)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round((n / MenuC) * 100, 2), "%")), vjust = -.5, size = 10/.pt) +
  labs(title = "Best Conforming,>=90, Item Count by each Company to High Carbs Diet",
       x = "Company",
       y = "Count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_grid(.~CalorieCategory,scales = "free")

grid.arrange(LC_best,LF_best, ncol = 1)
grid.arrange(B1_best,B2_best, ncol=1)
grid.arrange(HC_best, ncol =1)



#THE Best item per company

topItemPerCompany_LC <- LC_items %>%
  group_by(Company, CalorieCategory)  %>%
  slice_max(order_by = ConformityScore_LC, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  datatable(caption = htmltools::HTML("<b><big>The best item per company conforming to Low Carb Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))
  
topItemPerCompany_LF<- LF_items %>%
  group_by(Company, CalorieCategory)  %>%
  slice_max(order_by = ConformityScore_LF, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  datatable(caption = htmltools::HTML("<b><big>The best item per company conforming to Low Fat Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))

topItemPerCompany_B1 <- B1_items %>%
  group_by(Company, CalorieCategory)  %>%
  slice_max(order_by = ConformityScore_B1, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  datatable(caption = htmltools::HTML("<b><big>The best item per company conforming to Balanced1 Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))

topItemPerCompany_B2 <- B2_items %>%
  group_by(Company, CalorieCategory)  %>%
  slice_max(order_by = ConformityScore_B2, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  datatable(caption = htmltools::HTML("<b><big>The best item per company conforming to Balanced2 Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))

topItemPerCompany_HC <- HC_items %>%
  group_by(Company, CalorieCategory)  %>%
  slice_max(order_by = ConformityScore_HC, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  datatable(caption = htmltools::HTML("<b><big>The best item per company conforming to High Carb Diet</big></b>"), filter = 'top', options = list(pageLength = 10 ))




