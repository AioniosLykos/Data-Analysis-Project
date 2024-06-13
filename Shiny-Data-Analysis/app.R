library(ggplot2)
library(shiny)
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
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

calorieMinBudget <- 300
calorieIntervalDiff <- 200
#constants proven by science
cal_Per_GrOfProtein <- 4
cal_Per_GrOfFat <- 9
cal_Per_GrOfCarbs <- 4
cal_Per_GrOfFiber <- 2

#preserve the original data files
diet <- diet_original

#Cleaning data
clean_data <- function(input_data) {
  data <- input_data %>%
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
  
  
  return(data)
  
}

cleanData <- clean_data(FFdata_original)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@700&display=swap")
  ),
  fluidRow(
    column(
      width = 12,
      align = "center",
      titlePanel(HTML("<span style='font-size: 24px; color: #1E90FF; font-weight: bold; font-family: Roboto, sans-serif;'>Optimizing Fast Food Choices from Top 6 Chains: A Data-Driven Analysis Based on Dietary Types</span>"))
    )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("diet_type", "Select Diet Type:",
                  choices = c("Low Carb", "Low Fat", "Balanced1", "Balanced2", "High Carb"),
                  selected = "Low Carb"),
        # Output for displaying the diet table
    ),
    mainPanel(
      DTOutput("dietTable"),
      DTOutput("cleanedTable")  # Output for displaying the cleaned datatable
      
    )
  )
)


server <- function(input, output) {
  # Reactive expression for cleaned data
  cleaned_data <- reactive({
    clean_data(FFdata_original)
  })
  
  # Reactive expression for diet table
  dietTable <- reactive({
    diet_original  # Assuming diet_original is read from Diet.csv
  })
  
  # Render the cleaned datatable as a reactive output
  output$cleanedTable <- renderDT({
    cleaned_data()
  })
  
  # Render the diet table as a reactive output
  output$dietTable <- renderDT({
    dietTable()
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
