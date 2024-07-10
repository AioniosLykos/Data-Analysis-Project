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

#Final Usable Data Table 
macros <- cleanData
macros <- macros %>%
  #calFromFat_S means we are using scientific value and not data$CaloriesfromFat (which is not too far off)
  mutate(calFromFat_S = .$'TotalFat(g)'*cal_Per_GrOfFat,
         calFromCarbs_S = .$'Carbs(g)'*cal_Per_GrOfCarbs,
         calFromProtein_S = .$'Protein(g)'*cal_Per_GrOfProtein) %>%
  mutate(TotalCalories_S = .$calFromProtein_S + .$calFromCarbs_S + .$calFromFat_S)
macros <- macros %>% 
  mutate("Fat%"= (.$calFromFat_S/.$TotalCalories_S)*100) %>%
  mutate("Carbs%"= (.$calFromCarbs_S/.$TotalCalories_S)*100) %>%
  mutate("Protein%"= (.$calFromProtein_S/.$TotalCalories_S)*100) %>%
  select("Company" , "Item","TotalCalories_S", "Protein%", "Carbs%", "Fat%") %>%
  rename( "Calories" = "TotalCalories_S")

  
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
      
      # Two-sided interval for calorie budget
      sliderInput("calorie_budget", "Calorie Budget:",
                  min = 0, max = 1500, value = c(300, 700),
                  step = 25, sep = ""),
      
      # Output for displaying the diet table
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Diet",
                 tabsetPanel(
                   tabPanel("Preselected Macros",
                            htmlOutput("title1"),
                            DTOutput("dietTable")
                   ),
                   tabPanel("Personalized Data Entry",
                            h3("Enter your macro data"),
                            textInput("macro_name", "Name:", ""),
                            div(
                              style = "display: flex; justify-content: space-between;",
                              div(
                                h4(""),
                                numericInput("protein_percent", "Protein (%)", value = 0, min = 0, max = 100, step = 1)
                              ),
                              div(
                                h4(""),
                                numericInput("carbs_percent", "Carbs (%)", value = 0, min = 0, max = 100, step = 1)
                              ),
                              div(
                                h4(""),
                                numericInput("fat_percent", "Fat (%)", value = 0, min = 0, max = 100, step = 1)
                              )
                            ),
                            actionButton("add_macro", "Add Diet"),
                            br(), # New line for spacing
                            br(), # New line for spacing
                            div(
                              class = "row",
                              div(
                                class = "col-md-6",
                                h3("Select Diet to Delete:"),
                                uiOutput("select_diet_to_delete")
                              ),
                              div(
                                class = "col-md-6",
                                h3(""),
                                actionButton("delete_macro", "Confirm Delete")
                              )
                            ),
                            br(), # New line for spacing
                            DTOutput("macro_table"),
                            br(), # New line for spacing
                            textOutput("validation_message"),
                            br() # New line for spacing
                   )
                 )
        )
        , tabPanel("Data Tables",
                   tabsetPanel(
                     tabPanel("Raw Data Table",
                              
                              DTOutput("original")
                     ),
                     tabPanel("Cleaned Data Table",
                              DTOutput("cleanedTable")
                     ),
                     tabPanel("Macros% Data Table",
                              DTOutput("finalTable")
                     )
                   )
        ))
    )
  ))

# Define server logic
server <- function(input, output, session) {
  # Reactive expression for cleaned data
  cleaned_data <- reactive({
    clean_data(FFdata_original)
  })
  
  # Reactive expression for diet table
  dietTable <- reactive({
    # Replace with your dietTable reactive expression or function
    # diet_original  # Assuming diet_original is read from Diet.csv
    diet_original
  })
  
  # Reactive value to store diet types
  diet_types <- reactiveVal(c("Low Carb", "Low Fat", "Balanced1", "Balanced2", "High Carb"))
  
  # Reactive values to store user-entered macro data
  user_macros <- reactiveVal(data.frame(Name = character(), Protein = numeric(), Carbs = numeric(), Fat = numeric(), stringsAsFactors = FALSE))
  
  # Render the cleaned datatable as a reactive output
  output$cleanedTable <- renderDT({
    cleaned_data()
  })
  output$original <- renderDT(FFdata_original)
  output$macrosTable <- renderDT(macros)
  
  output$title1 <- renderUI({
    HTML('<h3 style="color:blue; font-style:italic;">Macros of Preselected Diet Types</h3>')
  })
  # Render the diet table as a reactive output
  output$dietTable <- renderDT({
    dietTable()
  }, options = list(
    dom = 't',       # Only show table, no other controls
    paging = FALSE,  # Disable pagination
    info = FALSE     # Disable info text (e.g., "Showing 1 to 10 of 100 entries")
  ))
  
  # Add macro to the table when button is clicked
  observeEvent(input$add_macro, {
    total_percent <- input$protein_percent + input$carbs_percent + input$fat_percent
    if (total_percent == 100) {
      # Check if macro_name already exists or is a predefined diet type
      if (input$macro_name %in% user_macros()$Name || input$macro_name %in% diet_types()) {
        output$validation_message <- renderText({ "This name is already used or it's a predefined diet type. Please choose a different name." })
        return()
      } else {
        output$validation_message <- renderText({ "" })
      }
      
      new_macro <- data.frame(
        Name = input$macro_name,
        Protein = input$protein_percent,
        Carbs = input$carbs_percent,
        Fat = input$fat_percent,
        stringsAsFactors = FALSE
      )
      # Concatenate new_macro with existing user_macros()
      updated_macros <- rbind(user_macros(), new_macro)
      user_macros(updated_macros)
      
      # Update diet_types with the new macro name
      diet_types(c(diet_types(), input$macro_name))
    } else {
      output$validation_message <- renderText({ "The percentages must sum up to 100%." })
    }
  })
  
  # Delete selected macro from user_macros when button is clicked
  observeEvent(input$delete_macro, {
    if (!is.null(input$select_diet_to_delete)) {
      updated_macros <- user_macros()
      updated_macros <- updated_macros[!(updated_macros$Name %in% input$select_diet_to_delete), ]
      user_macros(updated_macros)
      
      # Update diet_types excluding the deleted macro name
      diet_types(diet_types()[!(diet_types() %in% input$select_diet_to_delete)])
    }
  })
  
  # Render the user-entered macro table
  output$macro_table <- renderDT({
    user_macros()
  }, options = list(
    paging = FALSE,  # Disable pagination
    searching = FALSE,  # Disable search
    info = FALSE     # Disable info text
  ))
  
  output$finalTable <- renderDT({
    datatable(macros, options = list(
      columnDefs =  list(
        list(targets = c(1, 2), orderable = FALSE)  # Assuming Company is the first column and Item is the second
      )
    )) %>%
      formatRound(columns = c("Protein%", "Carbs%", "Fat%"), digits = 4)
  })
  # Dynamically render selectInput "select_diet_to_delete"
  output$select_diet_to_delete <- renderUI({
    selectInput("select_diet_to_delete", "",
                choices = user_macros()$Name)
  })
  
  # Update selectInput "diet_type" dynamically based on user_macros and diet_types
  observe({
    updated_choices <- unique(c(diet_types(), user_macros()$Name))
    updateSelectInput(session, "diet_type", choices = updated_choices)
  })
  
  # Initial update of selectInput "diet_type"
  observe({
    updateSelectInput(session, "diet_type", choices = diet_types())
  })
}
# Run the application 
shinyApp(ui = ui, server = server)