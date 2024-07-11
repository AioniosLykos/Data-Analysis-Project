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

library(shiny)
library(shinyBS)

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
  #Remove items that are not Diet_Typed or do not have the company Diet_Type 
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
  select("Company" , "Item","TotalCalories_S", "Protein%", "Carbs%", "Fat%","Protein(g)", "TotalFat(g)","Carbs(g)") %>%
  rename( "Calories" = "TotalCalories_S")


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@700&display=swap"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/popper.js/1.14.7/umd/popper.min.js"),
    tags$script(src = "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/js/bootstrap.min.js")
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
      width = 5 ,
      selectInput("diet_type", "Select Diet Type:",
                  choices = c("Low_Carb", "Low_Fat", "Balanced1", "Balanced2", "High_Carb"),
                  selected = "Low_Carb"),
      
      # Two-sided interval for calorie budget
      sliderInput("calorie_budget", "Calorie Budget:",
                  min = 0, max = 1500, value = c(300, 700),
                  step = 25, sep = ""),
      
      fluidRow(
        column(4,
               numericInput("protein_constant", HTML("Protein Constant: <i class='fas fa-question-circle' data-toggle='tooltip' title='The closer this constant is to 1, the more you penalize calories from proteins in your diet. Protein, Carbs, and Fat constants must sum up to 1. '></i>"), value = 0.30, min = 0, max = 1, step = 0.01, width = "140%")
        ),
        column(4,
               numericInput("carbs_constant", HTML("Carbs Constant: <i class='fas fa-question-circle' data-toggle='tooltip' title='The closer this constant is to 1, the more you penalize calories from carbs in your diet. Protein, Carbs, and Fat constants must sum up to 1. '></i>"), value = 0.35, min = 0, max = 1, step = 0.01, width = "140%")
        ),
        column(4,
               numericInput("fat_constant", HTML("Fat Constant: <i class='fas fa-question-circle' data-toggle='tooltip' title='The closer this constant is to 1, the more you penalize calories from  in your diet. Protein, Carbs, and Fat constants must sum up to 1. '></i>"), value = 0.35, min = 0, max = 1, step = 0.01, width = "140%")
        )
      ),
      
      actionButton("confirmChoices", "Confirm Your Choices")
    ),
    mainPanel(
      width = 7,
      tabsetPanel(
        tabPanel("Diet",
                 tabsetPanel(
                   tabPanel("Preselected Macros",
                            htmlOutput("title1"),
                            DTOutput("dietTable")
                   ),
                   tabPanel("Personalized Data Entry",
                            h3("Enter your macro data"),
                            textInput("macro_Diet_Type", "Diet_Type:", ""),
                            div(
                              style = "display: flex; justify-content: space-between;",
                              div(
                                h4(""),
                                numericInput("protein_percent", "Protein (%)", value = 0, min = 0, max = 100, step = 1, width = "150%")
                              ),
                              div(
                                h4(""),
                                numericInput("carbs_percent", "Carbs (%)", value = 0, min = 0, max = 100, step = 1, width = "150%")
                              ),
                              div(
                                h4(""),
                                numericInput("fat_percent", "Fat (%)", value = 0, min = 0, max = 100, step = 1, width = "150%")
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
                     tabPanel("Raw Data",
                              DTOutput("original")
                     ),
                     tabPanel("Cleaned Data",
                              DTOutput("cleanedTable")
                     ),
                     tabPanel("Macros%",
                              DTOutput("finalTable")
                     ),
                     tabPanel("Conformity Score within Calorie Budget",
                              DTOutput("conformityTable")
                     )
                   )
        ))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  
  # Reactive expression for cleaned data
  cleaned_data <- reactive({
    clean_data(FFdata_original)
  })
  
  # Reactive expression for diet table
  dietTable <- reactive({
    diet_original  # Assuming diet_original is read from Diet.csv
  })
  
  # Reactive value to store diet types
  diet_types <- reactiveVal(c("Low_Carb", "Low_Fat", "Balanced1", "Balanced2", "High_Carb"))
  
  # Reactive values to store user-entered macro data
  user_macros <- reactiveVal(data.frame("Diet_Type" = character(), "Protein%" = numeric(), "Carbs%" = numeric(), "Fat%" = numeric(), stringsAsFactors = FALSE))
  
  
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
      # Check if macro_Diet_Type already exists or is a predefined diet type
      if (input$macro_Diet_Type %in% user_macros()$Diet_Type || input$macro_Diet_Type %in% diet_types()) {
        output$validation_message <- renderText({ "This Diet_Type is already used or it's a predefined diet type. Please choose a different Diet_Type." })
        return()
      } else {
        output$validation_message <- renderText({ "" })
      }
      
      new_macro <- data.frame(
        "Diet_Type" = input$macro_Diet_Type,
        "Protein%" = input$protein_percent,
        "Carbs%" = input$carbs_percent,
        "Fat%" = input$fat_percent,
        stringsAsFactors = FALSE
      )
      # Concatenate new_macro with existing user_macros()
      updated_macros <- rbind(user_macros(), new_macro)
      user_macros(updated_macros)
      
      # Update diet_types with the new macro Diet_Type
      diet_types(c(diet_types(), input$macro_Diet_Type))
    } else {
      output$validation_message <- renderText({ "The percentages must sum up to 100%." })
    }
  })
  
  # Delete selected macro from user_macros when button is clicked
  observeEvent(input$delete_macro, {
    if (!is.null(input$select_diet_to_delete)) {
      updated_macros <- user_macros()
      updated_macros <- updated_macros[!(updated_macros$Diet_Type %in% input$select_diet_to_delete), ]
      user_macros(updated_macros)
      
      # Update diet_types excluding the deleted macro Diet_Type
      diet_types(diet_types()[!(diet_types() %in% input$select_diet_to_delete)])
    }
  })
  
  # Render the user-entered macro table
  output$macro_table <- renderDT({
    datatable(user_macros(), options = list(
      paging = FALSE,  # Disable pagination
      searching = FALSE,  # Disable search
      info = FALSE     # Disable info text
    ), colnames = c("Diet_Type", "Protein%", "Carbs%", "Fat%"))
  })
  
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
                choices = user_macros()$Diet_Type)
  })
  
  # Update selectInput "diet_type" dynamically based on user_macros and diet_types
  observe({
    updated_choices <- unique(c(diet_types(), user_macros()$Diet_Type))
    updateSelectInput(session, "diet_type", choices = updated_choices)
  })
  
  # Initial update of selectInput "diet_type"
  observe({
    updateSelectInput(session, "diet_type", choices = diet_types())
  })
  
  # Observe event for confirming choices
  observeEvent(input$confirmChoices, {
    # Reactive expression to enforce sum of constants to 1
    sum_constants <- input$protein_constant + input$carbs_constant + input$fat_constant
    if (sum_constants != 1) {
      updateNumericInput(session, "protein_constant", value = 0.30)
      updateNumericInput(session, "carbs_constant", value = 0.35)
      updateNumericInput(session, "fat_constant", value = 0.35)
    }
    
    # Show notification for constants reset
    showNotification(
      "The constants (Protein, Carbs, Fat) have been reset to default values because their sum did not equal 1.",
      duration = 5000,  # Duration in milliseconds (e.g., 5000 ms = 5 seconds)
      type = "warning"  # Notification type
    )
    
    
    selected_diet_type <- input$diet_type
    calorie_budget_min <- input$calorie_budget[1]
    calorie_budget_max <- input$calorie_budget[2]
    
    
    if (!(selected_diet_type %in% user_macros()$Diet_Type)) {
      cat("in diet\n")
      diet_data <- dietTable() %>%
        filter(Diet_Type == selected_diet_type)  # Filter diet data for selected type
      if (nrow(diet_data) == 0) {
        cat("Selected diet type not found in diet data.\n")
        return()  # Exit function if diet type not found
      }
      protein <- diet_data$`Protein %`
      carbs <- diet_data$`Carbs %`
      fat <- diet_data$`Fat %`
    } else {
      cat("in user_macros\n")
      user_macro <- user_macros() %>% 
        filter(Diet_Type == selected_diet_type)
      if (nrow(user_macro) == 0) {
        cat("Selected diet type not found in user macros.\n")
        return()  # Exit function if diet type not found
      }
      protein <- user_macro$`Protein%`
      carbs <- user_macro$`Carbs%`
      fat <- user_macro$`Fat%`
    }
    
    if (!is.null(macros)) {
      updated_data <- macros %>%
        mutate(
          ConformityScore = 100 - (0.30 * abs(protein - `Protein%`) + 0.35 * abs(carbs - `Carbs%`) + 0.35 * abs(fat - `Fat%`))
        )%>%
        filter(Calories >= calorie_budget_min, Calories <= calorie_budget_max)%>%
        arrange(desc(ConformityScore)) %>%
        select(Company, Item, ConformityScore, Calories, `Protein%`, `Carbs%`, `Fat%`,`Protein(g)`, `Carbs(g)`, `TotalFat(g)`) 
      
      output$conformityTable <- renderDT({
        datatable(updated_data ) %>%
          formatRound(columns = c("Protein%", "Carbs%", "Fat%", "ConformityScore"), digits = 4) 
      })
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)