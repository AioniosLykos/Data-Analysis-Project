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
library(magrittr)
library(shiny)
library(shinyBS)
library(shinyWidgets)


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
  select("Company" , "Item","TotalCalories_S", "Protein%", "Carbs%", "Fat%","Protein(g)", "TotalFat(g)","Carbs(g)", "Sugars(g)", "Fiber(g)", "Cholesterol(mg)") %>%
  rename( "Calories" = "TotalCalories_S")

macros$`Protein(g)` <- as.numeric(macros$`Protein(g)`)
macros$`Fiber(g)` <- as.numeric(macros$`Fiber(g)`)
macros$`Sugars(g)` <- as.numeric(macros$`Sugars(g)`)
macros$`Cholesterol(mg)` <- as.numeric(macros$`Cholesterol(mg)`)



#Define UI 
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
      width = 6,
      
      tabsetPanel(
        tabPanel("Base parameters", 
                 selectInput("diet_type", "Select Diet Type:",
                             choices = c("Low_Carb", "Low_Fat", "Balanced1", "Balanced2", "High_Carb"),
                             selected = "Low_Carb"),
                 
                 # Two-sided interval for calorie budget
                 sliderInput("calorie_budget", "Calorie Budget:",
                             min = 0, max = 1500, value = c(300, 700),
                             step = 25, sep = "")
        ),
        tabPanel("Advanced Parameters",
                 h3("Advanced Options for Tailored Results"),
                 div(
                   style = "display: flex; justify-content: right;",  # Center align the content horizontally
                   div(
                     style = "margin-right: 0px;",  # Adjust margin-right to create space
                     actionButton("reset", "Reset Parameters")
                   )
                 ),
                 h4("The following parameters are used in Conformity Score calculations and must sum up to 1."),
                 tags$div(
                   style = "font-size: 10px;",  # Adjust the font size as needed
                   h5("While macros mainly determine the conformity of a food item, these parameters let you favor one or more macros over others.")
                 ),
                 fluidRow(
                   column(4,
                          numericInput("protein_constant", HTML("Protein Constant: <i class='fas fa-question-circle' data-toggle='tooltip' title='The closer this constant is to 0, the more you prioritize calories from proteins in your diet.'></i>"), value = 0.30, min = 0, max = 1, step = 0.01, width = "140%")
                   ),
                   column(4,
                          numericInput("carbs_constant", HTML("Carbs Constant: <i class='fas fa-question-circle' data-toggle='tooltip' title='The closer this constant is to 0, the more you prioritize calories from carbs in your diet.'></i>"), value = 0.35, min = 0, max = 1, step = 0.01, width = "140%")
                   ),
                   column(4,
                          numericInput("fat_constant", HTML("Fat Constant: <i class='fas fa-question-circle' data-toggle='tooltip' title='The closer this constant is to 0, the more you prioritize calories from fat in your diet.'></i>"), value = 0.35, min = 0, max = 1, step = 0.01, width = "140%")
                   )
                 ),
                 
                 # Optional advanced parameters for fiber, protein, sugar, and cholesterol
                 h4( HTML("Specify filter choices <i class='fas fa-question-circle' data-toggle='tooltip' title=' The intervals are closed meaning both endpoints are included.\n If only one of the endpoints has an input, the filter will show values > min  or values < max for that parameter.'></i>"), value = 0.30, min = 0, max = 1, step = 0.01, width = "140%" ),
                 
                 fluidRow(
                   column(6, numericInput("protein_min", "Minimum Protein Intake(g):", NA, min = 0, step = 1)),
                   column(6, numericInput("protein_max", "Maximum Protein Intake(g):",  NA, min = 0, step = 1)),
                 ), 
                 fluidRow(
                   column(6, numericInput("fiber_min", "Minimum Fiber Intake(g):", NA, min = 0, step = 1)),
                   column(6, numericInput("fiber_max", "Minimum Fiber Intake(g):",  NA, min = 0, step = 1))
                 ),
                 fluidRow(
                   column(6, numericInput("sugar_min", "Minimum Sugar Intake(g):", NA, min = 0, step = 1)),
                   column(6, numericInput("sugar_max", "Maximum Sugar Intake(g):",  NA, min = 0, step = 1)),
                 ),
                 fluidRow(
                   column(6, numericInput("cholesterol_min", "Minimum Cholesterol Intake(mg):", NA, min = 0, step = 1)),
                   column(6, numericInput("cholesterol_max", "Minimum Cholesterol Intake(mg):",  NA, min = 0, step = 1))
                 ),
                 sliderTextInput(
                   "conformity_range",
                   "Show Items with Conformity Score within",
                   choices = seq(from = 100, to = 0, by = -0.1),
                   selected = 90,
                   width = "100%",
                   post = ""
                 )  
                 
        )
      ), 
      
      actionButton("confirmChoices", "Confirm My Choices")
      
    ),
    mainPanel(
      width = 6,
      tabsetPanel(
        tabPanel("Diet",
                 tabsetPanel(
                   tabPanel("Preselected Macros",
                            htmlOutput("title1"),
                            DTOutput("dietTable")
                   ),
                   tabPanel("Personalized Data Entry",
                            h3("Enter your macro data"),
                            textInput("macro_Diet_Type", "Diet Name:", ""),
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
                                actionButton("delete_macro", "Delete Diet")
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
                     )
                     
                   )
        ), tabPanel("Analysis",
                    tabsetPanel(
                      tabPanel("Conformity Score Table",
                               DTOutput("conformityTable"),
                               h3("Parameter Dependent Table"),
                               h5("The updated table will appear once the choices are confirmed.")
                      ),
                      tabPanel("Graphs",
                               plotOutput("plotConformity", height = "500px", width = "650px")
                        
                      )
                    )
        )
        
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # ReactiveValues to store advanced parameters
  advanced_params <- reactiveValues(
    fiber = list(value = NA, comparison = "<"),
    protein = list(value = NA, comparison = "<"),
    sugar = list(value = NA, comparison = "<"),
    cholesterol = list(value = NA, comparison = "<")
  )
  
  
  
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
  user_macros <- reactiveVal(data.frame(
    "Diet_Type" = character(), 
    "Protein_Percent" = numeric(), 
    "Carbs_Percent" = numeric(), 
    "Fat_Percent" = numeric(), 
    stringsAsFactors = FALSE
  ))
  
  
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
      if (input$macro_Diet_Type %in% user_macros()$Diet_Type || input$macro_Diet_Type %in% diet_types()) {
        output$validation_message <- renderText({ "This Diet_Type is already used or it's a predefined diet type. Please choose a different Diet_Type." })
        return()
      } else {
        output$validation_message <- renderText({ "" })
      }
      
      new_macro <- data.frame(
        "Diet_Type" = input$macro_Diet_Type,
        "Protein_Percent" = input$protein_percent,
        "Carbs_Percent" = input$carbs_percent,
        "Fat_Percent" = input$fat_percent,
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
    ), colnames = c("Diet Type", "Protein (%)", "Carbs (%)", "Fat (%)"))
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
    # Check and reset constants if necessary (similar to your existing code)
    
    selected_diet_type <- input$diet_type
    calorie_budget_min <- input$calorie_budget[1]
    calorie_budget_max <- input$calorie_budget[2]
    
    advanced_params$fiber$value <- input$preferred_fiber
    advanced_params$fiber$comparison <- input$fiber_max
    advanced_params$protein$value <- input$preferred_protein
    advanced_params$protein$comparison <- input$protein_max
    advanced_params$sugar$value <- input$preferred_sugar
    advanced_params$sugar$comparison <- input$sugar_max
    advanced_params$cholesterol$value <- input$preferred_cholesterol
    advanced_params$cholesterol$comparison <- input$cholesterol_max
    
    if (!(selected_diet_type %in% user_macros()$Diet_Type)) {
      diet_data <- dietTable() %>%
        filter(Diet_Type == selected_diet_type)
      if (nrow(diet_data) == 0) {
        return()
      }
      protein <- diet_data$`Protein %`
      carbs <- diet_data$`Carbs %`
      fat <- diet_data$`Fat %`
    } else {
      user_macro <- user_macros() %>%
        filter(Diet_Type == selected_diet_type)
      if (nrow(user_macro) == 0) {
        return()
      }
      protein <- user_macro$`Protein_Percent`
      carbs <- user_macro$`Carbs_Percent`
      fat <- user_macro$`Fat_Percent`
    }
    
    if (!is.null(macros)) {
      # Apply optional thresholds
      updated_data <- macros %>%
        mutate(
          ConformityScore = 100 - (
            input$protein_constant * abs(protein - `Protein%`) +
              input$carbs_constant * abs(carbs - `Carbs%`) +
              input$fat_constant * abs(fat - `Fat%`)
          )
        ) %>%
        filter(
          # Filter rows based on protein_max and protein_min
          if (!is.na(input$protein_min) && is.na(input$protein_max)) {
            (`Protein(g)` >= input$protein_min) 
          } else {
            if (is.na(input$protein_min) && !is.na(input$protein_max)) {
              (`Protein(g)` <= input$protein_max) 
            } else {
              if (!is.na(input$protein_min) && !is.na(input$protein_max)) {
                ((`Protein(g)` >= input$protein_min) & (`Protein(g)` <= input$protein_max))
              }else {
                TRUE  # Include all rows if protein_min and protein_max are NA
              }
            } 
          },
          # Filter rows based on fiber_max and fiber_min
          if (!is.na(input$fiber_min) && is.na(input$fiber_max)) {
            (`Fiber(g)` >= input$fiber_min) 
          } else {
            if (is.na(input$fiber_min) && !is.na(input$fiber_max)) {
              (`Fiber(g)` <= input$fiber_max) 
            } else {
              if (!is.na(input$fiber_min) && !is.na(input$fiber_max)) {
                ((`Fiber(g)` >= input$fiber_min) & (`Fiber(g)` <= input$fiber_max))
              }else {
                TRUE  # Include all rows if protein_min and protein_max are NA
              }
            } 
          },
          # Filter rows based on sugar_max and sugar_min
          if (!is.na(input$sugar_min) && is.na(input$sugar_max)) {
            (`Sugars(g)` >= input$sugar_min) 
          } else {
            if (is.na(input$sugar_min) && !is.na(input$sugar_max)) {
              (`Sugars(g)` <= input$sugar_max) 
            } else {
              if (!is.na(input$sugar_min) && !is.na(input$sugar_max)) {
                ((`Sugars(g)` >= input$sugar_min) & (`Sugars(g)` <= input$sugar_max))
              }else {
                TRUE  # Include all rows if protein_min and protein_max are NA
              }
            } 
          },
          # Filter rows based on cholesterol_max and cholesterol_min
          if (!is.na(input$cholesterol_min) && is.na(input$cholesterol_max)) {
            (`Cholesterol(mg)` >= input$cholesterol_min) 
          } else {
            if (is.na(input$cholesterol_min) && !is.na(input$cholesterol_max)) {
              (`Cholesterol(mg)` <= input$cholesterol_max) 
            } else {
              if (!is.na(input$cholesterol_min) && !is.na(input$cholesterol_max)) {
                ((`Cholesterol(mg)` >= input$cholesterol_min) & (`Cholesterol(mg)` <= input$cholesterol_max))
              }else {
                TRUE  # Include all rows if protein_min and protein_max are NA
              }
            } 
          },
          # Filter rows based on calorie budget
          Calories >= calorie_budget_min,
          Calories <= calorie_budget_max
        )      %>%
        arrange(desc(ConformityScore)) %>%
        select(Company, Item, ConformityScore, Calories, `Protein%`, `Carbs%`, `Fat%`, `Protein(g)`, `Carbs(g)`, `TotalFat(g)`, `Sugars(g)`, `Fiber(g)`, `Cholesterol(mg)`)
      
      output$conformityTable <- renderDT({
        datatable(updated_data) %>%
          formatRound(columns = c("Protein%", "Carbs%", "Fat%", "ConformityScore"), digits = 4)
      })
      
      output$plotConformity <- renderPlot({
        
     plotTable <- updated_data %>% filter(ConformityScore >= 90 ) %>% count(Company) %>% mutate(MenuC = menu_counts[Company])
     
     ggplot(plotTable, aes(x = Company, y = n, fill = Company)) +
       geom_bar(stat = "identity") +
       geom_text(aes(label = paste0(round((n / MenuC) * 100, 2), "% of \nits whole menu")), 
                 vjust = 0.5, size = 13/.pt) +
       labs(x = "Company",
            y = "Count") +
       theme_classic() +
       theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 14, face= "italic"),
             axis.text.y = element_text(size = 14,face = "italic"),
             axis.title = element_text(size = 12, face="bold"),
             plot.title = element_text(face = "bold", size = 18),
             legend.title = element_text(size = 14, face="bold"),
             legend.text = element_text(size = 13, face="italic")) +  
       ggtitle(paste("Best Conforming,>=90, Item Count by each Company to ", input$diet_type, " Diet"))
      })
        
      
      
      
    }
    
    if(!(input$protein_constant + input$carbs_constant +input$fat_constant ==1)){
      updateNumericInput(session, "protein_constant", value = 0.3)
      updateNumericInput(session, "carbs_constant", value = 0.35)
      updateNumericInput(session, "fat_constant", value = 0.35)
      
      ###############################Continuee
    }
    
    
    
    
  })
  
  
  observeEvent(input$reset, {
    updateNumericInput(session, "protein_constant", value = 0.3)
    updateNumericInput(session, "carbs_constant", value = 0.35)
    updateNumericInput(session, "fat_constant", value = 0.35)
    
    updateNumericInput(session, "protein_min", value = NA)
    updateNumericInput(session, "fiber_min", value = NA)
    updateNumericInput(session, "sugar_min", value = NA)
    updateNumericInput(session, "cholesterol_min", value = NA)
    
    updateNumericInput(session, "protein_max", value = NA)
    updateNumericInput(session, "fiber_max", value = NA)
    updateNumericInput(session, "sugar_max", value = NA)
    updateNumericInput(session, "cholesterol_max", value = NA)
    updateSliderTextInput(session, "conformity_range", selected = 90)
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)