library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(psych)

# Loading diabetes dataset ----------------------------------------------
MyDat <- read.csv("kidney_disease.csv")   #maybe change this to load for simplicity 

# Avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application header & title ----------------------------------------------
header <- dashboardHeader(title = "Chronic Kidney Disease Data",
                          
                          dropdownMenu(type = "notifications",
                                       notificationItem(text = "New Dataset recieved from Hospital",
                                                        icon = icon("users"))
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 100, 
                                                color = "green",
                                                "CKD dashboard Created")
                          ),
                          dropdownMenu(type = "messages",
                                       messageItem(from = "Kindey Practice India",
                                                   message = HTML("New dataset recieved on Jan 01 2022"),
                                                   icon = icon("exclamation-circle"))
                          )
)
# Dashboard Sidebar ----------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    
    # Menu Items ----------------------------------------------
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green"),
    
    #slider for age seleciton
    sliderInput("age",
                "Slect age range:",
                min = min(MyDat$age, na.rm = T),
                max = max(MyDat$age, na.rm =T),
                value = c(min(MyDat$age,na.rm =T), max(MyDat$age, na.rm =T)),
                step =1),
    
   # radio button for Hypertension diagnosis
    radioButtons("hyp", "Has Hypertension?", choices = c("yes","no"), selected = c("no")),
    
   
   # radio button for Diabetes Mellitus diagnosis
    radioButtons("dia", "Has Diabetes Mellitus?", choices = c("yes","no"), selected = c("no"))
    
    
    
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes for Total Patient count and CKD patient count ----------------------------------------------
          fluidRow(
            infoBoxOutput("TP"),
            infoBoxOutput("CKD"),
            valueBoxOutput("hemo")
            
          # Input and Value Boxes for Age and Blood Pressure 
          ),
          fluidRow(
            infoBoxOutput("Age"),
            valueBoxOutput("BP"),
            valueBoxOutput("Sugar")
            
          ),
          
          # Plot for CKD counts, Age by CKD diagnosis and Albumin levels,Blood Pressure  ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("CKD Count", plotlyOutput("plot_CKD")),
                   tabPanel("Age", plotlyOutput("plot_age")),
                   tabPanel("BP", plotlyOutput("plot_BP"))
            )    
                   
            )
          ),
          
          # Data Table Page for Summaryy Statistics ----------------------------------------------
          tabItem("table",
                  fluidPage(
                    box(title = "Summary Statistics for CKD Dataset", DT::dataTableOutput("table"), width = 12))
          )
  
)
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
    MyDat <- MyDat %>%
    # Slider Filter for age ----------------------------------------------
    filter(age >= input$age[1] & age <= input$age[2])
    
    # Hypertension Filter ----------------------------------------------
    if (length(input$hyp) > 0 ) {
      MyDat <- subset(MyDat, Hypertension %in% input$hyp)
    }
    
    # Diabetes Filter ----------------------------------------------
    if (length(input$dia) > 0 ) {
      MyDat <- subset(MyDat, Diabetes_mellitus %in% input$dia)
    }
    
    # Return dataframe ----------------------------------------------
    return(MyDat)

  })
  
  
  # Count of CKD Diagnosis patients bar chart
  output$plot_CKD <- renderPlotly({
    barchart <- swInput() %>%
      group_by(CKD_Diagnosis) %>%
      summarize('count' = n()) %>%
      ggplot(aes(x = CKD_Diagnosis, 
                 y = count, 
                 fill = CKD_Diagnosis
      ))  + 
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set3")+ 
      coord_flip()+ 
      labs(x = "CKD diagnosis", 
           y = "Count", 
           title = "Number of patients with CKD") 
    
    ggplotly(barchart, tooltip = "y")
  })  
  
  # Correlation between CKD,Age,Albumin Levels scatter plots
  output$plot_age <- renderPlotly({
    scatter <- swInput()%>%
      group_by(CKD_Diagnosis)%>% 
      ggplot( aes(x = age,
                  y = Albumin,
      )) +
      geom_point(aes(color = factor(CKD_Diagnosis),
                     size = 3))+ 
      scale_color_brewer(palette = "Set3")+ 
      labs(x = "Age", 
           y = "Albumin levels", 
           title = "Correlation between CKD,Albumnin Levels,and Age ")
    
    ggplotly(scatter, tooltip = "text")
  })
  
  
  # A plot showing the different in BP levels for patients with and wihout CKD 
  
  output$plot_BP  <- renderPlotly({
    swInput() %>%
      group_by(CKD_Diagnosis) %>%
      ggplot(aes(x = CKD_Diagnosis, 
                 y = blood_pressure,
                 fill = CKD_Diagnosis,))+
      geom_violin() + 
      scale_fill_brewer(palette = "Set3")+ 
      labs(x = "CKD diagnosis", 
           y = "Blood Pressure Levels", 
           title = "CKD and BP Levels") 
  })
  
  
  #Datatabe showing summary statistics for some Key variables 
  
  output$table <- DT::renderDataTable({ d <- describe(swInput()[ , c('age','blood_pressure','Albumin','Blood_sugar','Hemoglobin')] ,fast=TRUE) 
  })
  
  
  # CKD patients count value box ----------------------------------------------
  output$CKD <- renderValueBox({
    sw <- swInput()
    num <- length(which(sw$CKD_Diagnosis=="ckd"))
    
    valueBox(subtitle = "Number of patients with CKD diagnosis", value = num, icon = icon("sort-numeric-asc"), color = "green")
    
  })
  
  # Mean age info box ----------------------------------------------
  output$Age <- renderInfoBox({
    sw <- swInput()
    num <- round(mean(sw$age, na.rm = T), 2)
    
    infoBox("Avg Age", value = num, icon = icon("balance-scale"), color = "purple")
  })
  
  # Mean Blood Pressure value box ----------------------------------------------
  output$BP <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$blood_pressure, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Blood Pressure", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
  
  # Mean Sugal levels value box ----------------------------------------------
  output$Sugar <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$Sugar, na.rm = T), 2)
    
    valueBox(subtitle = "Average Sugar Levels", value = num, icon = icon("sort-numeric-asc"), color = "red")
  })
  
  # Mean Hemoglobin levels value box ----------------------------------------------
  output$hemo <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$Hemoglobin, na.rm = T), 2)
    
    valueBox(subtitle = "Average Hemoglobin Levels", value = num, icon = icon("sort-numeric-asc"), color = "red")
  })
  
  
  #Total number of Patients value box
  output$TP <- renderValueBox({
    sw <- swInput()
    num <- length(unique(sw$id))
    
    valueBox(subtitle = "Total Number of Patients", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
  
  
  
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
