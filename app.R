library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(psych)

# Load and clean data ----------------------------------------------
MyDat <- read.csv("kidney_disease.csv")   #maybe change this to load for simplicity 
#   mutate(CKD_Diagnosis = as.character(CKD_Diagnosis),
#          pc = as.character(pc),
#          rbc = as.character(rbc),
#          pcc = as.character(pcc),
#          ba = as.character(ba),
#          htn =as.character(htn),
#          dm	= as.character(dm),
#          cad= as.character(cad),
#          appet= as.character(appet),
#          pe	=as.character(pe),
#          ane = as.character(ane),
#          id = as.factor(id))
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
    
    sliderInput("age",
                "Slect age range:",
                min = min(MyDat$age, na.rm = T),
                max = max(MyDat$age, na.rm =T),
                value = c(min(MyDat$age,na.rm =T), max(MyDat$age, na.rm =T)),
                step =1)
    
    
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("TP"),
            infoBoxOutput("CKD")
            
          ),
          fluidRow(
            infoBoxOutput("Age"),
            valueBoxOutput("BP")
            
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("CKD Count", plotlyOutput("plot_CKD")),
                   tabPanel("Age", plotlyOutput("plot_age")),
                   tabPanel("BP", plotlyOutput("plot_BP"))
            )    
                   
            )
          ),
          
          # Data Table Page ----------------------------------------------
          tabItem("table",
                  fluidPage(
                    box(title = "Summary Statistics for CKD Dataset", DT::dataTableOutput("table"), width = 12))
          )
  
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
    MyDat <- MyDat %>%
    # Slider Filter ----------------------------------------------
    filter(age >= input$age[1] & age <= input$age[2])
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
  
  
  # A plot showing the different in BP levels for patients with and wihout CKD -----------------------------------
  
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
  
  
  
  output$table <- DT::renderDataTable({ d <- describe(swInput()[ , c('age','blood_pressure','Albumin','Blood_sugar','Hemoglobin')] ,fast=TRUE) 
  })
  
  
  # CKD patients count value box ----------------------------------------------
  output$CKD <- renderValueBox({
    sw <- swInput()
    num <- length(which(sw$CKD_Diagnosis=="ckd"))
    
    valueBox(subtitle = "Number of patients with CKD diagnosis", value = num, icon = icon("sort-numeric-asc"), color = "green")
    
  })
  
  # Mass mean info box ----------------------------------------------
  output$Age <- renderInfoBox({
    sw <- swInput()
    num <- round(mean(sw$age, na.rm = T), 2)
    
    infoBox("Avg Age", value = num, icon = icon("balance-scale"), color = "purple")
  })
  
  # Height mean value box ----------------------------------------------
  output$BP <- renderValueBox({
    sw <- swInput()
    num <- round(mean(sw$blood_pressure, na.rm = T), 2)
    
    valueBox(subtitle = "Avg Blood Pressure", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
  
  #Total number of Patients 
  output$TP <- renderValueBox({
    sw <- swInput()
    num <- length(unique(sw$id))
    
    valueBox(subtitle = "Total Number of Patients", value = num, icon = icon("sort-numeric-asc"), color = "green")
  })
  
  
  
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)
