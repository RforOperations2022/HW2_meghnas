library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)

# Load and clean data ----------------------------------------------
MyDat <- read.csv("kidney_disease.csv")  %>%
  mutate(classification = as.character(classification),
         pc = as.character(pc),
         rbc = as.character(rbc),
         pcc = as.character(pcc),
         ba = as.character(ba),
         htn =as.character(htn),
         dm	= as.character(dm),
         cad= as.character(cad),
         appet= as.character(appet),
         pe	=as.character(pe),
         ane = as.character(ane),
         id = as.factor(id))
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
    menuItem("Table", icon = icon("table"), tabName = "table", badgeLabel = "new", badgeColor = "green")
    
    # # Inputs: select variables to plot ----------------------------------------------
    # selectInput("Select",
    #             "Variable:",
    #             choices = sort(unique(starwars.load$homeworld)),
    #             multiple = TRUE,
    #             selectize = TRUE,
    #             selected = c("Naboo", "Tatooine")),
    
    # # Birth year Selection ----------------------------------------------
    # sliderInput("birthSelect",
    #             "Birth Year:",
    #             min = min(starwars.load$birth_year, na.rm = T),
    #             max = max(starwars.load$birth_year, na.rm = T),
    #             value = c(min(starwars.load$birth_year, na.rm = T), max(starwars.load$birth_year, na.rm = T)),
    #             step = 1)
  )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(tabItems(
  
  # Plot page ----------------------------------------------
  tabItem("plot",
          
          # Input and Value Boxes ----------------------------------------------
          fluidRow(
            infoBoxOutput("TP"),
            infoBoxOutput("CKD"),
            infoBoxOutput("Age"),
            valueBoxOutput("BP"),
            
          ),
          
          # Plot ----------------------------------------------
          fluidRow(
            tabBox(title = "Plot",
                   width = 12,
                   tabPanel("Age", plotlyOutput("plot_age")),
                   tabPanel("BP", plotlyOutput("plot_BP")),
                   tabPanel("CKD Count", plotlyOutput("plot_CKD"))
                   
            )
          ),
          
          # Data Table Page ----------------------------------------------
          tabItem("table",
                  fluidPage(
                    box(title = "CKD Data", DT::dataTableOutput("table"), width = 12))
          )
  )
)
)

ui <- dashboardPage(header, sidebar, body)

# Define server function required to create plots and value boxes -----
# Define server function required to create plots and value boxes -----
server <- function(input, output) {
  
  # Reactive data function -------------------------------------------
  swInput <- reactive({
    MyDat <- MyDat 
  })
  # Reactive melted data ----------------------------------------------
  mwInput <- reactive({
    swInput() %>%
      melt(id = "classification")
  })
  
  # # A plot showing the mass of characters -----------------------------
  # output$plot_age <- renderPlotly({
  #   dat <- subset(mwInput(), variable == "age")
  #   
  #   # Generate Plot ----------------------------------------------
  #   ggplot(data = dat, aes(x = "age", y = "classification", fill = classification)) + geom_bar(stat = "identity")
  # })
  
  
  # age vs albumin scatter plot
  output$plot_age <- renderPlotly({
    ggplot(data =swInput(), aes_string(x = "age", y = "al")) + geom_point()+
     facet_wrap(~classification)
  })
  
  # albumin levels by CKD diagnosis
  
  output$plot_CKD <- renderPlotly({
    ggplot(data =swInput(), aes_string(x = "classification")) + geom_bar()
  })
  
  
  # A plot showing the BP of Patients -----------------------------------
  output$plot_BP <- renderPlotly({
    ggplot(swInput(), aes(x = bp)) +
      geom_histogram(colour = "white", fill = "peachpuff", bins = 50) +
      labs(x = "Blood Pressure") +
      facet_wrap(~classification)
  }) 
  
  # Data table of characters ----------------------------------------------
  output$table <- DT::renderDataTable({
    subset(swInput(), select = c(id,age,classification,bp))
  })
  
  # CKD patients count value box ----------------------------------------------
  output$CKD <- renderValueBox({
    sw <- swInput()
    num <- length(which(sw$classification=="ckd"))
    
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
    num <- round(mean(sw$bp, na.rm = T), 2)
    
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
