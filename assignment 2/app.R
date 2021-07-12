# square_symmetries

################################################################################
#
# Link to Published App: 
#
# Link to Demo D4 App: https://jannawithrow.shinyapps.io/square_symmetries/
#
################################################################################

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(rsconnect)

source("d3calc.R")

# The user interface
header <- dashboardHeader(title = span(HTML("D<sub>4 </sub>: According to
                                            Biggs"), 
                                       style = "font-weight: bold;
                                                font-size: 22px;"),
                          titleWidth = 310)

sidebar <- dashboardSidebar(
  width = "110px",
  btn_sidebar("btninit", "Initialize"),
  btn_sidebar("btni", "Apply i"),
  btn_sidebar("btnr", "Apply r"),
  btn_sidebar("btnr2", "Apply r^2"),
  btn_sidebar("btnr3", "Apply r^3"),
  btn_sidebar("btns", "Apply s"),
  btn_sidebar("btnrs", "Apply rs"),
  btn_sidebar("btnr2s", "Apply r2s"),
  btn_sidebar("btnr3s", "Apply r3s")
)

body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      plotOutput("configs", height = 200)
    )
  ),
  
  fluidRow(
    column(
      width = 5,
      plotOutput("square", height = 265) #square
    ),
    
    column(
      width = 7,
      dataTableOutput("multable"),
      tags$style(type = "text/css",
                 "#multable td:first-child {font-weight:bold}")
    )
  )
)

ui <- dashboardPage(title = "D4: According to Biggs", header, sidebar, body)


# Functions that read the input and modify the output and input
server <- function(session, input, output) {
  # Variables that are shared among server functions
  D4DF <- D4.makeDataFrame()
  config <- "ABCD"
  # Initialization
  output$configs <- renderPlot(D4.showConfigs(D4DF))
  output$square <- renderPlot(D4.showsquare(config))
  tbl <- outer(D4DF$name, D4DF$name, vD4.multiply, DF = D4DF)
  colnames(tbl) <- D4DF$name
  rownames(tbl) <- D4DF$name 
  # Use options to suppress the fancy controls
  output$multable <- renderDataTable(datatable(tbl, options = list(dom = "t"),
                                               escape = FALSE)
                                     %>% formatStyle(columns = c(0:8),
                                                     width='11.1%'))
  
  # Functions that respond to events in the input
  observeEvent(input$btninit, {
    config <<- "ABCD"
    output$square <- renderPlot(D4.showsquare(config))
  })
  
  observeEvent(input$btni, {
    config <<- D4.apply("i",config)
    output$square <- renderPlot(D4.showsquare(config))
  })
  
  observeEvent(input$btnr, {
    config <<- D4.apply("r",config)
    output$square <- renderPlot(D4.showsquare(config))
  })
  
  observeEvent(input$btnr2, {
    config <<- D4.apply("r2",config)
    output$square <- renderPlot(D4.showsquare(config))
  })
  
  observeEvent(input$btnr3, {
    config <<- D4.apply("r3",config)
    output$square <- renderPlot(D4.showsquare(config))
  })
  
  observeEvent(input$btns, {
    config <<- D4.apply("s",config)
    output$square <- renderPlot(D4.showsquare(config))
  })
  
  observeEvent(input$btnrs, {
    config <<- D4.apply("rs",config)
    output$square <- renderPlot(D4.showsquare(config))
  })
  
  observeEvent(input$btnr2s, {
    config <<- D4.apply("r2s", config)
    output$square <- renderPlot(D4.showsquare(config))
  })
  
  observeEvent(input$btnr3s, {
    config <<- D4.apply("r3s", config)
    output$square <- renderPlot(D4.showsquare(config))
  })
}

# Run the app.
shinyApp(ui = ui, server = server)
