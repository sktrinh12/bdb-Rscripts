library(DT)
library(shiny)


autoFillDF <- structure(list(Name = c("ABC", "XYZ", "PQR"), Age = c(30L, 24L, 
                                                                    27L), Grade = c("A", "B", "D")), .Names = c("Name", "Age", "Grade"
                                                                    ), class = "data.frame", row.names = c(NA, -3L))

ui <- shinyUI(fluidPage(
  titlePanel("Auto Fill"),
  sidebarPanel(
    selectizeInput("p1", choices = autoFillDF$Name, selected = NULL, label = 'Name'),
    selectizeInput("p2", choices = NULL, label = 'Age'),
    selectizeInput("p3", choices = NULL, label = 'Grade')
    
  ),
  mainPanel(
    DT::dataTableOutput('table')
  )
)
)

server <- function(input, output, session) {
  
  updateApp <- reactive({
    data <- autoFillDF
    data <- data[data$Name %in% input$p1,]
    updateSelectizeInput(session, 'p2', choices = data$Age, selected = data$Age, server = TRUE)
    updateSelectizeInput(session, 'p3', choices = data$Grade, selected = data$Grade, server = TRUE)
    
    data
  })
  
  output$table <- DT::renderDataTable(
    DT::datatable(updateApp()) 
  )
  
}


shinyApp(ui, server)
