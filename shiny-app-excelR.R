library(excelR)
library(shiny)

shinyApp(
  
  ui = fluidPage( tags$h6("Selected Data:"),
                  tableOutput("selectedData"),
                  tags$h6("Excel Table:"),
                  excelOutput("table", height = 175)),
  
  server = function(input, output, session) {
    
    output$table <- renderExcel(excelTable(data = head(iris), getSelectedData = TRUE))
    
    # Print the selected data in table
    observeEvent(input$table,{
      output$selectedData <- renderTable(get_selected_data(input$table))
    })
  }
)