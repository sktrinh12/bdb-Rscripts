library(shiny)

VM = "10.29.128.4"

curl_api <- function(input_string) {
  link <- paste0("http://", VM, ":8000/api/v1/dags/r_dag/dagRuns/", input_string)
  message(link)
  res <- httr::GET(url = link,
                   config = authenticate("airflow", "airflow"),
                   httr::add_headers(`accept` = 'application/json'), 
                   httr::content_type('application/json'))
  
  res <- content(res, "parsed")
  paste("the api output:", res$state)
}


ui <- fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
               information from the 2010 US Census."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("Percent White", 
                              "Percent Black",
                              "Percent Hispanic", 
                              "Percent Asian"),
                  selected = "Percent White"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0, 100)),
      
      textInput("api", "dag id:", "testrun14"),
      submitButton("Update", icon("refresh")),
    ),
    
    mainPanel(
      textOutput("selected_var"),
      textOutput("selected_range"),
      textOutput("output_api")
    )
  )
)


server <- function(input, output) {
  
  output$selected_var <- renderText({ 
    paste("You have selected var:", input$var)
  })
  output$selected_range <- renderText({ 
    paste("You have selected range:", input$range[1], "to", input$range[2])
  })
  output$output_api <- renderText({ 
      curl_api(input$api)
    })
  
}

shinyApp(ui, server)