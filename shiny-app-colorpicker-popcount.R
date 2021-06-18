library(shiny)
library(shinydashboard)
library(shinyFiles)
library(colourpicker)

# Layout Variables
COLUMN_ID <- 1
OFFSET_ID <- 1
OMIQ_COLUMN <- 2
stats_file <- "C:/Users/10322096/Documents/gitrepos/romiq-meta-docker-setup/common_drive/reports/20210701SDFY21W3P7/all_stats.csv"


ui <- dashboardPage(skin='green',
                    
                    dashboardHeader(title='Create Metadata CSV File'),
                    dashboardSidebar(
                                     sidebarMenu(
                                       menuItem("User Inputs", tabName="userInputs", icon=icon("pen")),
                                       menuItem("Configure Plate Layout", tabName='plateConfig', icon=icon('th')),
                                       menuItem("Preview Metadata Table", tabName="outputs", icon=icon("table")),
                                       menuItem("OMIQ setup", tabName="omiq_setup", icon=icon("cog", lib= "glyphicon"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        ####### User Inputs Tab #######
                        tabItem(tabName='userInputs',
                                
                                fluidRow(
                                  box(shinyDirButton("filePath", "FCS File Directory", ""),
                                      textInput("plateID", "Plate ID*", value="20191218-10299178-01"),
                                      # textInput("plateID", "Plate ID*", placeholder="YYYYMMDD-EMPID-XX"),
                                      # textInput("stainID", "Stain ID*", placeholder="YYYYMMDD-EMPID"),
                                      textInput("stainID", "Stain ID*", value="20191218-10299178"),
                                      selectizeInput("sampleType", "Sample Type*",
                                                     choices = NULL,
                                                     options=list(create=TRUE)),
                                      # textInput("donorID", "Donor ID", value = 'NA'),
                                      textInput("donorID", "Donor ID", value = 'DN22'),
                                      selectizeInput("sampleSpecies", "Sample Species*",
                                                     choices = NULL,
                                                     options=list(create=TRUE)),
                                      textInput("sampleStrain", "Sample Strain", value = "NA"),
                                      selectizeInput("cytometer", "Cytometer*",
                                                     choices = NULL,
                                                     options=list(create=TRUE)),
                                      p(em(h6('* indicates required field')))
                                  ))),
                      
                        tabItem(tabName='omiq_setup',
                                
                                # button to push to R-OMIQ
                                actionButton('pushData', 'Push Metadata to OMIQ'),
                                actionButton('rerun', 'Re-run'),
                                uiOutput('uiStatus'),
                                
                                fluidRow(
                                  column(OMIQ_COLUMN, textInput('Pop1', 'Pop1', value='Lymph')),
                                  column(OMIQ_COLUMN, textInput('Pop2', 'Pop2', value='Mono')),
                                  column(OMIQ_COLUMN, textInput('Pop3', 'Pop3', value='Gran')),
                                  width=12),
                                fluidRow(
                                  column(OMIQ_COLUMN, numericInput('PopCount1', 'PopCount1', value=2000, min=100, max=5000, step=100)),
                                  column(OMIQ_COLUMN, numericInput('PopCount2', 'PopCount2', value=300, min=10, max=3000, step=100)),
                                  column(OMIQ_COLUMN, numericInput('PopCount3', 'PopCount3', value=1500, min=100, max=10000, step=100)),
                                  width=12),
                                fluidRow(
                                  column(OMIQ_COLUMN, colourInput("col1", "Pop1 colour", "#fc1828ff")),
                                  column(OMIQ_COLUMN, colourInput("col2", "Pop2 colour", "#fd8628ff")),
                                  column(OMIQ_COLUMN, colourInput("col3", "Pop3 colour", "#2983ffff")),
                                  width=12),
                                hr(),
                                sliderInput("biexSlider", label = h3("biex value"), min = -1000, max = -1, value = -300)
                        )
                      )
                      
                    ))


server <- function(input, output, session) {
  basedir <- 'testing'
  omiq_status <- reactiveValues(text = 'Idle')
  output$uiStatus <- renderUI(
    h4(paste0('STATUS: ', omiq_status$text), style="color:blue;")
  )
  observeEvent(input$rerun, {
      basedir <- basename(parseDirPath(c(home=output_dir), input$filePath))
      print(paste(replicate(20,"="),collapse=""))
      rdn_url <- paste0("romiq", as.character(sample(1:2, 1)), URL)
      rdn_url <- paste0(rdn_url, scriptName, "/", basedir)
      print(paste0("basedir: ", basedir, "; hostname: ", rdn_url))
      omiq_status$text <- "Running..."
      print(paste('colours:', input$col1, input$col2, input$col3, sep= ' '))
  })
  
  
  observe({
    # if the modified time is 20 seconds from now
    filename <- file.path(stats_file)
    mod_time <- file.info(filename)$mtime
    time_diff <- abs(difftime(Sys.time(), mod_time, units = "secs"))
    if(file.exists(filename) && time_diff < 22) {
      omiq_status$text <- paste0("OMIQ pipeline re-run completed for ", basedir)
    }
    else {
      invalidateLater(15000)
    }
  })
}

shinyApp(ui, server)
