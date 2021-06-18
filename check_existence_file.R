library(shiny)
library(shinyFiles)

ui <- fluidPage(
	p("Status ", textOutput("status", inline=TRUE))
	p(shinyDirButton("basedirChooser","Base dir","Base dir")),
	p("Selected base dir ", textOutput("basedir", inline=TRUE)),
)

server <- function(input, output) {
# manage and to display the status
status <- reactiveVal("Waiting for base dir")
output$status <- renderText(status())

# to manage the Dir Chooser
roots <- c(home="~")
shinyDirChoose(input, "basedirChooser", roots=roots)

# to manage the selected directory the reactive way
# if no path selected --> NULL
basedir <- reactive({
path <- parseDirPath(roots, input$basedirChooser)
if(length(path))
path
})

# to display the selected path
output$basedir <- renderText(basedir())

# to handle the post process in case we have a basedir
observeEvent(basedir(), {
# say we are running from now
status("Running")

# do POST here using basedir() as selected directory
})

# to manage the reception of the result
observe({
# if we are in a running state
if(status()=="Running") {
# check presence and size of file here
filename <- file.path(basedir(), "dummy.pdf")
if(file.exists(filename) && (file.size(filename)>5000))
# Good we got result
status("Finished")
else
# otherwise let's recheck a bit later (here 2000ms=2s)
invalidateLater(2000)
}
})
}

shinyApp(ui, server)
