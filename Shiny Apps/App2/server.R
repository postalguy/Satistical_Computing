
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
options(shiny.maxRequestSize = 1024*1024^2)
shinyServer(function(input, output) {
    dataSet <- reactive({
   inFile <- input$file1
   #importing data from file input
   if(!is.null(inFile))
   isolate(dataSet <- read.csv(inFile$datapath, header = input$header,sep = input$sep, quote = input$quote,comment.char="", stringsAsFactors=FALSE)) 
 
   })
    output$contents <- renderDataTable(dataSet)

   
 
})

