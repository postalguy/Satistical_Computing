library(shiny)
library(ggplot2)
# use the below options code if you wish to increase the file input limit, in this example file input limit is increased from 5MB to 9MB
options(shiny.maxRequestSize = 1024*1024^2)

shinyServer(function(input,output){
  ######################################################################################
  ############################ Input File and Data View ################################
  ######################################################################################
  
  
  
  #returns the dataset in the form of a dataframe.
  # file$datapath ->  path of the file
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    read.csv(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors,quote = input$quote)
    
  })
  
  # output contains the summary of the dataset and display the summary in table format
  output$filedf <- renderTable({
    if(is.null(data())){return ()}
    input$file
  })
  
  # Summary of the dataset in table format
  output$sum <- renderTable({
    if(is.null(data())){return ()}
    summary(data())
    
  })
  
  # This reactive output contains the dataset and display the dataset in table format
  output$table <- renderDataTable({
    if(is.null(data())){return ()}
    data()
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
      h3("Waiting for file...")
    else
      tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data", dataTableOutput("table")),tabPanel("Summary", tableOutput("sum")))
  })
  
  #################################################################################
  ################################# PLOTING SECTION ###############################
  #################################################################################
  
  # the following renders dynamicaly the plot x & y selectors from data
  output$plotselection <- renderUI({
    if(is.null(data())){return ()}
    
    sidebarPanel(
      #for x 
      selectInput("X","x",choices <- names(data()),selected = choices[2]),
      uiOutput("Sliderx"),
      #for y
      selectInput("Y","y",choices <- names(data()),selected = choices[3]),
      uiOutput("Slidery")
      )
  })
  
  # reactive function to get max from X selection & add a slider for it
  output$Sliderx <- renderUI({
     # tests if the input has numerical or factor values
    if(is.factor(input$X)){ h3("Factor Value") }
    else{ 
      sliderInput("SliderX", "Values of X", min = 0, max = max(data()[as.character(input$X)],na.rm = FALSE) , value = c(25, 75))}
   
  })
  # reactive function to get max from selection & add a slider for it
  output$Slidery <- renderUI({
    if(is.factor(input$X)){h3("Factor Value") }
    else {
    sliderInput("SliderY", "Values of Y", min = 0, max = max(data()[as.character(input$Y)],na.rm = FALSE) , value = c(25, 75))
    }
       })
  
  # the following renders the plot on the ploting tab after the data loads of course
  output$plotout <- renderPlot({
    if(is.null(data())) {return ()}
    print(input$SliderY[2])
    plot(input$X, input$Y, data = data(),xlab = as.character(input$X),xlim = input$SliderX[2],ylab = as.character(input$Y),ylim = input$SliderY[2])
  
    })
  
})