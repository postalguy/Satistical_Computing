library(shiny)
library(shinythemes)
shinyUI(
  
  navbarPage("Outils R",
              tabPanel("Upload",
                       # Upload UI :
                       fluidPage(theme = shinytheme("spacelab"),
                                 titlePanel("Charger un fichier"),
                                 sidebarLayout(
                                   #Side panel content
                                   sidebarPanel(
                                     fileInput("file","Upload the file", accept = c(
                                       'text/csv',
                                       'text/comma-separated-values',
                                       'text/tab-separated-values',
                                       'text/plain',
                                       '.csv',
                                       '.tsv'
                                     )), 
                                     helpText("Default max. file size is 1Go"),
                                     tags$hr(),
                                     h5(helpText("Paramètres")),
                                     checkboxInput(inputId = 'header', label = 'en-tete', value = TRUE),
                                     checkboxInput(inputId = "stringAsFactors", "stringAsFactors (accélère l'upload)", TRUE),
                                     
                                     br(),
                                     radioButtons(inputId = 'sep', label = 'Séparateur', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                                     radioButtons('quote', 'Citation',
                                                  c(None='',
                                                    '\"'='"',
                                                    '\''="'"),
                                                  '"')),
                                   mainPanel(
                                     uiOutput("tb")
                                   )
                                   
                                 )
                       )
                       
                       
                       ),
              tabPanel("Plots",
                       # Plot UI :
                       fluidPage(theme = shinytheme("spacelab"),
                                 titlePanel("Voir les graphes"),
                                 sidebarLayout(
                                   #Side panel content
                                   uiOutput("plotselection"),
                                   mainPanel(
                                    plotOutput("plotout")
                                   )
                                   
                                 )
                       )
                       ),
              tabPanel("Prediction")
  ))