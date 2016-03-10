shinyUI(
  navbarPage("R data",
             # Tab Panel 1 : for uploading the data and view in in a table
             tabPanel("Upload",
                      fluidPage(
                        titlePanel("Uploading Files"),
                        sidebarLayout(
                          #SIDE
                          sidebarPanel(
                            fileInput('file1', 'Choose file to upload',
                                      accept = c(
                                        'text/csv',
                                        'text/comma-separated-values',
                                        'text/tab-separated-values',
                                        'text/plain',
                                        '.csv',
                                        '.tsv'
                                      )
                            ),
                            tags$hr(),
                            checkboxInput('header', 'Header', TRUE),
                            radioButtons('sep', 'Séparateur',
                                         c('Virgule'=',',
                                           'Point Virgule'=';',
                                           'Tabulation'='\t'),
                                         ','),
                            radioButtons('quote', 'Citation',
                                         c(None='',
                                           '\"'='"',
                                           '\''="'"),
                                         '"'),
                            tags$hr(),
                            p('Cet onglet permet de cahrger un fichier')
                          ),
                          #CENTER
                          mainPanel(
                            dataTableOutput('contents')
                          )
                        )
                      )),
             # Tab Panel 2 : for graphes and plots
             tabPanel("Statistical exploration",
                      fluidPage(
                        titlePanel("Statistics"),
                        sidebarLayout(
                          # SIDE
                          sidebarPanel(
                            h2('Distributions'),
                            checkboxInput("Global","Afficher",value = FALSE)
                            ,
                            uiOutput('summaryForVars'),
                            tags$hr(),
                            p('Informations statistiques')
                          ),
                          #CENTER
                          mainPanel(
                            textOutput('TextOut')
                          )
                        )
                      )),
             #Tab Panel 3 for plots
             tabPanel( "Graphes",uiOutput("ploting")
                       
                       
             ),
             navbarMenu("More",
                        tabPanel("Sub-Component A"),
                        tabPanel("Sub-Component B"))
  )
)