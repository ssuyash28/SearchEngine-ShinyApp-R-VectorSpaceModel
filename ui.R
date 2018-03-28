shinyUI(fluidPage(titlePanel('BOOKMARK SEARCH'),
                  sidebarLayout(
                        sidebarPanel(
                          
                          textInput('x','Looking For'),
                          submitButton('Submit')
                          
                        ),
                        mainPanel(tableOutput('cc2'))
                        )))
