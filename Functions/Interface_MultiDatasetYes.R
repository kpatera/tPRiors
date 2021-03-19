mainPanel(width=8,fluidRow(valueBoxOutput("Boxsetup3", width = 50)),
          tabsetPanel(
            tabPanel("Data",
                     fluidRow(dataTableOutput("table2")) 
            ),
            tabPanel("Output",
                     fluidRow(uiOutput("APar1_Plot_fb"))
            )
          )
)
