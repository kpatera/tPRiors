mainPanel(width=8,fluidRow(valueBoxOutput("Boxsetup3", width = 50)),
          tabsetPanel(
            tabPanel("Step 1. Data",
                     fluidRow(dataTableOutput("table2"))
            ),
            tabPanel("Step 2. Output",
                     fluidRow(uiOutput("APar1_Plot_fb"))
            )
          )
)
