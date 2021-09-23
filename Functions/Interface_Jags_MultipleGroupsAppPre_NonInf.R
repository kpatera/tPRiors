sidebarPanel(width = 4,
             tabsetPanel(
               tabPanel("Data input",
                        uiOutput("tab"),
                        #downloadButton("downloadFile",label = "Download example file"),
                        radioButtons("LoadData", "Load dataset:",
                                     c("Preload" = "Option1Preload",
                                       "Upload" = "Option2Upload"),inline = TRUE),
                        uiOutput("MultiDataset_fb"),
                        sliderInput("perVal", "Set prevalence checkpoint: ",min = 0, max = 1, value = 0.05,step=0.01),
                        div(style="display:inline-block;width:45%;text-align: left;",actionButton("buttonModelReset", "Reset tPriors"),style=icon("check")),
                        div(style="display:inline-block;width:45%;text-align: right;",actionButton("buttonFixModel", "Fix model"),style=icon("check"))             
               ),
               tabPanel("MCMC input",
                        numericInput("nniter", "Number of MCMC iterations: ",min = 10, max = 100000, value = 10000),
                        numericInput("nnthin", "Thinning interval: ",min = 2, max = 1000, value = 5),
                        sliderInput(inputId = "nchains", 
                                    label = " Number of MCMC chains: ",
                                    min=1, max=3, value=2, step = 1)#,
                        #  div(style="display:inline-block;width:45%;text-align: left;",actionButton("buttonModelReset", "Reset..."),style=icon("check")),
                        #  div(style="display:inline-block;width:45%;text-align: right;",actionButton("buttonFixModel", "Fix model"),style=icon("check"))             
               )
             )
)