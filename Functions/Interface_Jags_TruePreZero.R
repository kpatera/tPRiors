sidebarPanel(width = 4,
             
             tabsetPanel(
               tabPanel("Data input",
                        numericInput("n", "Sample size: ",min = 2, max = 1000000, value = 500),
                        numericInput(inputId = "y", label = "Number of positive tests: ",min = 1, max = 1000000, step = 1, value = 25),
                       # numericInput("tau0", "Probability for non-zero prevalence: ",min = 0, max = 1, value = 0.1,step = 0.01),
                       sliderInput("perVal", "Set a prevalence checkpoint/threshold value (c), to calculate the Pr(Prevalence>c): ",min = 0, max = 1, value = 0.05,step= 0.01),
                       div(style="display:inline-block;width:45%;text-align: left;",actionButton("buttonModelReset", "Soft reset"),style=icon("check")),
                        div(style="display:inline-block;width:45%;text-align: right;",actionButton("buttonFixModel", "Fix model"),style=icon("check"))             
               ),
               tabPanel("MCMC input",
                        numericInput("nniter", "Number of MCMC iterations: ",min = 10, max = 100000, value = 10000),
                        numericInput("nnthin", "Thinning interval: ",min = 2, max = 1000, value = 5),
                        sliderInput(inputId = "nchains", 
                                    label = " Number of MCMC chains: ",
                                    min=1, max=3, value=2,step = 1)#,
                        #  div(style="display:inline-block;width:45%;text-align: left;",actionButton("buttonModelReset", "Reset..."),style=icon("check")),
                        #  div(style="display:inline-block;width:45%;text-align: right;",actionButton("buttonFixModel", "Fix model"),style=icon("check"))             
               )
             )
)