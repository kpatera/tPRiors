

sidebarPanel(width = 4,
             tabsetPanel(
               tabPanel("Data input",
                        sliderInput(inputId = "y", 
                                    label = "Number of positive tests: ", 
                                    min=1, max=3000, value=14,step = 1),
                        sliderInput(inputId = "n", 
                                    label = "Sample size: ", 
                                    min=1, max=3000, value=72,step = 1),
                        sliderInput(inputId = "tau0", 
                                    label = "tau0 of dbern(tau0,0): ", 
                                    min=0, max=1, value=0.1,step = 0.01),
                        sliderInput(inputId = "perVal", 
                                    label = "Set prevalence limit: ", 
                                    min=0, max=1, value=0.05,step = 0.05),
                        div(style="display:inline-block;width:45%;text-align: left;",actionButton("buttonModelReset", "Reset..."),style=icon("check")),
                        div(style="display:inline-block;width:45%;text-align: right;",actionButton("buttonFixModel", "Fix model"),style=icon("check"))             
               ),
               tabPanel("MCMC input",
                        sliderInput(inputId = "nniter", 
                                    label = "Number of MCMC iterations: ", 
                                    min=10, max=100000, value=10000,step = 1000),
                        sliderInput(inputId = "nnthin", 
                                    label = " Thinning interval: ",
                                    min=1, max=500, value=5,step = 1),
                        sliderInput(inputId = "nchains", 
                                    label = " Number of MCMC chains: ",
                                    min=1, max=4, value=2,step = 1),
                        # div(style="display:inline-block;width:45%;text-align: left;",actionButton("buttonModelReset", "Reset..."),style=icon("check")),
                        #div(style="display:inline-block;width:45%;text-align: right;",actionButton("buttonFixModel", "Fix model"),style=icon("check"))             
               )
             )
)
