
shinyUI(fluidPage(
  titlePanel("IWA : Interactive Web Application for Bayesian Hierarchical true prevalence estimation"),
  tabsetPanel(type = "tabs",
              tabPanel("IWA",
                       fluidPage(
                         fluidRow(column(6,img(src='iwa.png', align = "left"))),
                         h1("The model is based on a hierarchical structure that provides the following estimates:"),
                         br(),
                         h3("(i) a",tags$strong(" comparison between animal-level apparent"), "and", tags$strong("true prevalence of infection/disease within each herd,")),
                         h3("(ii) the ", tags$strong("true prevalence within each region,")),
                         h3("(iii) the", tags$strong(" probability of freedom of disease/infection in each region,")),
                         h3("(iv) the", tags$strong(" probability that the prevalence in each region is lower than a minimum acceptable threshold,")),
                         h3("(v) the", tags$strong(" probability that the whole country is free from infection,")),
                         h3("(vi) the", tags$strong(" probability that the prevalence within the country is lower than a minimum acceptable threshold.")),
                         h1("Below the user can find a brief description of the package functions and options."),
                         br(),
                         h3("(a) In tab (Set up model) the use can set the model parameters."),
                         h3("Next steps...")
                       )
              ),
              
              tabPanel("Set up model",
                       sidebarLayout(
                         sidebarPanel(radioButtons(inputId = "input4options", 
                                                   label="Type of 4 options", choices=c("Option 1",
                                                                                        "Option 2",
                                                                                        "Option 3",
                                                                                        "Option 4"), 
                                                   selected="Option 1",inline = T),
                                      radioButtons(inputId = "inputYesNo", 
                                                   label="Type of Yes on No", choices=c("No",
                                                                                        "Yes"), 
                                                   selected="No",inline = T),
                                      selectInput(inputId = 'slider1', choices=c("Choice1","Choice2",
                                                                                 "Choice3","Choice4",
                                                                                 "Choice5"), 
                                                  label = 'Pick a choice', selected = "Choice1"),
                                      verbatimTextOutput("info1"),
                                      verbatimTextOutput("info2"),
                                      verbatimTextOutput("info3"),
                                      verbatimTextOutput("info4")
                         ),
                         mainPanel(splitLayout(cellWidths = c("50%", "50%"),
                                               plotOutput("plot1", click = "plot_click1"), 
                                               plotOutput("plot2", click = "plot_click2")),
                                   splitLayout(cellWidths = c("50%", "50%"), 
                                               plotOutput("plot3", click = "plot_click3"), 
                                               plotOutput("plot4", click = "plot_click4")),
                         )
                       )
              ),
              tabPanel("PriorGen",
                       tabsetPanel(
                         tabPanel("Find beta",
                                  sidebarPanel(
                                    sliderInput(inputId = "PriorMedian", 
                                                label = "Specify your prior beleif about the mean/median/mode: ", 
                                                min=0, max=1, value=0.9,step = 0.01),
                                    sliderInput(inputId = "Percentile1", 
                                                label = " Specify the level of confidence that the true value of the median/mean/mode is greater or lower than the percentile.value: ",
                                                min=0, max=1, value=0.95,step = 0.01),
                                    #                                  sliderInput(inputId = "PercentileValue1", 
                                    #                                              label = "Specify the upper or lower limit for the mean/median/mode at the specified level of confidence: ", 
                                    #                                              min=0, max=1, value=0.80,step = 0.01),
                                    uiOutput("sliders_fb"),
                                    radioButtons(inputId = "lower.value", 
                                                 label="Is the percentile  the upper limit of the mean?", choices=c("TRUE","FALSE"), 
                                                 selected="FALSE",inline = T),
                                    plotOutput("PriorGenPlot1")),
                                  mainPanel(),
                         ),
                         tabPanel("Find betamupsi",
                                  sidebarPanel(
                                    sliderInput(inputId = "PriorMean", 
                                                label = "Specify your prior beleif about the mean: ", 
                                                min=0, max=1, value=0.2,step = 0.01),
                                    sliderInput(inputId = "Percentile2", 
                                                label = " Specify the level of confidence that the true value of the mean is greater or lower than the percentile.value: ",
                                                min=0, max=1, value=0.99,step = 0.01),
                                    sliderInput(inputId = "PercentileValue2", 
                                                label = "Specify the upper or lower limit for the mean/median/mode at the specified level of confidence: ", 
                                                min=0, max=1, value=0.30,step = 0.01),
                                    radioButtons(inputId = "lower.value", 
                                                 label="Is the percentile  the upper limit of the mean?", choices=c("TRUE","FALSE"), 
                                                 selected="TRUE",inline = T),
                                    sliderInput(inputId = "PercentilePsi", 
                                                label = ".... ", 
                                                min=0, max=1, value=0.90,step = 0.01),
                                    sliderInput(inputId = "PercentileMedian", 
                                                label = "....", 
                                                min=0, max=1, value=0.50,step = 0.01),
                                    sliderInput(inputId = "Percentile95value", 
                                                label = "....", 
                                                min=0, max=1, value=0.60,step = 0.01)),
                                  mainPanel(plotOutput("PriorGenPlot2")),
                         ),
                         tabPanel("Find betaqq",
                                  sidebarPanel(
                                    sliderInput(inputId = "PercentileValue3_1", 
                                                label = "Specify your prior beleif about the mean/median/mode: ", 
                                                min=0, max=1, value=0.3,step = 0.01),
                                    sliderInput(inputId = "Percentile3_1", 
                                                label = " Specify the level of confidence that the true value of the median/mean/mode is greater or lower than the percentile.value: ",
                                                min=0, max=1, value=0.3,step = 0.01),
                                    sliderInput(inputId = "PercentileValue3_2", 
                                                label = "Specify the upper or lower limit for the mean/median/mode at the specified level of confidence: ", 
                                                min=0, max=1, value=0.6,step = 0.01),
                                    sliderInput(inputId = "Percentile3_2", 
                                                label="Is the percentile  the upper limit of the mean?",
                                                min=0, max=1, value=0.8,step = 0.01)),
                                  mainPanel(plotOutput("PriorGenPlot3")),
                                  
                         )
                       )),
              tabPanel("Online report",
                       numericInput("obsY", "Y:", 1, min = 1, max = 1000000),
                       verbatimTextOutput("valueY")
              ),
              
              tabPanel("Acknowledgments",
                       fluidPage(
                         fluidRow(column(6,p("IWA was created using R Shiny and JAGS to implement the full hierarchical model for prevalence estimation."))),
                         fluidRow(column(6,img(src='hotline.png', align = "left")))
                       )    
                       
              )
  )
  
)

)
