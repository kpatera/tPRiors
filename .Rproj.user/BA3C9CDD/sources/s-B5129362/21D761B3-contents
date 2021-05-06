shinyUI(
  navbarPage(inverse=TRUE,
             #theme = shinytheme("yeti"),
             shinyWidgets::useShinydashboard(),
             #shinythemes::themeSelector(), 
             #titlePanel("IWA : Interactive Web Application for Bayesian Hierarchical true prevalence estimation"),
             #tabsetPanel(type = "tabs",
             tabPanel("Start",
                      fluidPage(id = "navbar",
                                useShinyjs(),
                                fluidRow(column(12,img(src='BannerApp2.png', align = "center",width="100%"))),
                                #h2("The full model is based on a Bayesian hierarchical structure."),# that provides the following estimates:"),
                                br(),
                                #                                         h3("(i) a",tags$strong(" comparison between animal-level apparent"), "and", tags$strong("true prevalence of infection/disease within each herd,")),
                                #                                        h3("(ii) the ", tags$strong("true prevalence within each region,")),
                                #                                       h3("(iii) the", tags$strong(" probability of freedom of disease/infection in each region,")),
                                #                                      h3("(iv) the", tags$strong(" probability that the prevalence in each region is lower than a minimum acceptable threshold,")),
                                #                                     h3("(v) the", tags$strong(" probability that the whole country is free from infection,")),
                                #                                    h3("(vi) the", tags$strong(" probability that the prevalence within the country is lower than a minimum acceptable threshold.")),
                                h4("Below the user can find a brief description of the shiny application functions and options."),
                                br(),
                                h5("After a tab has been set by the user, settings may still be changed, though, we advise users to perform a Reset pf |tPRiors| when they want to change a previously set setting."),
                                br(),
                                h5("(a) In tab (Set up) the user following questions can fix the parameters of the analysis (Choose model, priors, special characteristics) "),
                                h5("(b) In tab (Priors) the user can elicitate the prior distribution(s) with the aid of sliders and visual confirmation"),
                                h5("(c) In tab (Model) the user inputs the observed data and Jags sampling characteristics. A basic inference plot is presented. For multiple population the model may take some time to run."),
                                h5("(d) In tab (Report) the program returns a dynamic output that changes based on (a), (b) and (c)."),
                                h5("(e) In tab (Acks) acknoweledgments and useful links can be found."),
                                br(),
                                #h5("Note: If the sliders in tab (Priors) act not normal, press the Reset button and try again."),
                                #hr(),
                                h5("The development of tPRiors was funded by H2020 project unCoVer:Unravelling Data for Rapid Evidence-Based Response. More details can be found in the manuscript, K Pateras and P Kostoulas, tPRiors: An R Shiny tool for generating prior and producing posterior distributions for disease prevalence"),
                                hr(),
                                br(),
                                fluidRow(
                                  shinydashboard::valueBoxOutput("NumberPriorSetups", width = 4),
                                  shinydashboard::valueBoxOutput("NumberModels", width = 4),
                                  shinydashboard::valueBoxOutput("NumberDemos", width = 4))
                      )
             ),
             tabPanel("Set up",
                      sidebarPanel(
                        shinyjs::useShinyjs(), # Platform for reset button
                        id = "setup-panel",
                        radioButtons(inputId = 'ID_Informative', choices=c("Yes",
                                                                       "No"),
                                     label = 'Would you define informative priors?',
                                     selected = "Yes",inline = TRUE),
                        hr(),
                        uiOutput("TrueApp_fb"), 
                        hr(),
                        radioButtons(inputId = 'ID_SingleMultiple', choices=c("Single population",
                                                                              "Multiple populations"),
                                     label = 'Do you want to model single or multiple population prevelances?',
                                     selected = "Multiple populations",inline = TRUE),
                        hr(),
                        uiOutput("zero_fb"),
                        hr(),
                        uiOutput("metric_fb"),
                        hr(),
                        div(style="display:inline-block;width:45%;text-align: left;",actionButton("buttonReset", "Reset tPRiors"),style=icon("check")),
                        div(style="display:inline-block;width:45%;text-align: right;",actionButton("buttonSetup", "Fix setup!"),style=icon("check")),
                      ),
                      mainPanel(
                        fluidRow(shinydashboard::valueBoxOutput("Boxsetup1", width = 60))
                      )
                      
                      
             ),
             
             tabPanel("Prior(s)",
                      #   tabsetPanel(
                      #   tabPanel("Generate prior distribution",
                      #    div(style="display:inline-block;width:40%;text-align: center;",radioButtons(inputId = 'Prior_general', choices=c("Find beta","Find betamupsi","Find betaqq"),
                      #                                                                                 inline = TRUE, label = 'Pick a way to elicitate beliefs on a beta prior', 
                      #                                                                                selected = "Find beta")),
                      shinyjs::useShinyjs(), # Platform for reset button
                      id = "priors-panel",
                      uiOutput("Priors_fb"),
                      mainPanel(
                        
                        
                        fluidRow(shinydashboard::valueBoxOutput("Boxsetup2", width = 60)),
                        #     textOutput("PriorSelectionText"),
                        # div(style="margin-top:+2.5em",
                        uiOutput("Priors_Plot_Sum_fb")
                        #  )
                      )
                      
                      #   )
                      #  )
             ),
             tabPanel("Model",
                      #radioButtons(inputId = 'Model_general', choices=c("Apparent prevalence estimation",
                      #                                                   "True prevalence estimation",
                      #                                                   "Allow for zero prevalence (in true prevalence estimation)",
                      #                                                   "Multiple groups"), label = 'Pick the appropriate statistical approach to model your data',
                      #              selected = "Apparent prevalence estimation",inline = TRUE),
                      shinyjs::useShinyjs(), # Platform for reset button
                      id = "model-panel",
                      uiOutput("Bayesian_fb"),
                      uiOutput("MultiDatasets_Out_fb")
             ),
             tabPanel(title = "Report",
                      shinydashboard::valueBoxOutput("Boxsetup4", width = 60),
                      uiOutput("report_fb"),
                      uiOutput("report_fb_side")
                      # sidebarLayout(
                      #   sidebarPanel(width=4,
                      #     radioButtons("dimens", label = "Figure dimensions:",choices = c(200,400,600,800),selected =400,inline = TRUE),
                      #     radioButtons("format", "Download report:", 
                      #                  c("PDF"),inline = TRUE #  "PDF", "Word"
                      #     ),
                      #  #   checkboxInput("echo", "Show code in report?", FALSE),
                      #     downloadButton("downloadReport"),
                      #   ),
                      #  mainPanel(
                      #            )
                      
                      
                      # tags$b("Your data:"),
                      # # DT::dataTableOutput("tbl"),
                      # br(),
                      # # uiOutput("data"),
                      # br(),
                      # tags$b("Compute parameters by hand:"),
                      # # uiOutput("by_hand"),
                      # br(),
                      # tags$b("Compute parameters in R/Rjags:"),
                      # # verbatimTextOutput("summary"),
                      # br(),
                      # tags$b("Prevelance plot:"),
                      # #uiOutput("results"),
                      # # plotlyOutput("plot"),
                      # br(),
                      # tags$b("Interpretation:"),
                      # #uiOutput("interpretation"),
                      # br(),
                      # br(),
                      # fluidRow(
                      #   splitLayout(cellWidths = c("46%", "46%"), 
                      #               uiOutput("APar5_Plot_fb"), 
                      #               uiOutput("APar6_Plot_fb")))
                      
                      #  )
             ),
             # tabPanel("Demos",
             #          sidebarLayout(
             #            sidebarPanel(selectInput(inputId = "Prior4options",
             #                                     label="Prior set Ups", choices = c("Beta(89,2) - Unif(0,1) - TN(0.1)T[0,1]]",
             #                                                                        "Beta(50,50) - Unif(0,1) - TN(1000)T[0,1]",
             #                                                                        "Beta(2,89) - Unif(0,1) - TN(1)T[0,1]",
             #                                                                        "Beta(1,1) - Unif(0,1) - TN(5)T[0,1]"),
             #                                     selected="Beta(89,2) - Unif(0.5,1) - TN(0.5)T[0,1]]"),
             #                         sliderInput(inputId = "y1", 
             #                                     label = "Number of positive tests: ", 
             #                                     min=1, max=3000, value=14,step = 1),
             #                         sliderInput(inputId = "n1", 
             #                                     label = "Sample size: ", 
             #                                     min=1, max=3000, value=72,step = 1),
             #                         sliderInput(inputId = "nniter1", 
             #                                     label = "Number of MCMC iterations: ", 
             #                                     min=10, max=100000, value=10000,step = 1000),
             #                         sliderInput(inputId = "nnthin1", 
             #                                     label = " Thinning interval: ",
             #                                     min=1, max=500, value=2,step = 1)
             #                         
             #            ),
             #            mainPanel(plotOutput("demo_APpre"))
             #            #,
             #            #              radioButtons(inputId = "inputYesNo",
             #            #                           label="Type of Yes on No", choices=c("No",
             #            #                                                                "Yes"),
             #            #                           selected="No",inline = T),
             #            # 
             #            #              verbatimTextOutput("info1"),
             #            #              verbatimTextOutput("info2"),
             #            #              verbatimTextOutput("info3"),
             #            #              verbatimTextOutput("info4")
             #            # ),
             #            # mainPanel(splitLayout(cellWidths = c("50%", "50%"),
             #            #                       plotOutput("plot1", click = "plot_click1"),
             #            #                       plotOutput("plot2", click = "plot_click2")),
             #            #           splitLayout(cellWidths = c("50%", "50%"),
             #            #                       plotOutput("plot3", click = "plot_click3"),
             #            #                       plotOutput("plot4", click = "plot_click4")),
             #          )
             # ),
             tabPanel("Acks",
                      fluidPage(
                        fluidRow(box(align="center",width = 12,background = "black",
                                     h3("tPRiors was created using Shiny, JAGS and PriorGen to implement the full hierarchical model for prevalence estimation. tPRiors is powered by the following R packages:"))),
                        #fluidRow(column(6,img(src='hotline.png', align = "left"))),
                        fluidRow(
                          box(align="center",width = 3,background = "light-blue",h3("ggplot2")),
                          box(align="center",width = 3,background = "light-blue",h3("plotly")),
                          box(align="center",width = 3,background = "teal",h3("gridExtra")),
                          box(align="center",width = 3,background = "teal",h3("grid"))),
                        fluidRow(
                          box(align="center",width = 3,background = "red",h3("tidyr")),
                          box(align="center",width = 3,background = "red",h3("DT")),
                          box(align="center",width = 3,background = "olive",h3("shiny")),
                          box(align="center",width = 3,background = "olive",h3("shinythemes"))),
                        fluidRow(
                          box(align="center",width = 3,background = "olive",h3("shinydashboard")),
                          box(align="center",width = 3,background = "olive",h3("shinyjs")),
                          box(align="center",width = 3,background = "olive",h3("shinyWidgets")),
                          box(align="center",width = 3,background = "teal",h3("rmarkdown"))),
                        fluidRow(
                          box(align="center",width = 4,background = "orange",h3("R2jags")),
                          box(align="center",width = 4,background = "orange",h3("ggmcmc")),
                          box(align="center",width = 4,background = "orange",h3("PriorGen")))
#                         fluidRow(box(align="left",width = 12,background = "black",
#                                      h3("The development of tPRiors was funded by H2020 project unCoVer:Unravelling Data for Rapid Evidence-Based Response. More details can be found in the manuscript Pateras K, ..., Kostoulas P. tPRiors: An R Shiny tool for generating prior and
# producing posterior distributions for disease prevalence")))
                        #)
                        
                        #                                           box(width = 4,background = "olive",tags$a(href="www.rstudio.com", "Click here!")))
                        
                      )    
                      
             )
  )
  
)

#)
