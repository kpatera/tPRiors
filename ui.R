shinyUI(
  navbarPage(inverse=TRUE,
             #theme = shinytheme("yeti"),
             shinyWidgets::useShinydashboard(),
             #shinythemes::themeSelector(), 
             #tabsetPanel(type = "tabs",
             tabPanel("Start",
                      fluidPage(id = "navbar",
                                useShinyjs(),
                                fluidRow(column(12,img(src='BannerApp2.png', align = "center",width="100%"))),
                                br(),
                                h4("Below the user can find a brief description of the shiny application functions and options."),
                                h5("(a) In tab (Set up) the user following questions can fix the parameters of the analysis (Choose model, priors, special characteristics) "),
                                h5("(b) In tab (Priors) the user can elicitate the prior distribution(s) with the aid of sliders and visual confirmation"),
                                h5("(c) In tab (I & O) the user inputs the observed data and Jags sampling characteristics. A basic inference plot is presented. For multiple population both step 1 and step 2 should be selected before you proceed."),
                                h5("(d) In tab (Report) the program returns a dynamic output that changes based on (a), (b) and (c)."),
                                h5("(e) In tab (Acks) acknoweledgments and useful links can be found."),
                                br(),
                                h5("Settings may still be changed even after a tab has been fixed by the user. Though, we advise users to perform a 'Reset' of |tPRiors| when they want to change a previously fixed setting."),
                                br(),
                                #h5("Note: If the sliders in tab (Priors) act not normal, press the Reset button and try again."),
                                #hr(),
                                h5("The development of tPRiors was funded by H2020 project unCoVer:Unravelling Data for Rapid Evidence-Based Response. More details can be found in the manuscript, K Pateras and P Kostoulas, tPRiors: tPRiors: A tool for prior elicitation and obtaining posterior distributions of true disease prevalence"),
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

                        radioButtons(inputId = 'ID_SingleMultiple', choices=c("Single",
                                                                              "Multiple"),
                                     label = 'Do you have single or multiple populations/clusters',
                                     selected = "Multiple",inline = TRUE),
                        hr(),
                        uiOutput("TrueApp_fb"), 
                        hr(),
                        uiOutput("zero_fb"),
                        hr(),
                        radioButtons(inputId = 'ID_Informative', choices=c("Yes",
                                                                           "No"),
                                     label = 'Would you specify informative priors?',
                                     selected = "Yes",inline = TRUE),
                        h6("If No, then the apparent prevalence will be modelled"),
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
             tabPanel("I|nput & O|utput",
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
             ),
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
                      )    
             )
  )
  
)

#)
