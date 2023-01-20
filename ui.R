shinyUI(
  navbarPage(title="",
             inverse=TRUE,
             #theme = shinytheme("yeti"),
             shinyWidgets::useShinydashboard(),
             #shinythemes::themeSelector(), 
             #tabsetPanel(type = "tabs",
             tabPanel("Start",
                      fluidPage(id = "navbar",
                                useShinyjs(),
                                fluidRow(column(12,img(src='BannerApp2.png', align = "center",width="100%"))),
                                br(),
                                h2("Below the user can find a brief description of the shiny application functions and options."),
                                h3("(a) In tab (Set up) the user following questions can fix the parameters of the analysis (Choose model, priors, special characteristics) "),
                                h3("(b) In tab (Prior(s)) the user can elicitate the prior distribution(s) with the aid of sliders and visual confirmation"),
                                h3("(c) In tab (I|nput & O|utput) the user inputs the observed data and Jags sampling characteristics. A basic inference plot is presented. For multiple population both step 1 and step 2 should be selected before you proceed."),
                                h3("(d) In tab (Report) the program returns a dynamic output that changes based on (a), (b) and (c)."),
                                h3("(e) In tab (Acks) acknoweledgments and Frequently Asked Questions (FAQ) can be found."),
                                br(),
                                h3("Settings may still be changed even after a tab has been fixed by the user. Though, we advise users to perform a 'Reset' of |tPRiors| when they want to change a previously fixed setting. We also advice users to perform a 'Refresh' of the web page tPRiors when they perform a different analysis"),
                                br(),
                                h3("A template file of a multiple population example can be found ", a("here", href="https://1drv.ms/x/s!Ag8vFzeTRzBthzImmQV_nEDdj4Uy?e=BjwmQL"),". It is essential that at least two columns exist in the spreasheet one named 'positive' and one named 'n'"),
                                hr(),
                                h3("The development of tPRiors was funded by H2020 project unCoVer:Unravelling Data for Rapid Evidence-Based Response. More details can be found in the manuscript, K Pateras and P Kostoulas, tPRiors: A tool for prior elicitation and obtaining posterior distributions of true disease prevalence"),
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
                        div(style="display:inline-block;width:45%;text-align: left;",actionButton("buttonReset", "Soft reset"),style=icon("check")),
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
                        uiOutput("Priors_Plot_Sum_fb"),
                        textOutput("PriorValues")
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
                          box(align="center",width = 4,background = "orange",h3("PriorGen"))),
                        
                        br(),
                        h2("Frequently asked questions (FAQ)"),
                        h3("Q1: What to do if a set-up does not produce a valid prior firuge in page 'priors'?"),
                        h5("Answer: If the movement of a slider produces a warning message, slightly move the slider to the other direction."),
                        h3("Q2: What do to if a slider in the page 'priors' gets stuck and it starts jumping around?"),
                        h5("Answer: This behaviour is only observed if the button 'Set priors' has been clicked and consequently the user performs ad-hoc changes. We suggest eliciting priors before visualizing them by pressing the 'set priors' button. We provide an ad-hoc solution to the user, in case the situation appears. In case any slider gets stuck, select 'Patch', wait and select 'Fixed' to continue. The analysis can continue and finish properly."),
                        h3("Q3: When using the percentile approach to elicit priors, the prior is not defined and error occurs"),
                        h5("Answer: If the user inputs a very small value for the 1st percentile and a very large value for the 2nd percentile the prior will not be defined."),
                        h3("Q4: When should I press the 'Set prior' button?"),
                        h5("Answer: This button is only displayed on the 1st prior tab and it should be clicked when priors for all parameters are elicited. Then, the user should wait until the prior figures are plotted on the white space on the right side of the application. If no figure is properly plotted, then the prior elicitation has not been performed properly. "),
                        h3("Q5: What do the abbreviations mean in the 'Prior(s)' page?"),
                        h5("Answer: The abbreviations are set only to make the text in the Help button easier to understand. Then if a user follow the example, it would be clear that each value correspond to a specific question/slider."),
                        br(),
                        h3("Q6: I run a single population example and instead for a report, I receive an error 'Error: An error has occurred. Check your logs or contact the app author for clarification'. What should I do? "),
                        h5("Answer: The most common reason for this error in the single population setting would be improper prior spefications. Avoid eliciting extreme prior densities (very informative as in a single point or very vague densities across the parameter space) "),
                        h3("Q7: I run a multiple population example and instead for a report, I receive an error 'Error: An error has occurred. Check your logs or contact the app author for clarification'. What should I do? "),
                        h5("Answer: Before cliking on the report page, make sure that both Step 1. and Step 2. tabs have to be selected to run the multiple-population models."),
                        h3("Q8: How to interpret the diagnostic plots? Is my analysis valid?"),
                        h5("Answer: Most diagnostic plots show convergence issues. Some points to consider 1. The simple density plots can show abnormalities in the posterior densities ,2. The multiple chain density plots must contain densities of different chains that mostly coincide ,3. MCMC trace plots should depict chains that cross often  4. The ergodic mean and the correlation plot should very quickly move towards the mean and zero respectively, otherwise the user should increase the thinning interval and/or the number of MCMC samples."),
                        h3("Q9: Some diagnostic plots look abnormal, can I still use the final report?"),
                        h5("Answer: First, the user should try to make corrections (see accompanying manuscript). In case one of the diagnostics still slightly point towards non-convergence or high autocorrelation the users should read the report with caution if not at all."),
                        h3("Q10: How can I download the whole report?"),
                        h5("Answer: Most browsers have the option to print into a .pdf file the whole webpage. Try pressing ctrl+P and then selecting pdf."),
                        h3("Q11: How to perform a reset of the |tPRiors| application?"),
                        h5("Answer: We advise users to reset the application by refreshing the webpage. For a quicker soft reset the corresponding button can be clicked on each page."),
                        br(),
                        h3("Q12: I have more questions on how to run an analysis."),
                        h5("Answer.1: We further refer users to the complementary manuscript 'K Pateras and P Kostoulas, tPRiors: tPRiors: A tool for prior elicitation and obtaining posterior distributions of true disease prevalence'"),
                        h5("Answer.2: In case a question is not covered by this FAQ nor the manuscript, please do not hesitate to contact the authors via the following e-mails: kostas.pateras(at)gmail.com, pkost(at)uth.gr")
                        
                      )    
             )
  )
  
)

#)
