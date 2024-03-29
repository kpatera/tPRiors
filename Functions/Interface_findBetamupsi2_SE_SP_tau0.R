sidebarPanel(width = 4,
             tabsetPanel(
               tabPanel("Prevalence",
                        hr(),
                        h3("Set a prevalence prior"),
                        hr(),
                        sliderInput(inputId = "PriorMean2", 
                                    label = paste("What is the most likely value for the mean of the",input$ID_TrueApp," (tm): "), 
                                    min=0, max=1, value=0.2,step = 0.01),
                        radioButtons(inputId = "lower.value2",
                                     label=paste("Will you set an upper percentile limit for the mean of the",input$ID_TrueApp," (lv)? If No, then a lower limit is specified."), choices=c("Yes","No"),
                                     selected="Yes",inline = T),
                        sliderInput(inputId = "Percentile2", 
                                    label = " Specify the level of confidence (per) that the true value of the mean is greater or lower than the percentile value: ",
                                    min=0, max=1, value=0.99,step = 0.005),
                        sliderInput(inputId = "PercentileValue2", label = paste("Specify the lower limit (pev) for the mean of the",input$ID_TrueApp,"at the specified level of confidence: "), 
                                    value = 0.5, min=0, max=1,step = 0.005),
                        sliderInput(inputId = "PercentilePsi2", 
                                    label = "Specify the level of confidence (perpsi) that a certain fraction of the units under study has a prevalence less than the percentile median. ", 
                                    min=0.501, max=0.999, value=0.9,step = 0.005),
                        sliderInput(inputId = "PercentileMedian2", label = "Which is the median value (perm) that corresponds to the defined confidence? This has to be higher than both (tm) and (per) set above: ", 
                                    value = 0.8, min=0, max=1,step = 0.005),
                        sliderInput(inputId = "Percentile95value2", label = "Which is the value that the percentile median (per95v) does not exceed with 95% confidence? This has to be higher than (per) set above", 
                                    value=0.9, min=0, max=1, step = 0.005)
               ),
               tabPanel("ZeroP",
                        hr(),
                        h3("Set a non-zero prevalence probability prior"),
                        hr(),
                        sliderInput(inputId = "PriorMetric2_tau0", 
                                    label = paste("What is the most likely value for the",input$ID_MeanMedianMode,"of the non-zero prevalence (tm): "), 
                                    min=0, max=1, value=0.6,step = 0.01),
                        radioButtons(inputId = "lower.value2_tau0", 
                                     label=paste("Will you set an upper percentile limit for the",input$ID_MeanMedianMode,"of the non-zero prevalence (lv)? If No, then a lower limit is specified."), choices=c("Yes","No"),
                                     selected="No",inline = T),
                        uiOutput("sliders_fb2_tau0"),
                        sliderInput(inputId = "Percentile2_tau0", 
                                    label = paste("Specify the level of confidence (per) that the true value of the non-zero prevalence",input$ID_MeanMedianMode,"is more extreme than the percentile limit: "),
                                    min=0.51, max=1, value=0.95,step = 0.01)
                        
               ),
               tabPanel("Se",
                        hr(),
                        h3("Set a sensitivity prior"),
                        hr(),
                        sliderInput(inputId = "PriorMetric2_SE", 
                                    label = paste("What is the most likely value for the",input$ID_MeanMedianMode,"of the Sensitivity (tm): "), 
                                    min=0, max=1, value=0.6,step = 0.01),
                        radioButtons(inputId = "lower.value2_SE", 
                                     label=paste("Will you set an upper percentile limit for the",input$ID_MeanMedianMode," of the Sensitivity (lv)? If No, then a lower limit is specified."), choices=c("Yes","No"),
                                     selected="No",inline = T),
                        uiOutput("sliders_fb2_SE"),
                        sliderInput(inputId = "Percentile2_SE", 
                                    label = paste("Specify the level of confidence (per) that the true value of the Sensitivity",input$ID_MeanMedianMode,"is more extreme than the percentile limit: "),
                                    min=0.51, max=1, value=0.95,step = 0.01)
               ),
               tabPanel("Sp",
                        hr(),
                        h3("Set a specificity prior"),
                        hr(),
                        sliderInput(inputId = "PriorMetric2_SP", 
                                    label = paste("What is the most likely value for the",input$ID_MeanMedianMode,"of the Specificity (tm): "), 
                                    min=0, max=1, value=0.6,step = 0.01),
                        radioButtons(inputId = "lower.value2_SP", 
                                     label=paste("Will you set an upper percentile limit for the",input$ID_MeanMedianMode," of the Specificity (lv)? If No, then a lower limit is specified."), choices=c("Yes","No"),
                                     selected="No",inline = T),
                        uiOutput("sliders_fb2_SP"),
                        sliderInput(inputId = "Percentile2_SP", 
                                    label = paste("Specify the level of confidence (per) that the true value of the Specificity",input$ID_MeanMedianMode,"is more extreme than the percentile limit: "),
                                    min=0.51, max=1, value=0.95,step = 0.01)
               )
             ),
             h4("Caution! Please press 'set priors' only when all values of all parameters are set. "),
             radioButtons(inputId = "lower.value.fix", 
                          label=paste("In case 1 slider gets stuck, select 'Patch', wait and select 'Fixed' to continue"),choices=c("Fixed","Patch"),
                          selected="Fixed", inline = T),
             div(style="display:inline-block;width:30%;text-align: left;",actionButton("buttonPriorReset", "Soft reset"),style=icon("check")),
             div(style="display:inline-block;width:30%;text-align: center;",actionButton("buttonPriorHelp3", "Help"),style=icon("check")),
             div(style="display:inline-block;width:30%;text-align: right;",actionButton("buttonPrior", "Set prior!"),style=icon("check"))
)