sidebarPanel(width = 4,
             sliderInput(inputId = "PriorMetric", 
                         label = paste("Specify your prior belief about the",input$ID_MeanMedianMode,"",input$ID_TrueApp,": "), 
                         min=0, max=1, value=0.6,step = 0.01),
             sliderInput(inputId = "Percentile1", 
                         label = paste("Specify the level of confidence that the true value of the",input$ID_MeanMedianMode,"is greater or lower than the percentile.value: "),
                         min=0, max=1, value=0.95,step = 0.01),
             uiOutput("sliders_fb"),
             radioButtons(inputId = "lower.value", 
                          label=paste("Is the percentile  the upper limit of the",input$ID_MeanMedianMode,"?"), choices=c("TRUE","FALSE"), 
                          selected="FALSE",inline = T),
             div(style="display:inline-block;width:45%;text-align: left;",actionButton("buttonPriorReset", "Reset tPriors"),style=icon("check")),
             div(style="display:inline-block;width:45%;text-align: right;",actionButton("buttonPrior", "Set prior!"),style=icon("check"))
             
)