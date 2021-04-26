sidebarPanel(width = 4,
             radioButtons(inputId = "priorNonInf", 
                          label=paste("Choose a weakly informative prevalence prior distribution ?"),
                          choices=c("Beta(1,1)","Beta(0.5,0.5)","Beta(2,2)"),
                          selected="Beta(1,1)",inline = T),
             div(style="display:inline-block;width:30%;text-align: left;",actionButton("buttonPriorReset", "Reset tPriors"),style=icon("check")),
             div(style="display:inline-block;width:30%;text-align: right;",actionButton("buttonPrior", "Set prior(s)!"),style=icon("check"))
             
)