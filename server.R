shinyServer(function(input, output, session) {
  #---------- Set up initial values -----------#
  options(shiny.reactlog=TRUE) #+ ctrl+F3
  priors <<- reactiveValues()
  cond<-reactiveValues()
  input<<-reactiveValues()
  priors$prior <<- reactiveValues()
  prior_cond<-reactiveValues()
  priors$temp<-"Status: 'Not set'"
  priors$temp2<-"Please first select options and press -Fix-"
  priors$icons<-"thumbs-down"
  priors$color<-"red"
  priors$SetupPriors<-FALSE
  priors$PriorSelect<-FALSE
  priors$mSetupmodel<-FALSE
  priors$mSetupPriors<-FALSE
  #input <- reactiveValues(buttonPrior = NULL)

  #------ POP UP MESSAGE in the beginning -------#
  
  # showModal(modalDialog(
  #   title = "Important message",
  #   easyClose = FALSE,
  #   p("tPriors aims at Bayesian (true) prevalence estimation based on elicicated prior opinions.",
  #     tags$strong("
  #   Following Data Protection legislations, we would like to inform you before you use our web application that :"), "We collect data regardingn your app usage within the IWA app to conduct analysis of usage and develope the application further. By clicking",
  #     tags$i(tags$u("I consent")), "you consent to us utilizing the data via Google Analytics.
  #         We refer interested users to our policy by clicking the 'Privacy notice' tab from within the app, and also ",tags$a(href="https://policies.google.com/privacy?hl=en", "Google Privacy & Terms.",target="_blank") ),
  #   br(),
  #   modalButton("I consent"),
  #   footer = NULL
  # ))
  
  #----- PriorGen routines ------#
  
  
  output$PriorGenPlot0 <- plotly::renderPlotly({
    #source("Functions/multiroot.R",local = TRUE)
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    fb<<-findbeta2(themean =0.2, percentile=0.9,lower.v=TRUE, percentile.value=0.5)
    if(input$priorNonInf=="Beta(1,1)") {
      fb$a<<-1;fb$atotalbeta<<-1; fb$b<<-1;fb$btotalbeta<<-1}
    if(input$priorNonInf=="Beta(0.5,0.5)") {
      fb$a<<-0.5;fb$atotalbeta<<-0.5; fb$b<<-0.5;fb$btotalbeta<<-0.5}
    if(input$priorNonInf=="Beta(2,2)") {
      fb$a<<-2;fb$atotalbeta<<-2; fb$b<<-2;fb$btotalbeta<<-2}
    priors$prior<<-list(a=fb$a,b=fb$b)
    
    x<-seq(0,1,length.out = 10000)
    #plot(x,dbeta(x = x,dbeta(x = x,shape1 = fb$a,shape2 = fb$b),shape1 = fb$a,shape2 = fb$b),type = "l",lwd=3,ylab = "Density Beta")
    Prevalence<-dbeta(x = x,shape1 = fb$a,shape2 = fb$b)
    df <- data.frame(x, Prevalence)
    df <- gather(df, func, val, -x)
    gg <- ggplot(df, aes(x=x, y=val, group=func))
    gg <- gg + geom_line(aes(color=func),size=1.3) 
    gg <- gg + scale_y_continuous(expand=c(0, 0))
    gg <- gg + scale_color_manual(name="Prior(s)", 
                                  values=c("#e69138"),
                                  labels=c("Prevalence"))
    gg <- gg + labs(x="Density", y="Probability distribution functions")
    gg <- gg + theme_bw()
    gg <- gg + theme(panel.border=element_blank())
    gg <- gg + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ggplotly(gg)
    #fig <- plot_ly(x = ~x, y = ~dbeta(x = x,shape1 = fb$a,shape2 = fb$b), type = 'scatter', mode = 'lines', fill = 'tozeroy',
    #               width=400, height=400)
    #fig <- fig %>% layout(xaxis = list(title = 'Beta'), yaxis = list(title = 'Density'))
    #fig
  })})
  
  output$PriorGenPlot1 <- plotly::renderPlotly({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    #source("Functions/multiroot.R",local = TRUE)
    if(input$lower.value=="Yes"){lower.value=TRUE}else{lower.value=FALSE}
    if(input$ID_MeanMedianMode=="Mean" & !is.null(input[["PriorMetric"]])){
      fb<<-findbeta2(themean =input$PriorMetric, percentile=input$Percentile1,lower.v=lower.value, percentile.value=input$PercentileValue1)
    }else if(input$ID_MeanMedianMode=="Median"){
      fb<<-findbeta2(themedian=input$PriorMetric, percentile=input$Percentile1,lower.v=lower.value, percentile.value=input$PercentileValue1)
    }else if(input$ID_MeanMedianMode=="Mode"){
      fb<<-findbeta2(themode =input$PriorMetric, percentile=input$Percentile1,lower.v=lower.value, percentile.value=input$PercentileValue1)
    }
    # find beta based on prior knowledge
    
    x<-seq(0,1,length.out = 10000)
    #plot(x,dbeta(x = x,dbeta(x = x,shape1 = fb$a,shape2 = fb$b),shape1 = fb$a,shape2 = fb$b),type = "l",lwd=3,ylab = "Density Beta")
    priors$prior<<-list(a=fb$a,b=fb$b)
    Prevalence<-dbeta(x = x,shape1 = priors$prior$a,shape2 = priors$prior$b)
    df <- data.frame(x, Prevalence)
    df <- gather(df, func, val, -x)
    gg <- ggplot(df, aes(x=x, y=val, group=func))
    gg <- gg + geom_line(aes(color=func),size=1.3) 
    gg <- gg + scale_y_continuous(expand=c(0, 0))
    gg <- gg + scale_color_manual(name="Prior(s)", 
                                  values=c("#e69138"),
                                  labels=c("Prevalence"))
    gg <- gg + labs(x="Density", y="Probability distribution functions")
    gg <- gg + theme_bw()
    gg <- gg + theme(panel.border=element_blank())
    gg <- gg + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ggplotly(gg)
    #fig <- plot_ly(x = ~x, y = ~dbeta(x = x,shape1 = fb$a,shape2 = fb$b), type = 'scatter', mode = 'lines', fill = 'tozeroy',
    #               width=400, height=400)
    #fig <- fig %>% layout(xaxis = list(title = 'Beta'), yaxis = list(title = 'Density'))
    #fig
  })})
  
  output$PriorGenPlot1_true <- plotly::renderPlotly({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    #source("Functions/multiroot.R",local = TRUE)
    if(input$lower.value=="Yes"){lower.value=TRUE}else{lower.value=FALSE}
    if(input$lower.value_SP=="Yes"){lower.value_SP=TRUE}else{lower.value_SP=FALSE}
    if(input$lower.value_SE=="Yes"){lower.value_SE=TRUE}else{lower.value_SE=FALSE}
    if(input$ID_MeanMedianMode=="Mean"){
      fb<<-findbeta2(themean =input$PriorMetric, percentile=input$Percentile1,lower.v=lower.value, percentile.value=input$PercentileValue1)
      fb_SE<<-findbeta2(themean =input$PriorMetric_SE, percentile=input$Percentile1_SE,lower.v=lower.value_SE, percentile.value=input$PercentileValue1_SE)
      fb_SP<<-findbeta2(themean =input$PriorMetric_SP, percentile=input$Percentile1_SP,lower.v=lower.value_SP, percentile.value=input$PercentileValue1_SP)
    }else if(input$ID_MeanMedianMode=="Median"){
      fb<<-findbeta2(themedian=input$PriorMetric, percentile=input$Percentile1,lower.v=lower.value, percentile.value=input$PercentileValue1)
      fb_SE<<-findbeta2(themedian =input$PriorMetric_SE, percentile=input$Percentile1_SE,lower.v=lower.value_SE, percentile.value=input$PercentileValue1_SE)
      fb_SP<<-findbeta2(themedian =input$PriorMetric_SP, percentile=input$Percentile1_SP,lower.v=lower.value_SP, percentile.value=input$PercentileValue1_SP)
    }else if(input$ID_MeanMedianMode=="Mode"){
      fb<<-findbeta2(themode =input$PriorMetric, percentile=input$Percentile1,lower.v=lower.value, percentile.value=input$PercentileValue1)
      fb_SE<<-findbeta2(themode =input$PriorMetric_SE, percentile=input$Percentile1_SE,lower.v=lower.value_SE, percentile.value=input$PercentileValue1_SE)
      fb_SP<<-findbeta2(themode =input$PriorMetric_SP, percentile=input$Percentile1_SP,lower.v=lower.value_SP, percentile.value=input$PercentileValue1_SP)
    }
    priors$prior<<-list(a=fb$a,b=fb$b,ase=fb_SE$a,bse=fb_SE$b,asp=fb_SP$a,bsp=fb_SP$b)
    # find beta based on prior knowledge
    x<-seq(0,1,length.out = 10000)
    Prevalence<-dbeta(x = x,shape1 = fb$a,shape2 = fb$b)
    Sensitivity<-dbeta(x = x,shape1 = fb_SE$a,shape2 = fb_SE$b)
    Specificity<-dbeta(x = x,shape1 = fb_SP$a,shape2 = fb_SP$b)
    df1 <- data.frame(x, Prevalence)
    df2 <- data.frame(x, Sensitivity, Specificity)
    df1 <- gather(df1, func, val, -x)
    df2 <- gather(df2, func, val, -x)
    
    gg1 <- ggplot(df1, aes(x=x, y=val, group=func))
    gg1 <- gg1 + geom_line(aes(color=func),size=1.3) 
    gg1 <- gg1 + scale_y_continuous(expand=c(0, 0))
    gg1 <- gg1 + scale_color_manual(name="Prior(s)", 
                                    values=c("#e69138"),
                                    labels=c("Prevalence"))
    gg1 <- gg1 + labs(x="Density", y="Probability distribution functions")
    gg1 <- gg1 + theme_bw()
    gg1 <- gg1 + theme(panel.border=element_blank())
    gg1 <- gg1 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    
    gg2 <- ggplot(df2, aes(x=x, y=val, group=func))
    gg2 <- gg2 + geom_line(aes(color=func),size=1.3) 
    gg2 <- gg2 + scale_y_continuous(expand=c(0, 0))
    gg2 <- gg2 + scale_color_manual(name="", 
                                    values=c("#f1c232","#6aa84f"),
                                    labels=c("Sensitivity","Specificity"))
    gg2 <- gg2 + labs(x="Density", y="Probability distribution functions")
    gg2 <- gg2 + theme_bw()
    gg2 <- gg2 + theme(panel.border=element_blank())
    gg2 <- gg2 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ply1 <- ggplotly(gg1)
    ply2 <- ggplotly(gg2)
    subplot(ply1, ply2, nrows=1,titleX = TRUE)
    
    #plot(x,dbeta(x = x,dbeta(x = x,shape1 = fb$a,shape2 = fb$b),shape1 = fb$a,shape2 = fb$b),type = "l",lwd=3,ylab = "Density Beta")
    # fig <- plot_ly(x = ~x, y = ~dbeta(x = x,shape1 = fb$a,shape2 = fb$b), type = 'scatter', mode = 'lines', fill = 'tozeroy',
    #                width=400, height=400)
    # fig <- fig %>% layout(xaxis = list(title = 'Beta'), yaxis = list(title = 'Density Prevalence'))
    # fig
    # 
    # 
    # fig_SE <- plot_ly(x = ~x, y = ~dbeta(x = x,shape1 = fb_SE$a, shape2 = fb_SE$b), type = 'scatter', mode = 'lines', fill = 'tozeroy',
    #                  width=400, height=400)
    # fig_SE <- fig_SE %>% layout(xaxis = list(title = 'Beta'), yaxis = list(title = 'Density (Sensitivity)'))
    # fig_SE 
    # 
    # fig_SP <- plot_ly(x = ~x, y = ~dbeta(x = x,shape1 = fb_SP$a, shape2 = fb_SP$b), type = 'scatter', mode = 'lines', fill = 'tozeroy',
    #                   width=400, height=400)
    # fig_SP <- fig_SP %>% layout(xaxis = list(title = 'Beta'), yaxis = list(title = 'Density (Specificity)'))
    # fig_SP 
    # 
    # fig_total<-subplot(fig,fig_SE,fig_SP, shareX = TRUE, shareY = TRUE)
    # fig_total
  })})
  
  output$PriorGenPlot1_true_zero <- plotly::renderPlotly({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    #source("Functions/multiroot.R",local = TRUE)
    if(input$lower.value=="Yes"){lower.value=TRUE}else{lower.value=FALSE}
    if(input$lower.value_SP=="Yes"){lower.value_SP=TRUE}else{lower.value_SP=FALSE}
    if(input$lower.value_SE=="Yes"){lower.value_SE=TRUE}else{lower.value_SE=FALSE}
    if(input$lower.value_tau0=="Yes"){lower.value_tau0=TRUE}else{lower.value_tau0=FALSE}
    if(input$ID_MeanMedianMode=="Mean"){
      fb<<-findbeta2(themean =input$PriorMetric, percentile=input$Percentile1,lower.v=lower.value, percentile.value=input$PercentileValue1)
      fb_SE<<-findbeta2(themean =input$PriorMetric_SE, percentile=input$Percentile1_SE,lower.v=lower.value_SE, percentile.value=input$PercentileValue1_SE)
      fb_SP<<-findbeta2(themean =input$PriorMetric_SP, percentile=input$Percentile1_SP,lower.v=lower.value_SP, percentile.value=input$PercentileValue1_SP)
      fb_tau0<<-findbeta2(themean =input$PriorMetric_tau0, percentile=input$Percentile1_tau0,lower.v=lower.value_tau0, percentile.value=input$PercentileValue1_tau0)
    }else if(input$ID_MeanMedianMode=="Median"){
      fb<<-findbeta2(themedian=input$PriorMetric, percentile=input$Percentile1,lower.v=lower.value, percentile.value=input$PercentileValue1)
      fb_SE<<-findbeta2(themedian =input$PriorMetric_SE, percentile=input$Percentile1_SE,lower.v=lower.value_SE, percentile.value=input$PercentileValue1_SE)
      fb_SP<<-findbeta2(themedian =input$PriorMetric_SP, percentile=input$Percentile1_SP,lower.v=lower.value_SP, percentile.value=input$PercentileValue1_SP)
      fb_tau0<<-findbeta2(themedian =input$PriorMetric_tau0, percentile=input$Percentile1_tau0,lower.v=lower.value_tau0, percentile.value=input$PercentileValue1_tau0)
    }else if(input$ID_MeanMedianMode=="Mode"){
      fb<<-findbeta2(themode =input$PriorMetric, percentile=input$Percentile1,lower.v=lower.value, percentile.value=input$PercentileValue1)
      fb_SE<<-findbeta2(themode =input$PriorMetric_SE, percentile=input$Percentile1_SE,lower.v=lower.value_SE, percentile.value=input$PercentileValue1_SE)
      fb_SP<<-findbeta2(themode =input$PriorMetric_SP, percentile=input$Percentile1_SP,lower.v=lower.value_SP, percentile.value=input$PercentileValue1_SP)
      fb_tau0<<-findbeta2(themode =input$PriorMetric_tau0, percentile=input$Percentile1_tau0,lower.v=lower.value_tau0, percentile.value=input$PercentileValue1_tau0)
    }
    
    
    priors$prior<<-list(a=fb$a,b=fb$b,ase=fb_SE$a,bse=fb_SE$b,asp=fb_SP$a,bsp=fb_SP$b,atau0=fb_tau0$a,btau0=fb_tau0$b)
    # find beta based on prior knowledge
    x<-seq(0,1,length.out = 10000)
    Prevalence<-dbeta(x = x,shape1 = fb$a,shape2 = fb$b)
    Prob.Zero.Prev<-dbeta(x = x,shape1 = fb_tau0$a,shape2 = fb_tau0$b)
    mean.Prob.Zero.Prev<<-round((fb_tau0$a)/(fb_tau0$a+fb_tau0$b),3)
    Sensitivity<-dbeta(x = x,shape1 = fb_SE$a,shape2 = fb_SE$b)
    Specificity<-dbeta(x = x,shape1 = fb_SP$a,shape2 = fb_SP$b)
    df1 <- data.frame(x, Prevalence,Prob.Zero.Prev)
    df2 <- data.frame(x, Sensitivity, Specificity)
    df1 <- gather(df1, func, val, -x)
    df2 <- gather(df2, func, val, -x)
    gg1 <- ggplot(df1, aes(x=x, y=val, group=func))
    gg1 <- gg1 + geom_line(aes(color=func),size=1.3) 
    gg1 <- gg1 + scale_y_continuous(expand=c(0, 0))
    gg1 <- gg1 + scale_color_manual(name="Prior(s)", 
                                    values=c("#e69138","#999999"),
                                    labels=c("Prevalence","Zero.Prob.Prev"))
    gg1 <- gg1 + labs(x="Density", y="Probability distribution functions")
    gg1 <- gg1 + theme_bw()
    gg1 <- gg1 + theme(panel.border=element_blank())
    gg1 <- gg1 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    
    gg2 <- ggplot(df2, aes(x=x, y=val, group=func))
    gg2 <- gg2 + geom_line(aes(color=func),size=1.3) 
    gg2 <- gg2 + scale_y_continuous(expand=c(0, 0))
    gg2 <- gg2 + scale_color_manual(name="", 
                                    values=c("#f1c232","#6aa84f"),
                                    labels=c("Sensitivity","Specificity"))
    gg2 <- gg2 + labs(x="Density", y="Probability distribution functions")
    gg2 <- gg2 + theme_bw()
    gg2 <- gg2 + theme(panel.border=element_blank())
    gg2 <- gg2 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ply1 <- ggplotly(gg1)
    ply2 <- ggplotly(gg2)
    subplot(ply1, ply2, nrows=1,titleX = TRUE)
  })})
  
  output$PriorGenPlot2 <- plotly::renderPlotly({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
  #  require(rootSolve);require(PriorGen) 
  # source("Functions/multiroot.R",local = TRUE)
   #source("Functions/findbetamupsi2.R",local = TRUE)
    if(input$lower.value2=="Yes"){lower.value2=TRUE}else{lower.value2=FALSE}
    fb<<-findbetamupsi2(themean=input$PriorMean2, percentile=input$Percentile2,
                        lower.v=lower.value2,percentile.value=input$PercentileValue2,
                        psi.percentile=input$PercentilePsi2, percentile.median=input$PercentileMedian2,
                        percentile95value=input$Percentile95value2)
    # fb<-NULL
    fb$aa<<-fb$atotalbeta; fb$bb<<-fb$btotalbeta
    fb$a<<-fb$abeta; fb$b<<-fb$bbeta
    fb$ag<<-fb$agamma; fb$bg<<-fb$bgamma
    priors$prior<<-list(aa=fb$atotalbeta,bb=fb$btotalbeta,
                        a=fb$abeta,b=fb$bbeta,
                        ag=fb$agamma,bg=fb$bgamma)
    
    x<-seq(0,1,length.out = 10000)
    #plot(x,dbeta(x = x,dbeta(x = x,shape1 = fb$a,shape2 = fb$b),shape1 = fb$a,shape2 = fb$b),type = "l",lwd=3,ylab = "Density Beta")
   # priors$prior<<-list(a=fb$a,b=fb$b)
    Prev.Prevalence<-dbeta(x = x,shape1 = fb$atotalbeta,shape2 = fb$btotalbeta)
    mu.Prevalence<-dbeta(x = x,shape1 = fb$abeta,shape2 = fb$bbeta)
    psi.Prevalence<-dbeta(x = x,shape1 = fb$agamma,shape2 = fb$bgamma)
    df1 <- data.frame(x, Prev.Prevalence)
    df1 <- gather(df1, func, val, -x)
    
    gg1 <- ggplot(df1, aes(x=x, y=val, group=func))
    gg1 <- gg1 + geom_line(aes(color=func),size=1.3) 
    gg1 <- gg1 + scale_y_continuous(expand=c(0, 0))
    gg1 <- gg1 + scale_color_manual(name="Prior(s)", 
                                    values=c("#e69138"),
                                    labels=c("Prev.Prevalence"))
    gg1 <- gg1 + labs(x="Density", y="Probability distribution functions")
    gg1 <- gg1 + theme_bw()
    gg1 <- gg1 + theme(panel.border=element_blank())
    gg1 <- gg1 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ply1 <- ggplotly(gg1)
    ply1
    
    #fig1 <- plot_ly(x = ~x, y = ~dbeta(x = x,shape1 = fb$atotalbeta, shape2 = fb$btotalbeta), type = 'scatter', mode = 'lines', fill = 'tozeroy', width=400, height=400)
    #fig1 <- fig1 %>% layout(xaxis = list(title = 'Beta'), yaxis = list(title = 'Density'))
    #fig1
    #fig2 <- plot_ly(x = ~x, y = ~dgamma(x = x,shape1 = fb$agamma,shape2 = fb$bgamma), type = 'scatter', mode = 'lines', fill = 'tozeroy', width=400, height=400)
    #fig2 <- fig2 %>% layout(xaxis = list(title = 'Gamma'), yaxis = list(title = 'Density'))
    
    #subplot(fig1, fig2, nrows = 2, margin = 0.04, heights = c(0.5, 0.5))
  })})
  
  output$PriorGenPlot2_true <- plotly::renderPlotly({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    #source("Functions/multiroot.R",local = TRUE)
    source("Functions/findbetamupsi2.R")
    if(input$lower.value2=="Yes"){lower.value2=TRUE}else{lower.value2=FALSE}
    if(input$lower.value2_SP=="Yes"){lower.value2_SP=TRUE}else{lower.value2_SP=FALSE}
    if(input$lower.value2_SE=="Yes"){lower.value2_SE=TRUE}else{lower.value2_SE=FALSE}

    fb<<-findbetamupsi2(themean=input$PriorMean2, percentile=input$Percentile2,
                        lower.v=lower.value2,percentile.value=input$PercentileValue2,
                        psi.percentile=input$PercentilePsi2, percentile.median=input$PercentileMedian2,
                        percentile95value=input$Percentile95value2)
    # fb<-NULL
    fb$aa<<-fb$atotalbeta; fb$bb<<-fb$btotalbeta
    fb$a<<-fb$abeta; fb$b<<-fb$bbeta
    fb$ag<<-fb$agamma; fb$bg<<-fb$bgamma
    
    # fb_SE<<-findbetamupsi2(themean=input$PriorMean2_SE, percentile=input$Percentile2_SE,
    #                        lower.v=lower.value2_SE,percentile.value=input$PercentileValue2_SE,
    #                        psi.percentile=input$PercentilePsi2_SE, percentile.median=input$PercentileMedian2_SE,
    #                        percentile95value=input$Percentile95value2_SE)
    # # fb_SE<-NULL
    # fb_SE$a<<-fb_SE$abeta; fb_SE$b<<-fb_SE$bbeta
    # 
    # fb_SP<<-findbetamupsi2(themean=input$PriorMean2_SP, percentile=input$Percentile2_SP,
    #                        lower.v=lower.value2_SP,percentile.value=input$PercentileValue2_SP,
    #                        psi.percentile=input$PercentilePsi2_SP, percentile.median=input$PercentileMedian2_SP,
    #                        percentile95value=input$Percentile95value2_SP)
    # # fb_SP<-NULL
    # fb_SP$a<<-fb_SP$abeta; fb_SP$b<<-fb_SP$bbeta
    
    
    fb_SE<<-findbeta2(themean =input$PriorMetric2_SE, percentile=input$Percentile2_SE,lower.v=lower.value2_SE, percentile.value=input$PercentileValue1_SE)
    fb_SP<<-findbeta2(themean =input$PriorMetric2_SP, percentile=input$Percentile2_SP,lower.v=lower.value2_SP, percentile.value=input$PercentileValue1_SP)
    
    
    
    priors$prior<<-list(aa=fb$atotalbeta,bb=fb$btotalbeta,
                        a=fb$abeta,b=fb$bbeta,
                        ag=fb$agamma,bg=fb$bgamma)
    
    x<-seq(0,1,length.out = 10000)
    mu.Prevalence<-dbeta(x = x,shape1 = fb$abeta,shape2 = fb$bbeta)
    psi.Prevalence<-dbeta(x = x,shape1 = fb$agamma,shape2 = fb$bgamma)
    Prev.Prevalence<-dbeta(x = x,shape1 = fb$atotalbeta, shape2 = fb$btotalbeta) 
    Sensitivity<-dbeta(x = x,shape1 = fb_SE$a,shape2 = fb_SE$b)
    Specificity<-dbeta(x = x,shape1 = fb_SP$a,shape2 = fb_SP$b)
    df1 <- data.frame(x, Prev.Prevalence)
    df2 <- data.frame(x, Sensitivity,Specificity)
    df1 <- gather(df1, func, val, -x)
    df2 <- gather(df2, func, val, -x)
    
    gg1 <- ggplot(df1, aes(x=x, y=val, group=func))
    gg1 <- gg1 + geom_line(aes(color=func),size=1.3) 
    gg1 <- gg1 + scale_y_continuous(expand=c(0, 0))
    gg1 <- gg1 + scale_color_manual(name="Prior(s)", 
                                    values=c("#e69138"),
                                    labels=c("Prevalence"))
    gg1 <- gg1 + labs(x="Density", y="Probability distribution functions")
    gg1 <- gg1 + theme_bw()
    gg1 <- gg1 + theme(panel.border=element_blank())
    gg1 <- gg1 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    gg1
    gg2 <- ggplot(df2, aes(x=x, y=val, group=func))
    gg2 <- gg2 + geom_line(aes(color=func),size=1.3)
    gg2 <- gg2 + scale_y_continuous(expand=c(0, 0))
    gg2 <- gg2 + scale_color_manual(name="",
                                    values=c("#f1c232","#6aa84f"),
                                    labels=c("Sensitivity","Specificity"))
    gg2 <- gg2 + labs(x="Density", y="Probability distribution functions")
    gg2 <- gg2 + theme_bw()
    gg2 <- gg2 + theme(panel.border=element_blank())
    gg2 <- gg2 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ply1 <- ggplotly(gg1)
    ply2 <- ggplotly(gg2)
    subplot(ply1, ply2, nrows=1,titleX = TRUE)
  })})
  
  output$PriorGenPlot2_true_zero <- plotly::renderPlotly({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    #source("Functions/multiroot.R",local = TRUE)
    source("Functions/findbetamupsi2.R")
    if(input$lower.value2=="Yes"){lower.value2=TRUE}else{lower.value2=FALSE}
    if(input$lower.value2_SP=="Yes"){lower.value2_SP=TRUE}else{lower.value2_SP=FALSE}
    if(input$lower.value2_SE=="Yes"){lower.value2_SE=TRUE}else{lower.value2_SE=FALSE}
    if(input$lower.value2_tau0=="Yes"){lower.value2_tau0=TRUE}else{lower.value2_tau0=FALSE}
    
    fb<<-findbetamupsi2(themean=input$PriorMean2, percentile=input$Percentile2,
                        lower.v=lower.value2,percentile.value=input$PercentileValue2,
                        psi.percentile=input$PercentilePsi2, percentile.median=input$PercentileMedian2,
                        percentile95value=input$Percentile95value2)
    # fb<-NULL
    fb$aa<<-fb$atotalbeta; fb$bb<<-fb$btotalbeta
    fb$a<<-fb$abeta; fb$b<<-fb$bbeta
    fb$ag<<-fb$agamma; fb$bg<<-fb$bgamma
    # 
    # fb_SE<<-findbetamupsi2(themean=input$PriorMean2_SE, percentile=input$Percentile2_SE,
    #                        lower.v=lower.value2_SE,percentile.value=input$PercentileValue2_SE,
    #                        psi.percentile=input$PercentilePsi2_SE, percentile.median=input$PercentileMedian2_SE,
    #                        percentile95value=input$Percentile95value2_SE)
    # # fb_SE<-NULL
    # fb_SE$a<<-fb_SE$abeta; fb_SE$b<<-fb_SE$bbeta
    # 
    # fb_SP<<-findbetamupsi2(themean=input$PriorMean2_SP, percentile=input$Percentile2_SP,
    #                        lower.v=lower.value2_SP,percentile.value=input$PercentileValue2_SP,
    #                        psi.percentile=input$PercentilePsi2_SP, percentile.median=input$PercentileMedian2_SP,
    #                        percentile95value=input$Percentile95value2_SP)
    # # fb_SP<-NULL
    # fb_SP$a<<-fb_SP$abeta; fb_SP$b<<-fb_SP$bbeta
    # 
    # 
    # fb_tau0<<-findbetamupsi2(themean=input$PriorMean2_tau0, percentile=input$Percentile2_tau0,
    #                        lower.v=lower.value2_tau0,percentile.value=input$PercentileValue2_tau0,
    #                        psi.percentile=input$PercentilePsi2_tau0, percentile.median=input$PercentileMedian2_tau0,
    #                        percentile95value=input$Percentile95value2_tau0)
    # # fb_tau0<-NULL
    # fb_tau0$a<<-fb_tau0$abeta; fb_tau0$b<<-fb_tau0$bbeta
    # 
    fb_SE<<-findbeta2(themean =input$PriorMetric2_SE, percentile=input$Percentile2_SE,lower.v=lower.value2_SE, percentile.value=input$PercentileValue1_SE)
    fb_SP<<-findbeta2(themean =input$PriorMetric2_SP, percentile=input$Percentile2_SP,lower.v=lower.value2_SP, percentile.value=input$PercentileValue1_SP)
    fb_tau0<<-findbeta2(themean =input$PriorMetric2_tau0, percentile=input$Percentile2_tau0,lower.v=lower.value2_tau0, percentile.value=input$PercentileValue1_tau0)
    
    
    priors$prior<<-list(aa=fb$atotalbeta,bb=fb$btotalbeta,
                        a=fb$abeta,b=fb$bbeta,
                        ag=fb$agamma,bg=fb$bgamma)
    
    x<-seq(0,1,length.out = 10000)
    Prev.Prevalence<-dbeta(x = x,shape1 = fb$atotalbeta,shape2 = fb$btotalbeta)
    Prob.Zero.Prev<-dbeta(x = x,shape1 = fb_tau0$a,shape2 = fb_tau0$b)
    mean.Prob.Zero.Prev<<-round((fb_tau0$a)/(fb_tau0$a+fb_tau0$b),3)
    
    Sensitivity<-dbeta(x = x,shape1 = fb_SE$a,shape2 = fb_SE$b)
    Specificity<-dbeta(x = x,shape1 = fb_SP$a,shape2 = fb_SP$b)
    df1 <- data.frame(x, Prev.Prevalence, Prob.Zero.Prev)
    df2 <- data.frame(x, Sensitivity,Specificity)
    df1 <- gather(df1, func, val, -x)
    df2 <- gather(df2, func, val, -x)
    
    gg1 <- ggplot(df1, aes(x=x, y=val, group=func))
    gg1 <- gg1 + geom_line(aes(color=func),size=1.3) 
    gg1 <- gg1 + scale_y_continuous(expand=c(0, 0))
    gg1 <- gg1 + scale_color_manual(name="Prior(s)", 
                                    values=c("#e69138","#999999"),
                                    labels=c("Prev.Prevalence","Zero.Prob.Prev"))
    gg1 <- gg1 + labs(x="Density", y="Probability distribution functions")
    gg1 <- gg1 + theme_bw()
    gg1 <- gg1 + theme(panel.border=element_blank())
    gg1 <- gg1 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    gg1
    gg2 <- ggplot(df2, aes(x=x, y=val, group=func))
    gg2 <- gg2 + geom_line(aes(color=func),size=1.3)
    gg2 <- gg2 + scale_y_continuous(expand=c(0, 0))
    gg2 <- gg2 + scale_color_manual(name="",
                                    values=c("#f1c232","#6aa84f"),
                                    labels=c("Sensitivity","Specificity"))
    gg2 <- gg2 + labs(x="Density", y="Probability distribution functions")
    gg2 <- gg2 + theme_bw()
    gg2 <- gg2 + theme(panel.border=element_blank())
    gg2 <- gg2 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ply1 <- ggplotly(gg1)
    ply2 <- ggplotly(gg2)
    subplot(ply1, ply2, nrows=1,titleX = TRUE)
  })})
  
  output$PriorGenPlot3 <- plotly::renderPlotly({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    #source("Functions/multiroot.R",local = TRUE)
    # Find alpha and beta of a beta dist based on percentiles
    fb<<-findbetaqq2(percentile.value1=input$PercentileValue3_1,percentile1=input$Percentile3_1,
                     percentile.value2=input$PercentileValue3_2,percentile2=input$Percentile3_2)
    priors$prior<<-list(a=fb$a,b=fb$b)
    x<-seq(0,1,length.out = 10000)

    Prevalence<-dbeta(x = x,shape1 = priors$prior$a,shape2 = priors$prior$b)
    df <- data.frame(x, Prevalence)
    df <- gather(df, func, val, -x)
    gg <- ggplot(df, aes(x=x, y=val, group=func))
    gg <- gg + geom_line(aes(color=func),size=1.3) 
    gg <- gg + scale_y_continuous(expand=c(0, 0))
    gg <- gg + scale_color_manual(name="Prior(s)", 
                                  values=c("#e69138"),
                                  labels=c("Prevalence"))
    gg <- gg + labs(x="Density", y="Probability distribution functions")
    gg <- gg + theme_bw()
    gg <- gg + theme(panel.border=element_blank(),plot.margin=unit(c(1,1,1.5,1.2),"cm"))
    gg <- gg + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ggplotly(gg)
  })})
  
  output$PriorGenPlot3_true <- plotly::renderPlotly({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
   #source("Functions/multiroot.R",local = TRUE)
    # Find alpha and beta of a beta dist based on percentiles Prevalence
    fb<<-findbetaqq2(percentile.value1=input$PercentileValue3_1,percentile1=input$Percentile3_1,
                     percentile.value2=input$PercentileValue3_2,percentile2=input$Percentile3_2)
    # Find alpha and beta of a beta dist based on percentiles Sensitivity
    fb_SE<<-findbetaqq2(percentile.value1=input$PercentileValue3_1_SE,percentile1=input$Percentile3_1_SE,
                        percentile.value2=input$PercentileValue3_2_SE,percentile2=input$Percentile3_2_SE)
    # Find alpha and beta of a beta dist based on percentiles Specificity
    fb_SP<<-findbetaqq2(percentile.value1=input$PercentileValue3_1_SP,percentile1=input$Percentile3_1_SP,
                        percentile.value2=input$PercentileValue3_2_SP,percentile2=input$Percentile3_2_SP)
    priors$prior<<-list(a=fb$a,b=fb$b, ase=fb_SE$a,bse=fb_SE$b, asp=fb_SP$a,bsp=fb_SP$b)
    
    x<-seq(0,1,length.out = 10000)
    Prevalence<-dbeta(x = x,shape1 = fb$a,shape2 = fb$b)
    Sensitivity<-dbeta(x = x,shape1 = fb_SE$a,shape2 = fb_SE$b)
    Specificity<-dbeta(x = x,shape1 = fb_SP$a,shape2 = fb_SP$b)
    df1 <- data.frame(x, Prevalence)
    df2 <- data.frame(x, Sensitivity, Specificity)
    df1 <- gather(df1, func, val, -x)
    df2 <- gather(df2, func, val, -x)
    
    gg1 <- ggplot(df1, aes(x=x, y=val, group=func))
    gg1 <- gg1 + geom_line(aes(color=func),size=1.3) 
    gg1 <- gg1 + scale_y_continuous(expand=c(0, 0))
    gg1 <- gg1 + scale_color_manual(name="Prior(s)", 
                                    values=c("#e69138"),
                                    labels=c("Prevalence"))
    gg1 <- gg1 + labs(x="Density", y="Probability distribution functions")
    gg1 <- gg1 + theme_bw()
    gg1 <- gg1 + theme(panel.border=element_blank())
    gg1 <- gg1 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    
    gg2 <- ggplot(df2, aes(x=x, y=val, group=func))
    gg2 <- gg2 + geom_line(aes(color=func),size=1.3) 
    gg2 <- gg2 + scale_y_continuous(expand=c(0, 0))
    gg2 <- gg2 + scale_color_manual(name="", 
                                    values=c("#f1c232","#6aa84f"),
                                    labels=c("Sensitivity","Specificity"))
    gg2 <- gg2 + labs(x="Density", y="Probability distribution functions")
    gg2 <- gg2 + theme_bw()
    gg2 <- gg2 + theme(panel.border=element_blank())
    gg2 <- gg2 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ply1 <- ggplotly(gg1)
    ply2 <- ggplotly(gg2)
    subplot(ply1, ply2, nrows=1,titleX = TRUE)
  })})
  
  output$PriorGenPlot3_true_zero <- plotly::renderPlotly({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    #source("Functions/multiroot.R",local = TRUE)
    # Find alpha and beta of a beta dist based on percentiles Prevalence
    fb<<-findbetaqq2(percentile.value1=input$PercentileValue3_1,percentile1=input$Percentile3_1,
                     percentile.value2=input$PercentileValue3_2,percentile2=input$Percentile3_2)
    # Find alpha and beta of a beta dist based on percentiles Sensitivity
    fb_SE<<-findbetaqq2(percentile.value1=input$PercentileValue3_1_SE,percentile1=input$Percentile3_1_SE,
                        percentile.value2=input$PercentileValue3_2_SE,percentile2=input$Percentile3_2_SE)
    # Find alpha and beta of a beta dist based on percentiles Specificity
    fb_SP<<-findbetaqq2(percentile.value1=input$PercentileValue3_1_SP,percentile1=input$Percentile3_1_SP,
                        percentile.value2=input$PercentileValue3_2_SP,percentile2=input$Percentile3_2_SP)
    # Find alpha and beta of a beta dist based on percentiles of zero probability
    fb_tau0<<-findbetaqq2(percentile.value1=input$PercentileValue3_1_tau0,percentile1=input$Percentile3_1_tau0,
                        percentile.value2=input$PercentileValue3_2_tau0,percentile2=input$Percentile3_2_tau0)
    priors$prior<<-list(a=fb$a,b=fb$b, ase=fb_SE$a,bse=fb_SE$b, asp=fb_SP$a,bsp=fb_SP$b,atau0=fb_tau0$a,btau0=fb_tau0$b)
    
    x<-seq(0,1,length.out = 10000)
    Prevalence<-dbeta(x = x,shape1 = fb$a,shape2 = fb$b)
    Prob.Zero.Prev<-dbeta(x = x,shape1 = fb_tau0$a,shape2 = fb_tau0$b)
    mean.Prob.Zero.Prev<<-round((fb_tau0$a)/(fb_tau0$a+fb_tau0$b),3)
    Sensitivity<-dbeta(x = x,shape1 = fb_SE$a,shape2 = fb_SE$b)
    Specificity<-dbeta(x = x,shape1 = fb_SP$a,shape2 = fb_SP$b)
    df1 <- data.frame(x, Prevalence,Prob.Zero.Prev)
    df2 <- data.frame(x, Sensitivity, Specificity)
    df1 <- gather(df1, func, val, -x)
    df2 <- gather(df2, func, val, -x)
    
    gg1 <- ggplot(df1, aes(x=x, y=val, group=func))
    gg1 <- gg1 + geom_line(aes(color=func),size=1.3) 
    gg1 <- gg1 + scale_y_continuous(expand=c(0, 0))
    gg1 <- gg1 + scale_color_manual(name="Prior(s)", 
                                    values=c("#e69138","#999999"),
                                    labels=c("Prevalence","Zero.Prob.Prev"))
    gg1 <- gg1 + labs(x="Density", y="Probability distribution functions")
    gg1 <- gg1 + theme_bw()
    gg1 <- gg1 + theme(panel.border=element_blank())
    gg1 <- gg1 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    
    gg2 <- ggplot(df2, aes(x=x, y=val, group=func))
    gg2 <- gg2 + geom_line(aes(color=func),size=1.3) 
    gg2 <- gg2 + scale_y_continuous(expand=c(0, 0))
    gg2 <- gg2 + scale_color_manual(name="", 
                                    values=c("#f1c232","#6aa84f"),
                                    labels=c("Sensitivity","Specificity"))
    gg2 <- gg2 + labs(x="Density", y="Probability distribution functions")
    gg2 <- gg2 + theme_bw()
    gg2 <- gg2 + theme(panel.border=element_blank())
    gg2 <- gg2 + theme(axis.line=element_line(size=0.15, color="#2b2b2b"))
    ply1 <- ggplotly(gg1)
    ply2 <- ggplotly(gg2)
    subplot(ply1, ply2, nrows=1,titleX = TRUE)
  })})
  
  
  #------- Interactive conditional interface for prior elication and ploting ------#
  output$Priors_fb<-renderUI({
    if(input$ID_Informative=="No"){
      source("Functions/Interface_findBeta_NonInformative.R",local = TRUE)$value
    }else{
      if(input$ID_MeanMedianMode=="Percentiles"){
        if(input$ID_TrueApp!="True prevalence"){
          source("Functions/Interface_findBetaqq2.R",local = TRUE)$value
        }else{
          if(input$ID_ZeroPrevalence=="Yes"){
            source("Functions/Interface_findBetaqq2_SE_SP_tau0.R",local = TRUE)$value
          }else{
            source("Functions/Interface_findBetaqq2_SE_SP.R",local = TRUE)$value
          }
        }
      }else{
        if(input$ID_SingleMultiple=="Single"){
          if(input$ID_TrueApp!="True prevalence"){
            source("Functions/Interface_findBeta.R",local = TRUE)$value
          }else{
            if(input$ID_ZeroPrevalence=="Yes"){
              source("Functions/Interface_findBeta_SE_SP_tau0.R",local = TRUE)$value
            }else{
              source("Functions/Interface_findBeta_SE_SP.R",local = TRUE)$value
            }
          }
        }else if(input$ID_SingleMultiple=="Multiple"){
          if(input$ID_TrueApp=="True prevalence"){
            if(input$ID_ZeroPrevalence=="Yes"){
              source("Functions/Interface_findBetamupsi2_SE_SP_tau0.R",local = TRUE)$value
            }else if(input$ID_ZeroPrevalence=="No"){
              source("Functions/Interface_findBetamupsi2_SE_SP.R",local = TRUE)$value
            }
          }else{
            source("Functions/Interface_findBetamupsi2.R",local = TRUE)$value
          }
        }
      }
    }
    
  })
  
  observeEvent(once = TRUE,input$buttonPrior,{
    output$Priors_Plot_Sum_fb <- renderUI({
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...', value = 1, {
      if(priors$SetupPriors==TRUE){
        if(input$ID_Informative=="No"){
          plotlyOutput("PriorGenPlot0")
          
        }else{
          if(input$ID_MeanMedianMode=="Percentiles"){
            if(input$ID_TrueApp!="True prevalence"){
              plotlyOutput("PriorGenPlot3")
            }else{
              if(input$ID_ZeroPrevalence=="No"){
                plotlyOutput(width = '100%',height = '100%',"PriorGenPlot3_true")
              }else{
                plotlyOutput(width = '100%',height = '100%',"PriorGenPlot3_true_zero")
              }
            }
          }else{
            if(input$ID_SingleMultiple=="Single"){
              if(input$ID_TrueApp!="True prevalence"){
                plotlyOutput("PriorGenPlot1")
              }else{
                if(input$ID_ZeroPrevalence=="No"){
                  plotlyOutput(width = '100%',height = '100%',"PriorGenPlot1_true")
                }else{
                  plotlyOutput(width = '100%',height = '100%',"PriorGenPlot1_true_zero")
                }
              }
            }else if(input$ID_SingleMultiple=="Multiple"){
              if(input$ID_TrueApp!="True prevalence"){
                plotlyOutput("PriorGenPlot2")
              }else{
                if(input$ID_ZeroPrevalence=="Yes"){
                  plotlyOutput(width = '100%',height = '100%',"PriorGenPlot2_true_zero")
                }else if(input$ID_ZeroPrevalence=="No"){
                  plotlyOutput(width = '100%',height = '100%',"PriorGenPlot2_true")
                }
              }
            }
          }
        }
      }
      
                   }) 
    })
  })
  
   
  #----- Plot inference for each model ------#
  
  output$APpre_Plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    source("Functions/gss.R",local=TRUE)
    source("Functions/Jags_ApparentPre.R",local=TRUE)$value
    # Simple plot
    #plot(density(Model1.mcmc[[1]][,1]),xlim=c(0,1),lwd=5, main = "Posterior (black) and Prior (red) distribution of APpre")
    #lines(1:1000/1000,dbeta(seq(0,1,length.out = 1000),a,b),type = "l",col="red",lwd=5)
    Model1.mcmc_df<-data.frame(Model1.mcmc[[1]])
    post <-  data.frame(density=data.frame(density=Model1.mcmc_df$main.ap))
    pri <-  data.frame(density=rbeta(10000,shape1 = fb$a,shape2=fb$b))
    Lik <- data.frame(density=rbinom(n = 10000, size =  input$n, prob = input$y/input$n)/input$n)
    post$Distribution <- 'posterior' ; pri$Distribution <- 'prior' ; Lik$Distribution <- 'likelihood'
    triple <- rbind(post, pri, Lik)
    
    p1<-ggplot(triple, aes(density, fill = Distribution)) + geom_density(alpha = 0.2,adjust = 1.5)+
      xlim(0, 1)+ theme(legend.position="top")
    S <- ggmcmc::ggs(Model1.mcmc)
    levels(S$Parameter)[levels(S$Parameter)=="main.ap"]<-"Apparent p (main)"
    p2<-ggs_traceplot(S,family = "main")
    Sless<-data.frame(S[S$Parameter=="plessthanSetvalue",])
    xinput_Table<-data.frame(table(factor(Sless$value,levels = c(0,1), labels=c("No","Yes"))))
    p3<-ggplot(data=xinput_Table, aes(x=Var1,y = Freq)) +
      geom_bar(stat="identity", fill="steelblue")+ 
      labs(title="Is the posterior prevalence lower than the set value?",
           x="Is it?", y = "MCMC samples")
    pright<-grid.arrange(p1,ncol=1)
    pleft <-grid.arrange(p2,p3,ncol=1)
    gridExtra::grid.arrange(pright,pleft,ncol=2)
                 })
  },width = 'auto', heigh='auto')
  
  output$TRpre_Plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    source("Functions/gss.R",local=TRUE)
    source("Functions/Jags_TruePre.R",local=TRUE)$value
    # Simple plot
    #plot(density(Model1.mcmc[[1]][,1]),ylim=c(0,100),xlim=c(0,1),lwd=5, main = "Posterior (black) and Prior (red) distribution of APpre")
    #lines(1:1000/1000,dbeta(seq(0,1,length.out = 1000),a,b),type = "l",col="red",lwd=5)
    # ggplot
    Model1.mcmc_df<-data.frame(Model1.mcmc[[1]])
    post <-  data.frame(density=data.frame(density=Model1.mcmc_df$main.ap))
    pri <-  data.frame(density=rbeta(10000,shape1 = priors$prior$a,shape2=priors$prior$b))
    Lik <- data.frame(density=rbinom(n = 10000, size =  input$n, prob = input$y/input$n)/input$n)
    post$Distribution <- 'posterior' ; pri$Distribution <- 'prior' ; Lik$Distribution <- 'likelihood'
    triple <- rbind(post, pri, Lik)
    p1<-ggplot(triple, aes(density, fill = Distribution)) + geom_density(alpha = 0.2)+
      xlim(0, 1)+ theme(legend.position="top")
    S <- ggmcmc::ggs(Model1.mcmc)
    levels(S$Parameter)[levels(S$Parameter)=="main.ap"]<-"True p (main)"
    levels(S$Parameter)[levels(S$Parameter)=="main.Se"]<-"Sensitivity (main)"
    levels(S$Parameter)[levels(S$Parameter)=="main.Sp"]<-"Specificity (main)"
    p2<-ggs_traceplot(S,family = "main")
    Sless<-data.frame(S[S$Parameter=="plessthanSetvalue",])
    xinput_Table<-data.frame(table(factor(Sless$value,levels = c(0,1), labels=c("No","Yes"))))
    p3<-ggplot(data=xinput_Table, aes(x=Var1,y = Freq)) +
      geom_bar(stat="identity", fill="steelblue")+ 
      labs(title="Is the posterior prevalence lower than the set value?",
           x="Is it?", y = "MCMC samples")
    pright<-grid.arrange(p1,ncol=1)
    pleft <-grid.arrange(p2,p3,ncol=1)
    gridExtra::grid.arrange(pright,pleft,ncol=2)
    #  ggs_pairs(S, lower = list(continuous = "density"))
                 })
  },width = 'auto', heigh='auto')
  
  output$TRpreZero_Plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    source("Functions/gss.R",local=TRUE)
    source("Functions/Jags_TruePreZero.R",local=TRUE)$value
    #simple plot
    #plot(density(Model1.mcmc[[1]][,5]),ylim=c(0,100),xlim=c(0,1),lwd=5, main = "Posterior (black) and Prior (red) distribution of TRpre")
    #lines(1:1000/1000,dbeta(seq(0,1,length.out = 1000),1.80,21),type = "l",col="red",lwd=5)
    #ggplot 
    Model1.mcmc_df<-data.frame(Model1.mcmc[[1]])
    post <-  data.frame(density=data.frame(density=Model1.mcmc_df$main.ap))
    pri <-  data.frame(density=rbeta(10000,shape1 = priors$prior$a,shape2=priors$prior$b))
    Lik <- data.frame(density=rbinom(n = 10000, size =  input$n, prob = input$y/input$n)/input$n)
    post$Distribution <- 'posterior' ; pri$Distribution <- 'prior' ; Lik$Distribution <- 'likelihood'
    triple <- rbind(post, pri, Lik)
    p1<-ggplot(triple, aes(density, fill = Distribution)) + geom_density(alpha = 0.2)+
      xlim(0, 1)+ theme(legend.position="top")
    S <- ggmcmc::ggs(Model1.mcmc)
    levels(S$Parameter)[levels(S$Parameter)=="main.ap"]<-"True p (main)"
    levels(S$Parameter)[levels(S$Parameter)=="main.Se"]<-"Sensitivity (main)"
    levels(S$Parameter)[levels(S$Parameter)=="main.Sp"]<-"Specificity (main)"
    
    p2<-ggs_traceplot(S,family = "main")
    Sless<-data.frame(S[S$Parameter=="plessthanSetvalue",])
    xinput_Table<-data.frame(table(factor(Sless$value,levels = c(0,1), labels=c("No","Yes"))))
    p3<-ggplot(data=xinput_Table, aes(x=Var1,y = Freq)) +
      geom_bar(stat="identity", fill="steelblue")+ 
      labs(title="Is the posterior prevalence lower than the set value?",
           x="Is it?", y = "MCMC samples")
    pright<-grid.arrange(p1,ncol=1)
    pleft <-grid.arrange(p2,p3,ncol=1)
    gridExtra::grid.arrange(pright,pleft,ncol=2)
    #  ggs_pairs(S, lower = list(continuous = "density"))
                 })
  },width = 'auto', heigh='auto')
  
  output$MultTRpre_Plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    source("Functions/gss.R",local=TRUE)
    source("Functions/Jags_MultipleGroupsPre.R",local=TRUE)$value
    
    Model1.mcmc_df<<-data.frame(Model1.mcmc[[1]])
    post <-  data.frame(density=data.frame(density=Model1.mcmc_df$main.ap))
    pri <-  data.frame(density=rbeta(10000,shape1 = fb$atotalbeta,shape2=fb$btotalbeta))
    post$Distribution <- 'posterior' ; pri$Distribution <- 'prior' 
    triple <- rbind(post, pri)
    p1<-ggplot(triple, aes(density, fill = Distribution)) + geom_density(alpha = 0.2,adjust=1) +
      xlim(0, 1) + theme(legend.position="top") + scale_fill_brewer(palette="Dark2")
    
    S <- ggmcmc::ggs(Model1.mcmc)
    levels(S$Parameter)[levels(S$Parameter)=="main.ap"]<-"True prevalence"
    levels(S$Parameter)[levels(S$Parameter)=="main.tau0"]<-"Pr(zero prevalence)"
    p2<-ggs_traceplot(S,family = "main")
    #    SPre<-get_family(D = S,family = "pre")
    #    p3<-ggs_density(SPre)
    
    Sless<-data.frame(S[S$Parameter=="plessthanSetvalue",])
    xinput_Table<-data.frame(table(factor(Sless$value,levels = c(0,1), labels=c("No","Yes"))))
    p3<-ggplot(data=xinput_Table, aes(x=Var1,y = Freq)) +
      geom_bar(stat="identity", fill="steelblue")+ 
      labs(title="Is the posterior prevalence lower than the set value?",
           x="Is it?", y = "MCMC samples")
    
    pright<-grid.arrange(p1,ncol=1)
    pleft <-grid.arrange(p2,p3,ncol=1)
    gridExtra::grid.arrange(pright,pleft,ncol=2)
})
  },width = 'auto', heigh='auto')

  output$MultTRpreZero_Plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    source("Functions/gss.R",local=TRUE)
    source("Functions/Jags_MultipleGroupsPreZero.R",local=TRUE)$value
    #Simple plot 
    #plot(density(Model1.mcmc[[1]][,1]),ylim=c(0,100),xlim=c(0,1),lwd=5, main = "Posterior (black) and Prior (red) distribution of APpre")
    #lines(1:1000/1000,dbeta(seq(0,1,length.out = 1000),a,b),type = "l",col="red",lwd=5)
    #ggplot
    Model1.mcmc_df<<-data.frame(Model1.mcmc[[1]])
    post <-  data.frame(density=data.frame(density=Model1.mcmc_df$main.ap))
    pri <-  data.frame(density=rbeta(10000,shape1 = fb$atotalbeta,shape2=fb$btotalbeta))
    #Lik <- data.frame(density=rbinom(n = 10000, size =  sum(input$n), prob = sum(input$y/input$n))/sum(input$n))
    post$Distribution <- 'posterior' ; pri$Distribution <- 'prior' #; Lik$Distribution <- 'likelihood'
    triple <- rbind(post, pri)#, Lik)
    p1<-ggplot(triple, aes(density, fill = Distribution)) + geom_density(alpha = 0.2,adjust=1) +
      xlim(0, 1) + theme(legend.position="top") + scale_fill_brewer(palette="Dark2")
    
    S <- ggmcmc::ggs(Model1.mcmc)
    levels(S$Parameter)[levels(S$Parameter)=="main.ap"]<-"True prevalence"
    levels(S$Parameter)[levels(S$Parameter)=="main.tau0"]<-"Pr(zero prevalence)"
    p2<-ggs_traceplot(S,family = "main")
#    SPre<-get_family(D = S,family = "pre")
#    p3<-ggs_density(SPre)
    
    Sless<-data.frame(S[S$Parameter=="plessthanSetvalue",])
    xinput_Table<-data.frame(table(factor(Sless$value,levels = c(0,1), labels=c("No","Yes"))))
    p3<-ggplot(data=xinput_Table, aes(x=Var1,y = Freq)) +
      geom_bar(stat="identity", fill="steelblue")+ 
      labs(title="Is the posterior prevalence lower than the set value?",
           x="Is it?", y = "MCMC samples")
    
    pright<-grid.arrange(p1,ncol=1)
    pleft <-grid.arrange(p2,p3,ncol=1)
    gridExtra::grid.arrange(pright,pleft,ncol=2)
    
    #   https://cran.r-project.org/web/packages/ggmcmc/vignettes/using_ggmcmc.html
    #   S.full <- ggs(radon$s.radon, par_labels=L.radon.intercepts, family="^alpha")
    #   ggs_caterpillar(S.full)  },width = 400, heigh=400)
    # Z <- data.frame(
    #   Parameter=paste("alpha[", radon$counties$id.county, "]", sep=""),
    #   value=radon$counties$uranium)
    # ggs_caterpillar(ggs(radon$s.radon, family="^alpha"), X=Z, horizontal=FALSE)
                 })
  },width = 'auto', heigh='auto')
  
  output$MultTRapp_Plot <- renderPlot({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 1, {
    source("Functions/gss.R",local=TRUE)
    if(input$ID_Informative=="Yes"){
      source("Functions/Jags_MultipleGroupsApp.R",local=TRUE)$value
    }else{
      source("Functions/Jags_MultipleGroupsApp_NonInf.R",local=TRUE)$value
    }
    #Simple plot 
    #Model1.mcmc<-Model1.mcmc
    #plot(density(Model1.mcmc[[1]][,1]),ylim=c(0,100),xlim=c(0,1),lwd=5, main = "Posterior (black) and Prior (red) distribution of APpre")
    #lines(1:1000/1000,dbeta(seq(0,1,length.out = 1000),a,b),type = "l",col="red",lwd=5)
    #ggplot
    Model1.mcmc_df<<-data.frame(Model1.mcmc[[1]])
    post <-  data.frame(density=data.frame(density=Model1.mcmc_df$main.ap))
    pri <-  data.frame(density=rbeta(10000,shape1 = fb$atotalbeta,shape2=fb$btotalbeta))
    #mn=mean((sum(dataset()$positive/dataset()$n))/sum(dataset()$n))
    #tau=sd(dataset()$positive/dataset()$n)
    #Lik <- data.frame(density=rbinom(n = 10000, size =  sum(dataset()$n), 
    #                                 prob = rbeta(10000,shape1 = mn*tau,
    #                                              shape2 = (1-mn)*tau)
    #                                 ))
    post$Distribution <- 'posterior' ; pri$Distribution <- 'prior'# ; Lik$Distribution <- 'likelihood'
    triple <- rbind(post, pri)#, Lik)
     p1<-ggplot(triple, aes(density, fill = Distribution)) + geom_density(alpha = 0.2,adjust=1) +
       xlim(0, 1) + theme(legend.position="top") + scale_fill_brewer(palette="Dark2")
   
     S <- ggmcmc::ggs(Model1.mcmc)
     levels(S$Parameter)[levels(S$Parameter)=="main.ap"]<-"Apparent prevalence"
     p2<-ggs_traceplot(S,family = "prevalence")
     
     SPre<-get_family(D = S,family = "pre")
     #p3<-ggs_density(SPre)
     Sless<-data.frame(S[S$Parameter=="plessthanSetvalue",])
     xinput_Table<-data.frame(table(factor(Sless$value,levels = c(0,1), labels=c("No","Yes"))))
     p3<-ggplot(data=xinput_Table, aes(x=Var1,y = Freq)) +
       geom_bar(stat="identity", fill="steelblue")+ 
       labs(title="Is the posterior prevalence lower than the set value?",
            x="Is it?", y = "MCMC samples")
     
     pright<-grid.arrange(p1,ncol=1)
     pleft <-grid.arrange(p2,p3,ncol=1)
     gridExtra::grid.arrange(pright,pleft,ncol=2)
     #   https://cran.r-project.org/web/packages/ggmcmc/vignettes/using_ggmcmc.html
    #   S.full <- ggs(radon$s.radon, par_labels=L.radon.intercepts, family="^alpha")
    #   ggs_caterpillar(S.full)  },width = 400, heigh=400)
    # Z <- data.frame(
    #   Parameter=paste("alpha[", radon$counties$id.county, "]", sep=""),
    #   value=radon$counties$uranium)
    # ggs_caterpillar(ggs(radon$s.radon, family="^alpha"), X=Z, horizontal=FALSE)
                 })
  },width = 'auto', heigh='auto')
  
  #----- Interactive conditional interface and inference plot for each model ------#
  output$Bayesian_fb<-renderUI({
    if(input$ID_Informative=="No"){
      if(input$ID_SingleMultiple=="Single"){
        source("Functions/Interface_Jags_ApparentPre.R",local = TRUE)$value # Done
      }else{
        source("Functions/Interface_Jags_MultipleGroupsAppPre.R",local = TRUE)$value # Done
      }
    }else{
      if(input$ID_TrueApp=="Apparent prevalence" & input$ID_SingleMultiple=="Single" & input$ID_ZeroPrevalence=="No"){
        source("Functions/Interface_Jags_ApparentPre.R",local = TRUE)$value # Done
      }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Single" & input$ID_ZeroPrevalence=="No"){
        source("Functions/Interface_Jags_TruePre.R",local = TRUE)$value # Done
        #    }else if(input$ID_TrueApp=="Apparent prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="No"){
        #      source("Functions/Interface_Jags_MultipleGroupsApPreNozero.R",local = TRUE) # File ready
        #    }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="No"){
        #      source("Functions/Interface_Jags_MultipleGroupsTruePreNozero.R",local = TRUE) # File ready
      }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Single" & input$ID_ZeroPrevalence=="Yes"){
        source("Functions/Interface_Jags_TruePreZero.R",local = TRUE)$value # Done
      }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="Yes"){
        source("Functions/Interface_Jags_MultipleGroupsTruePreZero.R",local = TRUE)$value # Done
      }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="No"){
        source("Functions/Interface_Jags_MultipleGroupsTruePre.R",local = TRUE)$value # Done
      }else if(input$ID_TrueApp!="True prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="No"){
        source("Functions/Interface_Jags_MultipleGroupsAppPre.R",local = TRUE)$value # Done 
      }
    }
  })
  
  observeEvent(input$buttonFixModel, {
  output$APar1_Plot_fb <- renderUI({
    if(input$ID_Informative=="No"){
      if(input$ID_SingleMultiple=="Single"){
        plotOutput("APpre_Plot")
      }else{
        plotOutput("MultTRapp_Plot")
      }
    }else{
    if(input$ID_TrueApp=="Apparent prevalence" & input$ID_SingleMultiple=="Single" & input$ID_ZeroPrevalence=="No"){
      plotOutput("APpre_Plot")
    }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Single" & input$ID_ZeroPrevalence=="No"){
      plotOutput("TRpre_Plot")
      #    }else if(input$ID_TrueApp=="Apparent prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="No"){
      #      source("Functions/Interface_Jags_MultipleGroupsApPreNozero.R",local = TRUE) # File ready
      #    }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="No"){
      #      source("Functions/Interface_Jags_MultipleGroupsTruePreNozero.R",local = TRUE) # File ready
    }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Single" & input$ID_ZeroPrevalence=="Yes"){
      plotOutput("TRpreZero_Plot")
     }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="Yes"){
       plotOutput("MultTRpreZero_Plot")
     }else if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="No"){
       plotOutput("MultTRpre_Plot")
    }else if(input$ID_TrueApp!="True prevalence" & input$ID_SingleMultiple=="Multiple" & input$ID_ZeroPrevalence=="No"){
      plotOutput("MultTRapp_Plot")
    }
    }
  })
  })
  
  #-----  Interactive + dynamic sliders/buttons ------#
  output$TrueApp_fb <- renderUI({
    if(input$ID_Informative=="No"){
      }else{
      radioButtons(inputId = 'ID_TrueApp', choices=c("True prevalence",
                                                     "Apparent prevalence"),
                   label = 'Do you want to model the true or the apparent prevalence?',
                   selected = "True prevalence",inline = TRUE)
      
    }
  })

  output$zero_fb <- renderUI({
    if(input$ID_TrueApp!="True prevalence" | input$ID_Informative=="No"){
    }else{
      radioButtons(inputId = 'ID_ZeroPrevalence', choices=c("No","Yes"),
                   label = 'Do you want to account for zero true prevalence?',
                   selected = "No",inline = TRUE)
    }
  })
  output$metric_fb <- renderUI({
    if(input$ID_Informative=="Yes" & input$ID_SingleMultiple=="Single"){
      radioButtons(inputId = 'ID_MeanMedianMode', choices=c("Mean",
                                                            "Median",
                                                            "Mode",
                                                            "Percentiles"),
                   label = 'Which measure of central tendency or dispersion would you like to use for the true prevalence prior?',
                   selected = "Mean",inline = TRUE)
    }else if(input$ID_Informative=="Yes" & input$ID_SingleMultiple=="Multiple"){
      radioButtons(inputId = 'ID_MeanMedianMode', choices=c("Mean"),
                   label = 'Which measure of central tendency or dispersion would you like to use for the true prevalence prior?',
                   selected = "Mean",inline = TRUE)
    }else if(input$ID_Informative=="No"){
      
    }
  })
  output$report_fb <- renderUI({
    if(priors$mSetupmodel==TRUE){uiOutput("Rmark")}
  })
  output$report_fb_side <- renderUI({
    if(priors$mSetupmodel==TRUE){
      source("Functions/Interface_DownloadResults.R",local = TRUE)$value # Done
      
    }
  })
  output$MultiDatasets_Out_fb <- renderUI({
    
    if(input$ID_SingleMultiple=="Multiple"){
      source("Functions/Interface_MultiDatasetYes.R",local = TRUE)$value
    }else{
      source("Functions/Interface_MultiDatasetNo.R",local = TRUE)$value
    }
  })
  output$MultiDataset_fb <- renderUI({
    if(input$ID_SingleMultiple=="Multiple"){
      if(input$LoadData=="Option1Preload"){
        selectInput("Indata1", "Dataset:",
                    c("Example1_4Studies" = "Example1_4Studies",
                      "Example2_40Studies" = "Example2_40Studies",
                      "Example4_129studies" = "Example4_129studies"))
      }else{
        fileInput(inputId = "Indata2", label = "Choose .xls/.csv file",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    ".xls")
        )    }
    }
    else if(input$ID_SingleMultiple!="Multiple"){
    }
    
  })

  output$sliders_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      
    if(input$lower.value=="No"){
      sliderInput(inputId = "PercentileValue1",label = paste("Specify the upper or lower limit for the ",input$ID_MeanMedianMode," at the specified level of confidence: "), 
                  value = min(input$PercentileValue1,input$PriorMetric), min=0, max=1,step = 0.01)
    }else{
      sliderInput(inputId = "PercentileValue1",label = paste("Specify the upper or lower limit for the ",input$ID_MeanMedianMode," at the specified level of confidence: "), 
                  value = max(input$PercentileValue1,input$PriorMetric), min=0, max=1,step = 0.01)
      
    }
    }
  })
  output$sliders_fb_SE <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      if(input$lower.value_SE=="No" ){
      sliderInput(inputId = "PercentileValue1_SE",label = paste("Specify the upper or lower limit for the sensitivity at the specified level of confidence: "), 
                  value = min(input$PercentileValue1_SE,input$PriorMetric_SE), min=0, max=1,step = 0.01)
    }else{
      sliderInput(inputId = "PercentileValue1_SE",label = paste("Specify the upper or lower limit for the sensitivity at the specified level of confidence: "), 
                  value = max(input$PercentileValue1_SE,input$PriorMetric_SE), min=0, max=1,step = 0.01)
      
    }
    }
  })
  output$sliders_fb_SP <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      if(input$lower.value_SP=="No" ){
      sliderInput(inputId = "PercentileValue1_SP",label = paste("Specify the upper or lower limit for the specificity at the specified level of confidence: "), 
                  value = min(input$PercentileValue1_SP,input$PriorMetric_SP), min=0, max=1,step = 0.01)
    }else{
      sliderInput(inputId = "PercentileValue1_SP",label = paste("Specify the upper or lower limit for the specificity at the specified level of confidence: "), 
                  value = max(input$PercentileValue1_SP,input$PriorMetric_SP), min=0, max=1,step = 0.01)
      
    }
    }
  })
  output$sliders_fb2_SE <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      if(input$lower.value2_SE=="No"){
      sliderInput(inputId = "PercentileValue1_SE",label = paste("Specify the upper or lower limit for the sensitivity at the specified level of confidence: "), 
                  value = min(input$PercentileValue1_SE,input$PriorMetric2_SE), min=0, max=1,step = 0.01)
    }else{
      sliderInput(inputId = "PercentileValue1_SE",label = paste("Specify the upper or lower limit for the sensitivity at the specified level of confidence: "), 
                  value = max(input$PercentileValue1_SE,input$PriorMetric2_SE), min=0, max=1,step = 0.01)
      
    }
    }
  })
  output$sliders_fb2_SP <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      
    if(input$lower.value2_SP=="No"){
      sliderInput(inputId = "PercentileValue1_SP",label = paste("Specify the upper or lower limit for the specificity at the specified level of confidence: "), 
                  value = min(input$PercentileValue1_SP,input$PriorMetric2_SP), min=0, max=1,step = 0.01)
    }else{
      sliderInput(inputId = "PercentileValue1_SP",label = paste("Specify the upper or lower limit for the specificity at the specified level of confidence: "), 
                  value = max(input$PercentileValue1_SP,input$PriorMetric2_SP), min=0, max=1,step = 0.01)
    }
    }
  })
  output$sliders_fb_tau0 <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      
    if(input$lower.value_tau0=="No"){
      sliderInput(inputId = "PercentileValue1_tau0",label = paste("Specify the upper or lower limit for the non-zero prevalence at the specified level of confidence: "), 
                  value = min(input$PercentileValue1_tau0,input$PriorMetric_tau0), min=0, max=1,step = 0.01)
    }else{
      sliderInput(inputId = "PercentileValue1_tau0",label = paste("Specify the upper or lower limit for the non-zero prevalence at the specified level of confidence: "), 
                  value = max(input$PercentileValue1_tau0,input$PriorMetric_tau0), min=0, max=1,step = 0.01)
      
    }
    }
  })
  output$sliders_fb2_tau0 <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      
    if(input$lower.value2_tau0=="No"){
      sliderInput(inputId = "PercentileValue1_tau0",label = paste("Specify the upper or lower limit for the non-zero prevalence at the specified level of confidence: "), 
                  value = min(input$PercentileValue1_tau0,input$PriorMetric2_tau0), min=0, max=1,step = 0.01)
    }else{
      sliderInput(inputId = "PercentileValue1_tau0",label = paste("Specify the upper or lower limit for the non-zero prevalence at the specified level of confidence: "), 
                  value = max(input$PercentileValue1_tau0,input$PriorMetric2_tau0), min=0, max=1,step = 0.01)
      
    }
    }
  })
  
  outputOptions(output, "sliders_fb_SE", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders_fb_SP", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders_fb_tau0", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders_fb2_SE", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders_fb2_SP", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders_fb2_tau0", suspendWhenHidden = FALSE)
  
  
  output$sliders2_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
    if(input$lower.value2=="No"){
      updateSliderInput(session = session,inputId = "PercentileValue2", label = "Specify the lower limit for the mean/median/mode at the specified level of confidence: ",
                  value = min(input$PercentileValue2,input$PriorMean2-0.01), min=0, max=1,step = 0.01)
    }else{
      updateSliderInput(session = session,inputId = "PercentileValue2", label = "Specify the upper limit for the mean/median/mode at the specified level of confidence: ",
                  value = max(input$PercentileValue2,input$PriorMean2+0.01), min=0, max=1,step = 0.01)
    }
    }
  })
  output$sliders22_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      
    if(input$lower.value2=="No"){
      updateSliderInput(session = session,inputId = "PercentileMedian2", label = "Specify the median value that corresponds to the defined psi.percentile. has to be higher than both themean and the percentile: ",
                  value = max(input$PercentileValue2+0.001,input$PercentileMedian2,input$PriorMean2+0.001), min=0, max=1,step = 0.001)
    }else{
      updateSliderInput(session = session,inputId = "PercentileMedian2", label = "Specify the median value that corresponds to the defined psi.percentile. has to be higher than both themean and the percentile: ",
                  value = max(input$PercentileValue2+0.01,input$PercentileMedian2,input$PriorMean2+0.01), min=0, max=1,step = 0.01)
    }
    }
  })
  output$sliders23_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      
    if(input$lower.value2=="No"){
      updateSliderInput(session = session,inputId = "Percentile95value2", label = "Specify the value that the percentile.median does not exceed with 95% confidence. has to be higher than the percentile",
                  value=max(input$Percentile95value2,input$PercentileMedian2+0.01), min=0, max=1, step = 0.01)
    }else{
      updateSliderInput(session = session,inputId = "Percentile95value2", label = "Specify the value that the percentile.median does not exceed with 95% confidence. has to be higher than the percentile",
                  value=max(input$Percentile95value2,input$PercentileMedian2+0.01), min=0, max=1, step = 0.01)
    }
    }
  })
 
  output$sliders2_SP_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      
    if(input$lower.value2_SP=="No"){
      updateSliderInput(session = session,inputId = "PercentileValue2_SP", label = "Specify the upper or lower limit for the mean/median/mode at the specified level of confidence: ",
                  value = min(input$PercentileValue2_SP,input$PriorMean2_SP-0.01), min=0, max=1,step = 0.01)
    }else{
      updateSliderInput(session = session,inputId = "PercentileValue2_SP", label = "Specify the upper or lower limit for the mean/median/mode at the specified level of confidence: ",
                  value = max(input$PercentileValue2_SP,input$PriorMean2_SP+0.01), min=0, max=1,step = 0.01)
    }
    }
  })
  output$sliders22_SP_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      
    if(input$lower.value2_SP=="No"){
      updateSliderInput(session = session,inputId = "PercentileMedian2_SP", label = "Specify the median value that corresponds to the defined psi.percentile. has to be higher than both themean and the percentile: ",
                  value = max(input$PercentileValue2_SP+0.01,input$PercentileMedian2_SP,input$PriorMean2_SP+0.01), min=0, max=1,step = 0.01)
    }else{
      updateSliderInput(session = session,inputId = "PercentileMedian2_SP", label = "Specify the median value that corresponds to the defined psi.percentile. has to be higher than both themean and the percentile: ",
                  value = max(input$PercentileValue2_SP+0.01,input$PercentileMedian2_SP,input$PriorMean2_SP+0.01), min=0, max=1,step = 0.01)
    }
    }
  })
  output$sliders23_SP_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      if(input$lower.value2_SP=="No"){
      updateSliderInput(session = session,inputId = "Percentile95value2_SP", label = "Specify the value that the percentile.median does not exceed with 95% confidence. has to be higher than the percentile",
                  min=0, max=1, value=max(input$Percentile95value2_SP,input$PercentileMedian2_SP+0.01),step = 0.01)
    }else{
      updateSliderInput(session = session,inputId = "Percentile95value2_SP", label = "Specify the value that the percentile.median does not exceed with 95% confidence. has to be higher than the percentile",
                  min=0, max=1, value=max(input$Percentile95value2_SP,input$PercentileMedian2_SP+0.01),step = 0.01)
    }
    }
  })

  output$sliders2_SE_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      
    if(input$lower.value2_SE=="No"){
      updateSliderInput(session = session,inputId = "PercentileValue2_SE", label = "Specify the lower limit for the mean/median/mode at the specified level of confidence: ",
                  value = min(input$PercentileValue2_SE,input$PriorMean2_SE-0.01), min=0, max=1,step = 0.01)
    }else{
      updateSliderInput(session = session,inputId = "PercentileValue2_SE", label = "Specify the upper limit for the mean/median/mode at the specified level of confidence: ",
                  value = max(input$PercentileValue2_SE,input$PriorMean2_SE+0.01), min=0, max=1,step = 0.01)
    }
    }
  })
  output$sliders22_SE_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      if(input$lower.value2_SE=="No"){
      updateSliderInput(session = session,inputId = "PercentileMedian2_SE", label = "Specify the median value that corresponds to the defined psi.percentile. has to be higher than both themean and the percentile: ",
                  value = max(input$PercentileValue2_SE+0.01,input$PercentileMedian2_SE,input$PriorMean2_SE+0.01), min=0, max=1,step = 0.01)
    }else{
      updateSliderInput(session = session,inputId = "PercentileMedian2_SE", label = "Specify the median value that corresponds to the defined psi.percentile. has to be higher than both themean and the percentile: ",
                  value = max(input$PercentileValue2_SE+0.01,input$PercentileMedian2_SE,input$PriorMean2_SE+0.01), min=0, max=1,step = 0.01)
    }
    }
  })
  output$sliders23_SE_fb <- renderUI({
    if(input$lower.value.fix=="Fixed"){
      if(input$lower.value2_SE=="No"){
      updateSliderInput(session = session,inputId = "Percentile95value2_SE", label = "Specify the value that the percentile.median does not exceed with 95% confidence. has to be higher than the percentile",
                  value=max(input$Percentile95value2_SE,input$PercentileMedian2_SE+0.01), min=0, max=1, step = 0.01)
    }else{
      updateSliderInput(session = session,inputId = "Percentile95value2", label = "Specify the value that the percentile.median does not exceed with 95% confidence. has to be higher than the percentile",
                  value=max(input$Percentile95value2_SE,input$PercentileMedian2_SE+0.01), min=0, max=1, step = 0.01)
    }
    }
  })

  output$sliders_qq_fb<-renderUI({ 
    if(input$lower.value.fix=="Fixed"){
    sliderInput(inputId = "PercentileValue3_2", label = "Specify the value for the 2nd percentile", 
                min=0, max=1, value=max(input$PercentileValue3_1+0.01,input$PercentileValue3_2),step = 0.01)
    }
  })
  output$sliders_qq22_fb<-renderUI({ 
    if(input$lower.value.fix=="Fixed"){
      
    sliderInput(inputId = "PercentileValue3_2", label = "Specify the value for the 2nd percentile", 
                min=0, max=1, value=max(input$PercentileValue3_1+0.01,input$PercentileValue3_2),step = 0.01)
    }
  })
  output$sliders_qq22_SE_fb<-renderUI({ 
    if(input$lower.value.fix=="Fixed"){
      sliderInput(inputId = "PercentileValue3_2_SE", label = "Specify the value for the 2nd percentile", 
                min=0, max=1, value=max(input$PercentileValue3_1_SE+0.01,input$PercentileValue3_2_SE),step = 0.01)
    }
  })
  output$sliders_qq22_SP_fb<-renderUI({ 
    if(input$lower.value.fix=="Fixed"){
      sliderInput(inputId = "PercentileValue3_2_SP", label = "Specify the value for the 2nd percentile", 
                min=0, max=1, value=max(input$PercentileValue3_1_SP+0.01,input$PercentileValue3_2_SP),step = 0.01)
    }
  })
  output$sliders_qq22_tau0_fb<-renderUI({ 
    if(input$lower.value.fix=="Fixed"){
      sliderInput(inputId = "PercentileValue3_2_tau0", label = "Specify the value for the 2nd percentile", 
                min=0, max=1, value=max(input$PercentileValue3_1_tau0+0.01,input$PercentileValue3_2_tau0),step = 0.01)
    }
  })
  
  
  outputOptions(output, "sliders2_fb", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders22_fb", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders23_fb", suspendWhenHidden = FALSE)

  # outputOptions(output, "sliders2_SP_fb", suspendWhenHidden = FALSE)
  # outputOptions(output, "sliders22_SP_fb", suspendWhenHidden = FALSE)
  # outputOptions(output, "sliders23_SP_fb", suspendWhenHidden = FALSE)
  # outputOptions(output, "sliders2_SE_fb", suspendWhenHidden = FALSE)
  # outputOptions(output, "sliders22_SE_fb", suspendWhenHidden = FALSE)
  # outputOptions(output, "sliders23_SE_fb", suspendWhenHidden = FALSE)
  
  outputOptions(output, "sliders_qq_fb", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders_qq22_fb", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders_qq22_SE_fb", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders_qq22_SP_fb", suspendWhenHidden = FALSE)
  outputOptions(output, "sliders_qq22_tau0_fb", suspendWhenHidden = FALSE)

  #------ Condition for sample size and positives bugged. ------#
  # output$DataInput_fb <- renderUI({
  #   numericInput(inputId = "y", label = "Number of positive tests: ",
  #min = 1, max = 1000000, step = 1, value = input$n)
  # })
  
  
  
  #------- Download stuff ---------#
  
  output$downloadModel <- downloadHandler(
    filename <- function(){
      paste("Model.mcmc.RData")
    },
    content = function(file) {
      save(Model1.mcmc, file = file)
    }
  )
  output$downloadJags <- downloadHandler(
    filename <- function(){
      paste("JagsModel.rData")
    },
    content = function(file) {
      save(model_out, file = file)
    }
  )
  output$downloadData <- downloadHandler(
    filename <- function(){
      paste("InputData.RData")
    },
    content = function(file) {
      if(input$ID_SingleMultiple=="Single"){
        Multi_data=temp_data
      save(Multi_data, file = file)
      }
      Single_data=list(y=input$y,n=input$n)
      save(Single_data, file = file)
      
    }
  )
  
  output$downloadFile <- downloadHandler(
    filename <- function(){
      paste("ExampleFile.xlsx")
    },
    content = function(file) {
      write.xlsx(x = Example2_40Studies_2cols, file = file)
    }
  )
  
  output$downloadReport <- downloadHandler( # STILL OPEN
    filename = function() {
      paste("my-report", sep = ".", switch(
        input$format, PDF = "pdf", HTML = "html", Word = "docx"
      ))
    },
    content = function(file) {
      src <- normalizePath("report.Rmd")
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)
      
      library(rmarkdown)
      out <- render("report.Rmd", switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  #-------- Initial screen stats --------#
  output$NumberPriorSetups <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      paste0("4"),"prior elicitation approaches",
      icon = icon("chevron-down"),color = "teal")
  })
  output$NumberModels <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      paste0("8")," prevelance model variations",
      icon = icon("arrows-alt"),color = "blue")
  })
  output$NumberDemos <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      paste0("3"),"preloaded datasets for demonstration",
      icon = icon("thumbs-up"),color = "red")
  })
  #-------- Intermediate screen banners --------#
  output$Boxsetup1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      paste0(priors$temp),paste(priors$temp2),
      icon = icon(priors$icons),color = priors$color)
  })
  output$Boxsetup2 <- shinydashboard::renderValueBox({
    if(priors$PriorSelect==FALSE){
      shinydashboard::valueBox(
        paste0("Prior setup:"),if(priors$SetupPriors==FALSE){"Please set up the model in Tab 'Set up' before you move forward"}else{"Setup done - Please elicite a prior and press select"},
        icon = if(priors$SetupPriors==FALSE){icon("thumbs-down")}else{icon("thumbs-up")},color = if(priors$SetupPriors==FALSE){"red"}else{"yellow"}
      )}
    else if(priors$PriorSelect==TRUE){
      shinydashboard::valueBox(
        paste0("Prior setup:"),if(priors$SetupPriors==FALSE){"Please set up the model in Tab 'Set up' before you move forward"}else{"Model and prior setup ready! - Move to tab 'Model'"},
        icon = if(priors$SetupPriors==FALSE){icon("thumbs-down")}else{icon("thumbs-up")},color = if(priors$SetupPriors==FALSE){"red"}else{"green"}
      )
    }
  })
  output$Boxsetup3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      
      paste0("Model setup:"),if(priors$PriorSelect==FALSE | priors$SetupPriors==FALSE | priors$mSetupmodel==FALSE){"Please set up the model, priors and input data in Tabs 'Set up', 'Priors' and 'Model' before you move forward"}else{paste0("Model is selected and (", name_data, ") data are currently loaded- press (Step 2. Output) and then go to report")},
      icon = if(priors$PriorSelect==FALSE | priors$SetupPriors==FALSE | priors$mSetupmodel==FALSE){icon("thumbs-down")}else{icon("thumbs-up")},
      color = if(priors$PriorSelect==FALSE | priors$SetupPriors==FALSE | priors$mSetupmodel==FALSE){"red"}else{"green"}
    )

  })
  output$Boxsetup4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      paste0("Status report:"),if(priors$mSetupmodel==FALSE){"Please finish with all previous steps before accesing the report"}else{"The report will become available below"},
      icon = if( priors$mSetupmodel==FALSE){icon("list-alt")}else{icon("thumbs-up")},
      color = if(priors$mSetupmodel==FALSE){"red"}else{"green"}
    )

  })
  
  #-------- Reset buttons ----------#
  observeEvent(input$buttonReset, {
    # shinyjs::reset("setup-panel")
    # priors$color<-"red"
    # priors$icons<-"thumbs-down"
    # priors$temp<-"Status: 'Not set'"
    # priors$temp2<-"Please first select options and press -Fix-"
    # priors$SetupPriors<-FALSE
    session$reload()
  })
  observeEvent(input$buttonPriorReset, {
    #shinyjs::reset("priors-panel")
    #priors$PriorSelect<-FALSE
    session$reload()
  })
  observeEvent(input$buttonModelReset, {
    #shinyjs::reset("model-panel")
    #priors$SetupPriors<-FALSE
    session$reload()
  })
  #-------- Fix buttons ----------#
  observeEvent(input$buttonSetup,{
    if(input$ID_Informative=="Yes"){
      TrueApp<-input$ID_TrueApp
    }else{
      TrueApp<-"Apparent prevalence"
    }
    priors$temp<-"Status: 'Set'";
    priors$temp2<-paste("Your input assumes that: \n 1. ",input$ID_SingleMultiple," will be modelled, \n 2. ",input$ID_ZeroPrevalence,", zero prevalence will be modelled, 3. the ",TrueApp, " will be modelled and \n 4. ",input$ID_Informative," informative prior(s) will be modelled",sep="")
    priors$icons<-"thumbs-up"#   prior_cond<<-" "
    priors$color<-"green"
    priors$SetupPriors<-TRUE
  })
  observeEvent(input$buttonPrior,{
    priors$PriorSelect<-TRUE
    if(input$ID_TrueApp=="Apparent prevalence" & input$ID_SingleMultiple=="Single" & input$ID_ZeroPrevalence=="Νο"){
      priors$prior<<-list(a=fb$a,b=fb$b)
    }
    if(input$ID_TrueApp=="True prevalence" & input$ID_SingleMultiple=="Single" & input$ID_ZeroPrevalence=="No"){
      priors$prior<<-list(a=fb$a,b=fb$b)
    }
  })
  observeEvent(input$buttonFixModel, {
    if(input$ID_SingleMultiple=="Multiple"){
      if(input$LoadData=="Option1Preload"){
        req(!is.na(input$Indata1))
        name_data <<- input$Indata1
      }else{
        req(!is.na(input$Indata2))
        name_data <<- "Uploaded dataset"#input$Indata2
      }
    }
    priors$mSetupmodel<-TRUE
  })
  
  #-------- Help set prior buttons ---------#
  observeEvent(input$buttonPriorHelp3, {
    showModal(modalDialog(
      title = "Example 3",
      paste0("Let assume that the mean prevalence of a disease/infection for the units within an area/region is thought to be 0.20 and we are 99% confident that it is not more than 0.40. Within this area/group, we are also confident that 90% of all units have a prevalence less or equal to 0.50 and we are 95% certain that it does not exceed 0.60. 
             Then we have to set <<themean=0.20, percentile=0.99, lower.v=TRUE, percentile.value=0.30, psi.percentile=0.90, percentile.median=0.50, percentile95value=0.60>>. Caution: If the movement of a slider produces a warning message, slightly move the slider to the other direction."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$buttonPriorHelp2, {
    showModal(modalDialog(
      title = "Example 2",
      paste0("Let assume that our beliefs point that 20% of the units in an area/region have a prevalence of disease/infection less than or equal to 0.30 while at the same time we are 90% certain that the prevalence is less than 0.60. 
      Then we have to set <<percentile.value1=0.30, percentile1=0.20, percentile.value2=0.60, percentile2=0.90>>. Caution: If the user inputs a very small value for the 1st percentile and a very large value for the 2nd percentile the prior will not be defined."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  observeEvent(input$buttonPriorHelp1, {
    showModal(modalDialog(
      title = "Example 1",
      paste0("Let assume that based on the available literature the mean value for the sensitivity of a test is expected to be 0.90 and we can be 95% sure that it is higher than 0.80. 
             Then we have to set, <<themean=0.90, percentile=0.95,lower.v=FALSE, percentile.value=0.80>>"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  #--------  Report rmakdown --------#
  output$Rmark <- renderUI({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0.8, {
    if(input$ID_Informative=="No"){
      if(input$ID_SingleMultiple=="Multiple"){
        temp<-"TPpre_Report_Mult.Rmd"
      }else{
        temp<-"TPpre_Report.Rmd"
      }
    }else{
      if(input$ID_TrueApp=="True prevalence"){
        temp<-"TPpre_Report_True.Rmd"
      }else{
        temp<-"TPpre_Report.Rmd"
      }
      if(input$ID_SingleMultiple=="Multiple"){
        if(input$ID_TrueApp=="True prevalence"){
          temp<-"TPpre_Report_Mult_True.Rmd"
        }else{
          temp<-"TPpre_Report_Mult.Rmd"
        }
      }
    }
    includeHTML(render(paste0("./Rmarkdown/",temp), quiet = TRUE,output_format = "html_document"))
                 })
      })
  
  #------- Load multiple datasets -------#
  dataset<<- reactive({
    if(input$ID_SingleMultiple=="Multiple"){
      if(input$LoadData=="Option1Preload"){
        req(!is.na(input$Indata1))
        temp_data <<- data.frame(get(input$Indata1)) 
      }else{
        req(!is.na(input$Indata2))
        inFile <- input$Indata2
        temp_data <<- data.frame(read_excel(inFile$datapath,sheet=1))
        name_data<<-"Uploaded dataset"
      }
    }
    #save(temp_data,file = "temp_data.rdata")
    return(temp_data)
  })
  
  #------- Export dataset to print in Shiny GUI -------#
  output$table2 <- renderDataTable({
    if(input$ID_SingleMultiple=="Multiple"){
      if(input$LoadData=="Option1Preload"){
        req(!is.na(input$Indata1))
        DT::datatable(data.frame(get(input$Indata1)))     
      }else{
        req(!is.na(input$Indata2))
        inFile <- input$Indata2
        temp_data <- data.frame(read_excel(inFile$datapath,sheet=1))
        DT::datatable(data.frame(temp_data))     
        
      }
      
    }
  })
})
