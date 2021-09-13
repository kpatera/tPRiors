cat("model{
    y ~ dbinom(main.ap, n)
    pre.y ~ dbinom(main.ap, m)

    #Uniform (non-informative) prior for apparent prevalence (ap)
    main.ap  ~ dbeta(a,b)########################################################### Check if they are the same.
    plessthanSetvalue <- step(perVal-main.ap)
  }", file=paste("ApparentPre.txt"))

SaveParams <- c("main.ap",'plessthanSetvalue',"pre.y")
generic_jags<-jagsoutput_Appa<-rjags::jags.model(data=list(n=input$n,y=input$y,m=100,
                                                           perVal=input$perVal,
                                                           a=fb$a, b=fb$b),
                            inits=NULL, n.chains=input$nchains,n.adapt = floor(input$nniter/10),
                            file=paste("ApparentPre.txt"),quiet=TRUE)

Model1.mcmc <<- coda.samples(jagsoutput_Appa,  n.iter=input$nniter,thin =input$nnthin,
                             variable.names=SaveParams,seed=998)

model_out<<-generic_jags$model()

return(Model1.mcmc)