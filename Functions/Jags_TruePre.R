cat("model{ #ap denotes apparent prevalences #main.ap true prevalence
    y ~ dbin(ap, n)
    y.pre ~ dbin(main.ap, m)
    ap<-main.ap*main.Se+(1-main.ap)*(1-main.Sp)
    #Uniform (non-informative) prior for apparent prevalence (ap)
    main.ap  ~ dbeta(a,b) 
    main.Se ~ dbeta(ase, bse)  
    main.Sp ~ dbeta(asp, bsp) 
    plessthanSetvalue <- step(perVal-main.ap)
  }", file=paste("TruePre.txt"))

SaveParams <- c("main.ap","ap","main.Se","main.Sp","plessthanSetvalue","y.pre")

generic_jags<-jagsoutput_True<-rjags::jags.model(data=list(n=input$n,y=input$y, m=100,
                                      ase=fb_SE$a, bse=fb_SE$b,
                                      asp=fb_SP$a, bsp=fb_SP$b,
                                      a=fb$a, b=fb$b,
                                      perVal=input$perVal),
                            inits=NULL, n.chains=input$nchains,file=paste("TruePre.txt"),quiet=TRUE)

Model1.mcmc <<- coda.samples(jagsoutput_True,  n.iter=input$nniter, n.thin=input$nnthin, n.burnin=floor(input$nniter/10),
                             variable.names=SaveParams)
model_out<<-generic_jags$model()

return(Model1.mcmc)