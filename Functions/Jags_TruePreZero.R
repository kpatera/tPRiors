cat("model{ #ap denotes apparent, #main.ap denotes true prevalence
    y ~ dbin(ap, n) 
    y.pre ~ dbin(main.ap, m)
    ap<-main.ap*main.Se+(1-main.ap)*(1-main.Sp)
    main.ap <- z* pstar
    z ~ dbern(main.tau0)
    pequal0 <- equals(main.ap, 0)    
    main.Se ~ dbeta(ase, bse) 
    main.Sp ~ dbeta(asp, bsp)
    main.tau0 ~ dbeta(atau0, btau0) T(0.001,0.999) 
    pstar ~ dbeta(astar, bstar) 
    plessthanSetvalue <- step(perVal-main.ap)
    }", file=paste("TruezeroPre.txt"))

SaveParams <- c("main.ap","pstar","main.Se","main.Sp","ap","pequal0","plessthanSetvalue","y.pre")


generic_jags<-jagsoutput_Truezero<-rjags::jags.model(data=list(n=input$n,y=input$y, m=100,
                                          ase=fb_SE$a, bse=fb_SE$b,
                                          asp=fb_SP$a, bsp=fb_SP$b,
                                          astar=fb$a, bstar=fb$b,
                                          atau0=fb_tau0$a, btau0=fb_tau0$b,
                                          tau0=input$tau0,
                                          perVal=input$perVal),n.chains=input$nchains,
                                file=paste("TruezeroPre.txt"),quiet=TRUE)

Model1.mcmc <<- coda.samples(jagsoutput_Truezero,  n.iter=input$nniter, n.thin=input$nnthin, n.burnin=floor(input$nniter/10),
                             variable.names=SaveParams)
model_out<<-generic_jags$model()


return(Model1.mcmc)