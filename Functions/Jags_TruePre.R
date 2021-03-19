cat("model{ #ap denots apparent prevalences #main.ap true prevalence
    y ~ dbin(ap, n)
    ap<-main.ap*Se+(1-main.ap)*(1-Sp)
    #Uniform (non-informative) prior for apparent prevalence (ap)
    main.ap  ~ dbeta(a,b) ########################################################### Check if they are the same.
    Se ~ dbeta(ase, bse)   	  #0.85 (0.70–0.95)
    Sp ~ dbeta(asp, bsp)    		 #0.77 (0.49–0.96)
  }", file=paste("TruePre.txt"))

#data - list(n=4072, y=1210)

#initials - list(main.ap=0.1, Se=0.90, Sp=0.85)

SaveParams <- c("main.ap","ap","Se","Sp")

jagsoutput_True<-rjags::jags.model(data=list(n=input$n,y=input$y,
                                      ase=fb_SE$a,
                                      bse=fb_SE$b,
                                      asp=fb_SP$a,
                                      bsp=fb_SP$b,
                                      a=fb$a,
                                      b=fb$b),
                            inits=NULL, n.chains=input$nchains,file=paste("TruePre.txt"),quiet=TRUE)

Model1.mcmc <<- coda.samples(jagsoutput_True,  n.iter=input$nniter, n.thin=input$nnthin, n.burnin=floor(input$nniter/10),
                             variable.names=SaveParams)

return(Model1.mcmc)