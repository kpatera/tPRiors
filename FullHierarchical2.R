cat("model{

    for(i in 1:Nobs) {
    thm[i] ~ dnorm(theta[zone[i]], tau.e) 
    # likelihood
    }
    
    for(z in 1:Nzone) {
    theta[z] ~ dnorm(mu, tau.z) 
    # zone means (random effects)
    }
    
    #hyperpriors on random effects mean and variance
    mu ~ dnorm(0, 0.000001)
    
    tau.z ~ dgamma(0.001, 0.001)
    sigma2.z <- 1/tau.z   # random effects variance
    
    tau.e ~ dgamma(0.001, 0.001)
    sigma2.e <- 1/tau.e   # residual error variance
    
}", file=paste("fullHR.txt"))


Dtsts<-list(Nobs = 17, Nzone = 5,
            thm = c(111.3, 112.9, 112.9, 105.5, 122.6, 124.6,135.4, 135.7, 156.7, 144.8, 133.1, 116.6,
                    106.2, 126, 111.6, 112.5, 98.6),
            zone = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5))

SaveParams <- c("theta","mu","tau.z")
                
generic_jags<-rjags::jags.model(data=list(Nobs=Dtsts$Nobs,
                                          Nzone=Dtsts$Nzone, 
                                          thm=Dtsts$thm,
                                          zone=Dtsts$zone),
                                inits=NULL, n.chains=2,
                                n.adapt = floor(100000/10),
                                file=paste("fullHR.txt")) # ,quiet=TRUE

Model1.mcmc <<- coda.samples(generic_jags,
                             n.iter=10000,thin = 5,
                             variable.names=SaveParams,seed=998)


S2<-ggs(Model1.mcmc,family = "theta")
ggs_density(S2,family = "theta")
data_out<-(data.frame(Model1.mcmc[[1]]))
data_out_sub<-data_out[,grepl( "theta" , names( data_out ) )]
data_out_sub_wide<-gather(data_out_sub, group, prevalence, names( data_out_sub ), factor_key=TRUE)

p2 <- data_out_sub_wide %>%
    ggplot( aes(x=group, y=prevalence, fill=group)) +
    geom_boxplot() +
    #  geom_violin() +
    xlab("class") +
    theme(legend.position="none") +
    xlab("")
#p2
ggplotly(p2)
