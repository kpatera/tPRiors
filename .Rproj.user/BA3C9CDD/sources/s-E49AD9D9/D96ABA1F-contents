Data<-data.frame(y=c(0.94,0.99,0.95,0.91,0.95,0.97,0.92),
                 V=c(0.0135,0.0029,0.035,0.023,0.015,0.019,
                      0.038),Nstud=7)

ref<-rma.uni(log(Data$y),sei = Data$V,method = "DL")
summary(ref)
  # Simple random-effects
cat("model 
{ 
  for (i in 1:Nstud)
  {
    P[i] <- 1/V[i]
    y[i] ~ dnorm(delta[i], P[i]) #study effects
    delta[i] ~ dnorm(d, prec)    #random level effects between studies
  }   
  d ~ dnorm(0, 1)
  OR <- exp(d)
  tau~dunif(0,1)
  tau.sq<-tau*tau
  prec<-1/(tau.sq)
}", file=paste("RE.txt"))

SaveParams <- c("d","P","y","tau","delta") 


generic_jagsRE<-rjags::jags.model(data=list(y=log(Data$y),V=Data$V,Nstud=Data$Nstud[1]),
                                                    inits=NULL, n.chains=2,
                                                    n.adapt = floor(10000/10),
                                                    file=paste("RE.txt"),quiet=TRUE)
Model1.mcmc_RE  <<- coda.samples(generic_jagsRE,
                             n.iter=50000,thin = 4,
                             variable.names=SaveParams,seed=998)



# Simple fixed-effects
cat("model 
{ 
  for (i in 1:Nstud)
  {
    P[i] <- 1/V[i]
    y[i] ~ dnorm(d, P[i])
  }
  d ~ dnorm(0, 1)
  OR <- exp(d)
}", file=paste("FE.txt"))
SaveParams <- c("d","P","y") 

generic_jagsFE<-rjags::jags.model(data=list(y=log(Data$y),V=Data$V,Nstud=Data$Nstud[1]),
                                  inits=NULL, n.chains=2,
                                  n.adapt = floor(10000/10),
                                  file=paste("FE.txt"),quiet=TRUE)
Model1.mcmc_FE <<- coda.samples(generic_jagsFE,
                             n.iter=50000,thin = 4,
                             variable.names=SaveParams,seed=998)

# Simple IVHET-model

cat("model 
{ 
  for (i in 1:Nstud)
  {
    ICC[i]<- tau.sq/(tau.sq+V[i])
    psi[i]<-1/(1-ICC[i])
    P2[i] <- 1/V[i]
    y2[i] ~ dnorm(delta[i], P2[i]) #study effects
    dpsi[i] ~ dnorm(0, psi[i]) #hyperdispersion parameter
    delta[i] ~ dnorm(d, 10000) #random level effects between studies
    mu[i]<-delta[i]*dpsi[i]
  }
  d ~ dnorm(0, 0.1)
  OR <- exp(d)
  tau~dunif(0,5)
  tau.sq<-tau*tau
  prec<-1/(tau.sq)}", file=paste("IVHET.txt"))

SaveParams <- c("d","tau") 

generic_jagsIVHET<-rjags::jags.model(data=list(y2=log(Data$y),V=Data$V,Nstud=Data$Nstud[1]),
                                  inits=NULL, n.chains=2,
                                  n.adapt = floor(10000/10),
                                  file=paste("IVHET.txt"),quiet=TRUE)
Model1.mcmc_IVHET <<- coda.samples(generic_jagsIVHET,
                                n.iter=50000,thin = 4,
                                variable.names=SaveParams,seed=998)



par(mfrow=c(1,3))
RE_1<-as.data.frame(Model1.mcmc_RE[[1]])
FE_1<-as.data.frame(Model1.mcmc_FE[[1]])
IVHET_1<-as.data.frame(Model1.mcmc_IVHET[[1]])
plot(density(RE_1$d),xlim=c(-0.25,0.15))
plot(density(FE_1$d),xlim=c(-0.25,0.15))
plot(density(IVHET_1$d),xlim=c(-0.25,0.15))
summary(RE_1$d)
summary(FE_1$d)
summary(IVHET_1$d)
