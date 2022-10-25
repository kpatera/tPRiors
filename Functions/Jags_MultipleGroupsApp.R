cat("model{

for(i in 1:k){
y[i] ~ dbin(ap[i], n[i])
ap[i] <- sub.p[i]*main.Se + (1-sub.p[i])*(1-main.Sp)

sub.p[i] ~ dbeta(alpha,beta) T(0.001,0.999)
}

alpha <- main.ap*main.psi
beta <- main.psi*(1-main.ap)
main.ap ~ dbeta(amu, bmu) T(0.001,0.999)
main.psi ~ dgamma(apsi, bpsi) T(0.001,0.999)

#informative prior for Se and Sp
main.Se <- 1
main.Sp <- 1

#predictions
y.pre ~ dbin(main.pstar.rep,m)

main.pstar.rep ~ dbeta(alpha,beta)
pre.pequal0 <- equals(main.ap,0)
pre.plessthan0.05 <- step(0.05-main.ap)
plessthanSetvalue <- step(perVal-main.ap)

}", file=paste("AppPreMultiple.txt"))



SaveParams <- c("main.ap","main.psi","main.pstar.rep","sub.p","plessthanSetvalue","pre.pequal0",
                "pre.plessthan0.05","y.pre") 

dtst<-dataset()
#dtst$region<-factor(dtst$region)
#dtst$country<-factor(dtst$country)
#dtst$ID<-1:length(dtst$region)


generic_jags<-jagsoutput_AppMult<-rjags::jags.model(data=list(n=dtst$n,y=dtst$positive,m=100,
                                                              k=length(dtst$n),
                                                              amu=fb$a, bmu=fb$b,
                                                              apsi=fb$ag, bpsi=fb$bg,
                                                              perVal=input$perVal),
                                                    inits=NULL, n.chains=input$nchains,
                                                    n.adapt = floor(input$nniter/10),
                                                    file=paste("AppPreMultiple.txt"),quiet=TRUE)

Model1.mcmc <<- coda.samples(jagsoutput_AppMult,
                             n.iter=input$nniter,thin = input$nnthin,
                             variable.names=SaveParams,seed=998)




# generic_jags<-jagsoutput_AppMult<-rjags::jags.model(data=list(n=dtst$n,y=dtst$positive,m=100,
#                                                               k=length(dtst$n),
#                                                               amu=75, bmu=300,
#                                                               apsi=2447881490, bpsi=403157841,
#                                                               perVal=0.5),
#                                                     inits=NULL, n.chains=3,
#                                                     n.adapt = floor(10000/10),
#                                                     file=paste("AppPreMultiple.txt"),quiet=TRUE)
# 
# Model1.mcmc <<- coda.samples(jagsoutput_AppMult,
#                              n.iter=10000,thin = 2,
#                              variable.names=SaveParams,seed=998)


model_out<<-generic_jags$model()

return(Model1.mcmc)