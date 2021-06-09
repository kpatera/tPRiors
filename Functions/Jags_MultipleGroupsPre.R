cat("model{

for(i in 1:k){
y[i] ~ dbin(ap[i], n[i])
ap[i] <- sub.p[i]*main.Se + (1-sub.p[i])*(1-main.Sp)
#informative hyperpior for prevalence
sub.p[i] ~ dbeta(alpha,beta) T(0.001,0.999)
}

alpha <- main.ap*main.psi
beta <- main.psi*(1-main.ap)
main.ap ~ dbeta(amu, bmu) T(0.001,0.999)
main.psi ~ dgamma(apsi, bpsi) T(0.001,0.999)

#informative prior for Se and Sp
main.Se ~ dbeta(ase, bse) I(0.001,0.999)
main.Sp ~ dbeta(asp, bsp) I(0.001,0.999)

#predictions
y.pre ~ dbin(main.pstar.rep,m)

main.pstar.rep ~ dbeta(alpha,beta)
pre.pequal0 <- equals(main.pstar.rep,0)
pre.plessthan0.05 <- step(0.05-main.pstar.rep)
plessthanSetvalue <- step(perVal-main.ap)

}", file=paste("TrePreMultiple.txt"))



SaveParams <- c("main.ap","main.psi","main.Sp","main.Se","y.pre",
                "main.pstar.rep","pre.pequal0","pre.plessthan0.05","sub.p","plessthanSetvalue")
dtst<-dataset()
#dtst$region<-factor(dtst$region)
#dtst$country<-factor(dtst$country)
#dtst$ID<-1:length(dtst$region)


generic_jags<-jagsoutput_TrueMult<-rjags::jags.model(data=list(n=dtst$n,y=dtst$positive, k=length(dtst$n),m=100,
                                                               ase=fb_SE$a, bse=fb_SE$b,
                                                               asp=fb_SP$a, bsp=fb_SP$b,
                                                               amu=fb$abeta, bmu=fb$bbeta,
                                                               apsi=fb$agamma, bpsi=fb$bgamma,
                                                               perVal=input$perVal),
                                                     inits=NULL, n.chains=input$nchains,n.adapt = floor(input$nniter/10),
                                                     file=paste("TrePreMultiple.txt"),quiet=TRUE)


# generic_jags<-jagsoutput_TrueMult<-rjags::jags.model(data=list(n=dtst$n,y=dtst$positive, k=length(dtst$n),m=100,
#                                                                ase=21, bse=1,
#                                                                asp=21, bsp=1,
#                                                                amu=21, bmu=1,
#                                                                apsi=21, bpsi=1,
#                                                                perVal=0.5),
#                                                      inits=NULL, n.chains=2,n.adapt = floor(3000/10),
#                                                      file=paste("TrePreMultiple.txt"),quiet=TRUE)
# 


# Model1.mcmc <<- coda.samples(jagsoutput_TrueMult,
#                             n.iter=input$nniter,thin =input$nnthin,
#                             variable.names=SaveParams,seed=998)

Model1.mcmc <<- coda.samples(jagsoutput_TrueMult,
                            n.iter=10000,thin =4,
                            variable.names=SaveParams,seed=998)


return(Model1.mcmc)