cat("model{
for(i in 1:k){
y[i] ~ dbin(ap[i], n[i])
ap[i] <- sub.p[i]*main.Se + (1-sub.p[i])*(1-main.Sp)
sub.p[i] <- z[i] * pstar[i]
z[i] ~ dbern(main.tau0)
#informative hyperpior for prevalence
pstar[i] ~ dbeta(alpha,beta) T(0.001,0.999)
}

alpha <- main.ap*main.psi
beta <- main.psi*(1-main.ap)
main.ap ~ dbeta(amu, bmu) T(0.001,0.999) 
main.psi ~ dgamma(apsi, bpsi) T(0.001,0.999)  

#informative prior for Se and Sp
main.Se ~ dbeta(ase, bse) T(0.001,0.999) 
main.Sp ~ dbeta(asp, bsp) T(0.001,0.999) 

#informative prior for the probability of zero between region prevalence
main.tau0 ~ dbeta(atau0, btau0) T(0.001,0.999) 

#predictions
y.pre ~ dbin(main.ap,m)
pre.z.rep ~ dbern(main.tau0)
main.pstar.rep ~ dbeta(alpha,beta)
pre.p.rep <- pre.z.rep*main.pstar.rep
pre.pequal0 <- equals(pre.p.rep,0)
pre.plessthan0.05 <- step(0.05-pre.p.rep)
plessthanSetvalue <- step(perVal-main.ap)
}", file=paste("TruezeroPreMultipleZero.txt"))

SaveParams <- c("main.ap","main.psi","main.tau0","main.Sp","main.Se","sub.p","y.pre",
                "pstar","pre.z.rep","main.pstar.rep","pre.p.rep","pre.pequal0",
                "pre.plessthan0.05","plessthanSetvalue")

dtst<-dataset()
#dtst$region<-factor(dtst$region)
#dtst$ID<-1:length(dtst$region)

generic_jags<-jagsoutput_TruezeroMult<-rjags::jags.model(data=list(n=dtst$n,y=dtst$positive, k=length(dtst$n),
                                              ase=fb_SE$a, bse=fb_SE$b,m=100,
                                              asp=fb_SP$a, bsp=fb_SP$b,
                                              amu=fb$abeta, bmu=fb$bbeta,
                                              apsi=fb$agamma, bpsi=fb$bgamma,
                                              atau0=fb_tau0$a, btau0=fb_tau0$b,
                                              perVal=input$perVal),
                                              inits=NULL, n.chains=input$nchains,n.adapt = floor(input$nniter/10),
                                    file=paste("TruezeroPreMultipleZero.txt"),quiet=TRUE)

Model1.mcmc <<- coda.samples(jagsoutput_TruezeroMult,  
                             n.iter=input$nniter,thin =input$nnthin,
                             variable.names=SaveParams,seed=998)
return(Model1.mcmc)