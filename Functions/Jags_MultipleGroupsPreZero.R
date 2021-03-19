cat("model{
for(i in 1:k){
y[i] ~ dbin(ap[i], n[i])
ap[i] <- sub.p[i]*main.Se + (1-sub.p[i])*(1-main.Sp)
sub.p[i] <- z[i] * pstar[i]
z[i] ~ dbern(main.tau0)

#informative hyperpior for prevalence
pstar[i] ~ dbeta(alpha,beta)
}
alpha <- main.ap*psi
beta <- psi*(1-main.ap)
main.ap ~ dbeta(amu, bmu) I(0.01,0.99) ## Mode=0.12, 95% sure < 0.30  dbeta(3.283, 17.744) 
psi ~ dgamma(apsi, bpsi) I(0.01,0.99) ## Uses Median of 95th percentile of prevalence dgamma(4.524, 0.387)

#informative prior for Se and Sp
main.Se ~ dbeta(ase, bse) I(0.01,0.99) ## Mode=0.25; 95% sure < 0.30
main.Sp ~ dbeta(asp, bsp) I(0.01,0.99) ## Mode=0.98, 95% sure > 0.96

#informative prior for the probability of zero between group/herd prevalence
main.tau0 ~ dbeta(atau0, btau0)## Mode=0.60, 95% sure < 0.827

#predictions
pre.z.rep ~ dbern(main.tau0)
pre.pstar.rep ~ dbeta(alpha,beta)
pre.p.rep <- pre.z.rep*pre.pstar.rep
pre.pequal0 <- equals(pre.p.rep,0)
pre.plessthan0.05 <- step(0.05-pre.p.rep)
}", file=paste("TruezeroPreMultipleZero.txt"))

#data - list(k=35, n=70, y=c(2,0,3,0,1,2,2,3,6,0,8,1,13,2,1,3,1,7,2,2,0,4,1,4,2,6,1,4,0,6,4,2,0,12,33))

#initials - list(Se=0.25, Sp=0.98, mu=0.12, psi=11.69, tau0=0.60, pstar=c(rep(0.05,35)), z=c(rep(1,35), z.rep=0, pstar.rep=0.05))  

SaveParams <- c("main.ap","psi","main.tau0","main.Sp","main.Se","sub.p",
                "pstar","pre.z.rep","pre.pstar.rep","pre.p.rep","pre.pequal0",
                "pre.plessthan0.05")

dtst<-dataset()
# dtst$n
# k=length(dtst$n) # k=5

y= dtst$positive#  c(5,14,23,24,1)
n=dtst$n #c(100,110,111,123,132)
k=length(n)
ase=asp=amu=apsi=a=1
bse=bsp=bmu=bpsi=b=10
atau0=0.1
btau0=0.5

# jagsoutput_TruezeroMult<-jags.model(data=list(n=n,y=y, k=k,
#                                               ase=ase, bse=bse,
#                                               asp=asp, bsp=bsp,
#                                               amu=amu, bmu=bmu,
#                                               apsi=apsi, bpsi=bpsi,
#                                               atau0=atau0, btau0=btau0),
#                                     inits=NULL, n.chains=input$nchains,
#                                     file=paste("TruezeroPreMultiple.txt"),quiet=TRUE)

jagsoutput_TruezeroMult<-rjags::jags.model(data=list(n=dtst$n,y=dtst$positive, k=length(dtst$n),
                                              ase=2, bse=12,
                                              asp=2, bsp=12,
                                              amu=2, bmu=12,
                                              apsi=2, bpsi=12,
                                              atau0=2, btau0=12),
                                    inits=NULL, n.chains=input$nchains,
                                    file=paste("TruezeroPreMultipleZero.txt"),quiet=TRUE)

#   update(jagsoutput0, n.iter=nniter, n.thin=nnthin, n.burnin=floor(nniter/6), progress.bar="none")
Model1.mcmc <<- coda.samples(jagsoutput_TruezeroMult,  
                             n.iter=input$nniter, 
                             n.thin=input$nnthin, 
                             n.burnin=floor(input$nniter/10),
                             variable.names=SaveParams)
return(Model1.mcmc)