cat("model{
for(i in 1:k){
y[i] ~ dbin(ap[i], n[i])
ap[i] <- main.pstar[i]*main.Se + (1-main.pstar[i])*(1-main.Sp)

#informative hyperpior for prevalence
main.pstar[i] ~ dbeta(alpha,beta)
}
alpha <- main.ap*psi
beta <- psi*(1-main.ap)
main.ap ~ dbeta(amu, bmu) I(0.01,0.99)## Mode=0.12, 95% sure < 0.30  dbeta(3.283, 17.744)
psi ~ dgamma(apsi, bpsi) I(0.01,0.99) ## Uses Median of 95th percentile of prevalence dgamma(4.524, 0.387)

#informative prior for Se and Sp
main.Se ~ dbeta(ase, bse) I(0.01,0.99) ## Mode=0.25; 95% sure < 0.30
main.Sp ~ dbeta(asp, bsp) I(0.01,0.99) ## Mode=0.98, 95% sure > 0.96


#predictions
pre.pstar.rep ~ dbeta(alpha,beta)
pre.pequal0 <- equals(pre.pstar.rep,0)
pre.plessthan0.05 <- step(0.05-pre.pstar.rep)
}", file=paste("TrePreMultiple.txt"))

#data - list(k=35, n=70, y=c(2,0,3,0,1,2,2,3,6,0,8,1,13,2,1,3,1,7,2,2,0,4,1,4,2,6,1,4,0,6,4,2,0,12,33))

#initials - list(Se=0.25, Sp=0.98, mu=0.12, psi=11.69, tau0=0.60, pstar=c(rep(0.05,35)), z=c(rep(1,35), z.rep=0, pstar.rep=0.05))  

SaveParams <- c("main.ap","psi","main.Sp","main.Se",
                "pre.pstar.rep","pre.pequal0","pre.plessthan0.05")
dtst<-dataset()
#k=length(dtst$n) # k=5

# y=c(1,10,1,2,5)
# n=c(10,134,14,12,123) 
# k=5
ase=asp=amu=apsi=a=1
bse=bsp=bmu=bpsi=b=10

 # jagsoutput_AppMult<-jags.model(data=list(n=n,y=y, k=k,
 #                                               amu=a,bmu=b,
 #                                               ase=ase, bse=bse,
 #                                               asp=asp, bsp=bsp,
 #                                               apsi=12, bpsi=15,
 #                                               atau0=4, btau0=12),
 #                                     inits=NULL, n.chains=2,
 #                                     file=paste("AppPreMultiple.txt"),quiet=TRUE)

jagsoutput_AppMult<-rjags::jags.model(data=list(n=dtst$n,y=dtst$positive, k=length(dtst$n),
                                              ase=12, bse=15,
                                              asp=12, bsp=15,
                                              amu=12, bmu=17.744,
                                              apsi=12, bpsi=15),
                                    inits=NULL, n.chains=input$nchains,
                                    file=paste("TrePreMultiple.txt"),quiet=TRUE)

  # Model1.mcmc <<- coda.samples(jagsoutput_AppMult,
  #                              n.iter=input$nniter,
  #                              n.thin=input$nnthin,
  #                              n.burnin=floor(input$nniter/10),
  #                              variable.names=SaveParams)
  
#  update(jagsoutput0, n.iter=nniter, n.thin=nnthin, n.burnin=floor(nniter/6), progress.bar="none")
 Model1.mcmc <<- coda.samples(jagsoutput_AppMult,
                              n.iter=input$nniter,
                              n.thin=input$nnthin,
                              n.burnin=floor(input$nniter/10),
                              variable.names=SaveParams)
return(Model1.mcmc)