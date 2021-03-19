cat("model{
for(i in 1:k){
y[i] ~ dbin(ap[i], n[i])
ap[i] <- sub.p[i]*main.Se + (1-sub.p[i])*(1-main.Sp)
sub.p[i] ~ dbeta(alpha,beta)
}
alpha <- main.ap*main.psi
beta <- main.psi*(1-main.ap)
main.ap ~ dbeta(amu, bmu) ## Mode=0.12, 95% sure < 0.30  dbeta(3.283, 17.744) 
main.psi ~ dgamma(apsi, bpsi) ## Uses Median of 95th percentile of prevalence dgamma(4.524, 0.387)

#informative prior for Se and Sp
main.Se <- 1
main.Sp <- 1


#predictions
main.pre.pstar.rep ~ dbeta(alpha,beta)
}", file=paste("AppPreMultiple.txt"))

#data - list(k=35, n=70, y=c(2,0,3,0,1,2,2,3,6,0,8,1,13,2,1,3,1,7,2,2,0,4,1,4,2,6,1,4,0,6,4,2,0,12,33))

#initials - list(Se=0.25, Sp=0.98, mu=0.12, psi=11.69, tau0=0.60, pstar=c(rep(0.05,35)), z=c(rep(1,35), z.rep=0, pstar.rep=0.05))  

SaveParams <- c("main.ap","main.psi","sub.p","main.pre.pstar.rep")
dtst<-dataset()
y=dtst$positive
#y=c(5,14,23,24,1)#
n=dtst$n
#n=c(100,110,111,123,132)#

k=length(dtst$n)
#k=5
ase=asp=amu=apsi=2
bse=bsp=bmu=bpsi=10
atau0=2
btau0=10

jagsoutput_TruezeroMult<-rjags::jags.model(data=list(n=n,y=y, k=k,
                                              #a=amu, b=bmu,
                                              amu=2, bmu=12,
                                              apsi=2, bpsi=12),
                                    inits=NULL, n.chains=,2,
                                    file=paste("AppPreMultiple.txt"),quiet=TRUE)

# jagsoutput_TruezeroMult<-jags.model(data=list(n=input$n,y=input$y, k=input$k,
#                                               ase=priors$prior$ase, bse=priors$prior$bse,
#                                               asp=priors$prior$asp, bsp=priors$prior$bsp,
#                                               amu=priors$prior$amu, bmu=priors$prior$bmu,
#                                               apsi=priors$prior$apsi, bpsi=priors$prior$bpsi,
#                                               atau0=input$atau0, btau0=input$btau0
#                                               ),inits=NULL, n.chains=2,
#                             file=paste("TruezeroPreMultiple.txt"),quiet=TRUE)

#   update(jagsoutput0, n.iter=nniter, n.thin=nnthin, n.burnin=floor(nniter/6), progress.bar="none")
Model1.mcmc <<- coda.samples(jagsoutput_TruezeroMult,
                             n.iter=10000,
                             n.thin=5,
                             n.burnin=floor(10000/10),
                             variable.names=SaveParams)
# return(Model1.mcmc)