#Author: Polychronis Kostoulas
#model specification

cat("model{
  
  for (i in 1:observations) {
    #app_p:apparent prevalence
    y[i] ~ dbin(app_p[i], n[i])
    #animal_p: animal prevalence
    #Se:sensitivity
    #Sp:specificity
    app_p[i] <- animal_p[i] * Se + (1 - animal_p[i]) * (1 - Sp)
    #animal_lev_p: animal level prevalence
    #herd_not_free: (0/1) the herd is free or not
    animal_p[i] <- animal_lev_p[i] * herd_not_free[i]
    herd_not_free[i] ~ dbern(herd_p[region[i]])
    animal_lev_p[i] ~ dbeta(alpha, beta)

  }
  
  for (j in 1:regions) {
    #herd_p: herd prevalence
    #herd_lev_p: herd level prevalence
    #region_not_free: (0/1) the region is free or not
    herd_p[j] <- herd_lev_p[j] * region_not_free[j]
    herd_lev_p[j] ~ dbeta(ahp, bhp)
    #region_p: region prevalence
    region_not_free[j] ~ dbern(region_p)

  }
  
  region_p ~ dbeta(arp, brp)
  #c: indices to calculate disease freedom or disease exceeding a prespecified level (here 20%) for the regions within the country.
  c[1] <- equals(region_p, 0.00000E+00)
  c[2] <- step(0.2 - region_p)
  #priors
  #mu: mean prevalence in infected herds
  #psi: a parameter expressing the variability of prevalence among infected herds
  mu ~ dbeta(amu, bmu)
  psi ~ dgamma(apsi, bpsi)
  alpha <- mu * psi
  beta <- psi * (1 - mu)
  Se ~ dbeta(ase1, bse1)
  Sp ~ dbeta(asp1, bsp1)
}", file=paste("FullHierarchical.txt"))

#data

Data<-list(
  region=c(1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 4.00000E+00, 4.00000E+00, 4.00000E+00, 4.00000E+00, 4.00000E+00, 4.00000E+00, 4.00000E+00, 4.00000E+00, 4.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 5.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 6.00000E+00, 7.00000E+00, 7.00000E+00, 7.00000E+00, 7.00000E+00, 7.00000E+00, 7.00000E+00, 7.00000E+00, 7.00000E+00, 7.00000E+00, 8.00000E+00, 8.00000E+00, 8.00000E+00, 8.00000E+00, 8.00000E+00, 8.00000E+00, 8.00000E+00, 8.00000E+00, 8.00000E+00, 9.00000E+00, 9.00000E+00, 9.00000E+00, 9.00000E+00, 9.00000E+00, 9.00000E+00, 9.00000E+00, 9.00000E+00, 9.00000E+00, 9.00000E+00, 9.00000E+00, 1.00000E+01, 1.00000E+01, 1.00000E+01, 1.00000E+01, 1.00000E+01, 1.00000E+01, 1.00000E+01, 1.00000E+01, 1.00000E+01), 
  y=c(1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 0.00000E+00, 1.00000E+00, 1.00000E+00, 2.00000E+00, 3.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 0.00000E+00, 1.00000E+00, 0.00000E+00, 1.00000E+00, 0.00000E+00, 1.00000E+00, 2.00000E+00, 0.00000E+00, 2.00000E+00, 1.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 2.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.30000E+01, 3.30000E+01, 4.50000E+01, 2.40000E+01, 2.80000E+01, 2.90000E+01, 3.30000E+01, 2.30000E+01, 1.40000E+01, 3.70000E+01, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 2.00000E+00, 2.00000E+00, 0.00000E+00, 2.00000E+00, 2.00000E+00, 1.00000E+00, 2.00000E+00, 4.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 3.00000E+00, 0.00000E+00, 3.00000E+00, 0.00000E+00, 3.00000E+00, 3.00000E+00, 1.00000E+00, 1.00000E+00, 2.00000E+00, 0.00000E+00, 3.00000E+00, 1.00000E+00, 2.00000E+00, 1.00000E+00, 2.40000E+01, 3.40000E+01, 2.60000E+01, 2.00000E+00, 2.30000E+01, 2.40000E+01, 4.40000E+01, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 0.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.00000E+00, 1.70000E+01, 2.20000E+01, 1.30000E+01, 2.80000E+01, 1.80000E+01, 3.40000E+01, 2.20000E+01, 2.10000E+01, 4.40000E+01, 1.10000E+01, 2.30000E+01, 7.00000E+00, 9.00000E+00, 1.10000E+01, 1.20000E+01, 1.30000E+01, 1.40000E+01, 1.50000E+01, 1.60000E+01, 2.20000E+01, 2.00000E+00, 1.00000E+00, 2.00000E+00, 1.10000E+01, 2.10000E+01, 4.00000E+00, 2.30000E+01, 7.00000E+00, 2.80000E+01),
  n=c(1.07000E+02, 9.20000E+01, 1.14000E+02, 1.21000E+02, 1.05000E+02, 9.20000E+01, 9.30000E+01, 8.00000E+01, 1.12000E+02, 9.40000E+01, 1.30000E+02, 1.45000E+02, 9.20000E+01, 1.09000E+02, 1.45000E+02, 1.01000E+02, 9.40000E+01, 1.32000E+02, 1.29000E+02, 1.46000E+02, 1.42000E+02, 1.02000E+02, 1.11000E+02, 1.07000E+02, 1.43000E+02, 9.00000E+01, 1.10000E+02, 8.60000E+01, 9.60000E+01, 1.09000E+02, 9.90000E+01, 1.13000E+02, 8.80000E+01, 9.00000E+01, 1.14000E+02, 8.30000E+01, 1.20000E+02, 1.33000E+02, 1.36000E+02, 1.27000E+02, 8.30000E+01, 8.70000E+01, 9.40000E+01, 9.70000E+01, 1.12000E+02, 1.14000E+02, 1.30000E+02, 1.31000E+02, 1.39000E+02, 1.46000E+02, 8.00000E+01, 1.10000E+02, 9.60000E+01, 1.35000E+02, 1.07000E+02, 9.80000E+01, 1.40000E+02, 1.06000E+02, 1.50000E+02, 8.10000E+01, 1.11000E+02, 1.02000E+02, 9.00000E+01, 8.60000E+01, 1.11000E+02, 1.05000E+02, 1.26000E+02, 1.40000E+02, 1.42000E+02, 1.35000E+02, 1.35000E+02, 1.36000E+02, 1.29000E+02, 1.43000E+02, 1.35000E+02, 1.26000E+02, 1.21000E+02, 1.01000E+02, 1.02000E+02, 1.27000E+02, 1.45000E+02, 1.27000E+02, 8.50000E+01, 9.50000E+01, 9.70000E+01, 9.90000E+01, 1.04000E+02, 1.05000E+02, 1.17000E+02, 1.33000E+02, 1.44000E+02, 1.37000E+02, 1.11000E+02, 1.35000E+02, 1.22000E+02, 1.25000E+02, 1.04000E+02, 1.38000E+02, 1.05000E+02, 1.48000E+02, 8.90000E+01, 8.80000E+01, 1.05000E+02, 1.10000E+02, 1.33000E+02, 1.34000E+02, 1.41000E+02, 1.41000E+02, 1.45000E+02, 9.10000E+01, 9.70000E+01, 1.08000E+02, 1.16000E+02, 1.31000E+02, 1.50000E+02, 1.45000E+02, 1.11000E+02, 1.15000E+02, 1.36000E+02, 1.41000E+02, 1.26000E+02, 1.31000E+02, 9.00000E+01, 1.01000E+02, 1.02000E+02, 1.03000E+02, 1.09000E+02, 1.34000E+02, 1.33000E+02),
  observations=1.29000E+02, regions=1.00000E+01, 
  ase1=6.32000E+01, bse1=3.33000E+00, asp1=1.01430E+02, bsp1=1.12700E+01, 
  amu=2.02700E+01, bmu=8.10700E+01, apsi=8.97000E+00, bpsi=2.79000E+00, 
  ahp=2.40000E+00, bhp=9.61000E+00, arp=6.24000E+00, brp=1.45500E+01)

#inits

inits<-list(psi=5.00000E-01)


SaveParams <- c("main.ap","main.psi","sub.p","pre.pstar.rep") # ,"reg.sub.p"

generic_jags<-jagsoutput_TruezeroMult<-rjags::jags.model(data=list(n=Data$n,y=Data$y, 
                                                                   observations=Data$observations,
                                                                   regions=Data$region,
                                                                   amu=Data$amu, bmu=Data$bmu,
                                                                   apsi=Data$apsi, bpsi=Data$bpsi,
                                                                   ahp=Data$ahp,bhp=Data$bhp,
                                                                   arp=Data$arp,brp=Data$brp,
                                                                   ase1=Data$ase1,bse1=Data$bse1,
                                                                   asp1=Data$asp1,bsp1=Data$bsp1),
                                                         inits=NULL, n.chains=2,
                                                         n.adapt = floor(100000/10),
                                                         file=paste("FullHierarchical.txt")) # ,quiet=TRUE
