findbetamupsi2<-function (themean, percentile = 0.95, lower.v = T, percentile.value, 
                          psi.percentile = 0.9, percentile.median, percentile95value) 
{
  if (lower.v == T) {
    pr_n = percentile
  }
  else {
    pr_n = 1 - percentile
  }
  stopifnot((lower.v == T && themean <= percentile.value) | 
              (lower.v == F && themean >= percentile.value))
  stopifnot(percentile.median > themean && percentile95value > 
              percentile.median)
  stopifnot((lower.v == T && percentile.value < percentile.median) | 
              (lower.v == F && percentile.value != percentile.median))
  stopifnot(psi.percentile > 0.5 && psi.percentile < 1)
  a = runif(1, 1, 10)
  to.minimize <- function(a) {
    abs(qbeta(pr_n, shape1 = a, shape2 = a * (1 - themean)/themean) - 
          percentile.value)
  }
  estimate <- optim(runif(1, 1, 10), to.minimize, lower = 0.1, 
                    upper = 10^4, method = "Brent")
  finalshape1 = estimate$par
  finalshape2 = finalshape1 * (1 - themean)/themean
  mu = finalshape1/(finalshape1 + finalshape2)
  gu = finalshape1 + finalshape2
  f <- function(ga) abs(qbeta(psi.percentile, mu * ga, ga * 
                                (1 - mu)) - percentile.median)
  r <- optim(1, f, lower = 0, upper = 10^4, method = "Brent")
  alpha_t = r$par[1]
  f <- function(ga) abs(qbeta(psi.percentile, mu * ga, ga * 
                                (1 - mu)) - percentile95value)
  r <- optim(1, f, lower = 0, upper = 10^4, method = "Brent")
  beta_t = r$par[1]
  
  
  model <- function(x) c(F1 = qgamma(0.5, shape = x[1], scale = 1/x[2]) - 
                           alpha_t, F2 = qgamma(0.05, shape = x[1], scale = 1/x[2]) - 
                           beta_t)
  ss2 <- multiroot(f = model, start = c(5, 2), positive = T,useFortran = F)
  
  
  qgamma(0.5, ss2$root[1], ss2$root[2])
  qgamma(0.95, ss2$root[1], ss2$root[2])
  a = rgamma(10000, ss2$root[1], ss2$root[2])
  b = rbeta(10000, finalshape1, finalshape2)
  
  print(summary((rbeta(10000, a * b, a * (1 - b)))))
  
  return(
    list(abeta = round(finalshape1, 2), 
         bbeta = round(finalshape2, 2),
         agamma =round(ss2$root[1], 2),
         bgamma= round(ss2$root[2], 2),
         atotalbeta=mean(a)*mean(b),
         btotalbeta=mean(a)*(1-mean(b))#,
         # text_b=paste("The desired Beta distribution that satisfies the specified conditions about the mean of the prevalence 'mu' is: Beta(", 
         #             round(finalshape1, 2), round(finalshape2, 2), ")"),
         # text_g=paste("The desired Gamma distribution that satisfies the specified conditions about the variability 'psi' of the prevalence is: Gamma(", 
         #              round(ss2$root[1], 2), round(ss2$root[2], 2), ")"),
         # summary= summary((rbeta(10000, a * b, a * (1 - b))))
    )
  )
}
