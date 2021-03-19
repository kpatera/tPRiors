findbetaqq2<-function (percentile.value1, percentile1, percentile.value2, 
          percentile2) 
{
  findcentiles <- function(x) {
    c(F1 = qbeta(percentile1, x[1], x[2]) - percentile.value1, 
      F2 = qbeta(percentile2, x[1], x[2]) - percentile.value2)
  }
  ss <- rootSolve::multiroot(f = findcentiles, start = c(1, 1))
  finalshape1 = ss$root[1]
  finalshape2 = ss$root[2]
  sample_beta = rbeta(10000, finalshape1, finalshape2)
  
  return(
    list(a = round(finalshape1, 2), 
         b = round(finalshape2, 2),
         Percentile1 =percentile1,
         Percentile2= percentile2#,
         # text=paste("The desired Beta distribution that satisfies the specified conditions is: Beta(", 
         #            round(finalshape1, 2), round(finalshape2, 2), ")"),
         # summary=summary(sample_beta)
         
    )
  )
}
