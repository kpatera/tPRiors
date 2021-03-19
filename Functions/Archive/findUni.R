findbeta2<-function (themean = 0.3, percentile = 0.95, lower.v = F, variance=0.6) 
{
  stopifnot((lower.v == T && themean <= percentile.value) | 
              (lower.v == F && themean >= percentile.value))

  if (lower.v == T) {
    pr_n = percentile
  }
  else {
    pr_n = 1 - percentile
  }
  k=sqrt(3*variance)
  shape1=themean-k
  shape2=themean+k
  shape1;shape2
  summary(runif(100000,shape1,shape2))
  var(runif(100000,shape1,shape2))
  
  return(
    list(a = round(shape1, 2), 
         b = round(shape2, 2),
         Per =round(qunif(pr_n, shape1, shape2), 2)
         # Text=paste("The desired Beta distribution that satisfies the specified conditions is: Beta(", 
         #       round(finalshape1, 2), round(finalshape2, 2), ")"),
         # summary=summary(sample_beta)
    )
  )
}
