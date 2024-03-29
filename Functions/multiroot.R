
multiroot<-function (f, start, maxiter = 100, rtol = 1e-06, atol = 1e-08, 
          ctol = 1e-08, useFortran = FALSE, positive = FALSE, jacfunc = NULL, 
          jactype = "fullint", verbose = FALSE, bandup = 1, banddown = 1, 
          parms = NULL, ...) 
{
  initfunc <- NULL
  if (is.list(f)) {
    if (!is.null(jacfunc) & "jacfunc" %in% names(f)) 
      stop("If 'f' is a list that contains jacfunc, argument 'jacfunc' should be NULL")
    jacfunc <- f$jacfunc
    initfunc <- f$initfunc
    f <- f$func
  }
  N <- length(start)
  if (!is.numeric(start)) 
    stop("start conditions should be numeric")
  if (!is.numeric(maxiter)) 
    stop("`maxiter' must be numeric")
  if (as.integer(maxiter) < 1) 
    stop("maxiter must be >=1")
  if (!is.numeric(rtol)) 
    stop("`rtol' must be numeric")
  if (!is.numeric(atol)) 
    stop("`atol' must be numeric")
  if (!is.numeric(ctol)) 
    stop("`ctol' must be numeric")
  if (length(atol) > 1 && length(atol) != N) 
    stop("`atol' must either be a scalar, or as long as `start'")
  if (length(rtol) > 1 && length(rtol) != N) 
    stop("`rtol' must either be a scalar, or as long as `y'")
  if (length(ctol) > 1) 
    stop("`ctol' must be a scalar")
  if (useFortran) {
    if (!is.compiled(f) & is.null(parms)) {
      Fun1 <- function(time = 0, x, parms = NULL) list(f(x, 
                                                         ...))
      Fun <- Fun1
    }
    else if (!is.compiled(f)) {
      Fun2 <- function(time = 0, x, parms) list(f(x, parms, 
                                                  ...))
      Fun <- Fun2
    }
    else {
      Fun <- f
      f <- function(x, ...) Fun(n = length(start), t = 0, 
                                x, f = rep(0, length(start)), 1, 1)$f
    }
    JacFunc <- jacfunc
    if (!is.null(jacfunc)) 
      if (!is.compiled(JacFunc) & is.null(parms)) 
        JacFunc <- function(time = 0, x, parms = parms) jacfunc(x, 
                                                                ...)
    else if (!is.compiled(JacFunc)) 
      JacFunc <- function(time = 0, x, parms = parms) jacfunc(x, 
                                                              parms, ...)
    else JacFunc <- jacfunc
    method <- "stode"
    if (jactype == "sparse") {
      method <- "stodes"
      if (!is.null(jacfunc)) 
        stop("jacfunc can not be used when jactype='sparse'")
      x <- stodes(y = start, time = 0, func = Fun, atol = atol, 
                  positive = positive, rtol = rtol, ctol = ctol, 
                  maxiter = maxiter, verbose = verbose, parms = parms, 
                  initfunc = initfunc)
    }
    else x <- steady(y = start, time = 0, func = Fun, atol = atol, 
                     positive = positive, rtol = rtol, ctol = ctol, maxiter = maxiter, 
                     method = method, jacfunc = JacFunc, jactype = jactype, 
                     verbose = verbose, parms = parms, initfunc = initfunc, 
                     bandup = bandup, banddown = banddown)
    precis <- attr(x, "precis")
    attributes(x) <- NULL
    x <- unlist(x)
    if (is.null(parms)) 
      reffx <- f(x, ...)
    else reffx <- f(x, parms, ...)
    i <- length(precis)
  }
  else {
    #if (is.compiled(f)) 
    #  stop("cannot combine compiled code with R-implemented solver")
    precis <- NULL
    x <- start
    jacob <- matrix(nrow = N, ncol = N, data = 0)
    if (is.null(parms)) 
      reffx <- f(x, ...)
    else reffx <- f(x, parms, ...)
    if (length(reffx) != N) 
      stop("'f', function must return as many function values as elements in start")
    for (i in 1:maxiter) {
      refx <- x
      pp <- mean(abs(reffx))
      precis <- c(precis, pp)
      ewt <- rtol * abs(x) + atol
      if (max(abs(reffx/ewt)) <= 1) 
        break
      delt <- jitter(x)
      for (j in 1:N) {
        x[j] <- x[j] + delt[j]
        if (is.null(parms)) 
          fx <- f(x, ...)
        else fx <- f(x, parms, ...)
        jacob[, j] <- (fx - reffx)/delt[j]
        x[j] <- refx[j]
      }
      relchange <- as.numeric(solve(jacob, -1 * reffx))
      if (max(abs(relchange)) < ctol) 
        break
      x <- x + relchange
      if (is.null(parms)) 
        reffx <- f(x, ...)
      else reffx <- f(x, parms, ...)
    }
  }
  names(x) <- names(start)
  return(list(root = x, f.root = reffx, iter = i, estim.precis = precis[length(precis)]))
}