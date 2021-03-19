gss<-function (S, family = NA, description = NA, burnin = TRUE, par_labels = NA, 
          sort = TRUE, keep_original_order = FALSE, splitting = FALSE, 
          inc_warmup = FALSE, stan_include_auxiliar = FALSE) 
{
  processed <- FALSE
  original.object.class <- class(S)[1]
  if (length(class(S)) > 1 & class(S)[1] == "stanreg") {
    S <- S$stanfit
  }
  if (class(S) == "brmsfit") {
    S <- S$fit
  }
  if (class(S) == "stanfit") {
    nChains <- S@sim$chains
    nThin <- S@sim$thin
    mDescription <- S@model_name
    D <- NULL
    for (l in 1:nChains) {
      sdf <- as.data.frame(S@sim$samples[[l]])
      names(sdf) <- names(S@sim$samples[[l]])
      sdf$Iteration <- 1:dim(sdf)[1]
      s <- tidyr::gather(sdf, Parameter, value, -Iteration) %>% 
        dplyr::mutate(Chain = l) %>% dplyr::select(Iteration, 
                                                   Chain, Parameter, value)
      D <- dplyr::bind_rows(D, s)
    }
    if (!inc_warmup) {
      if (original.object.class == "stanfit") {
        D <- dplyr::filter(D, Iteration > (S@sim$warmup/nThin))
        D$Iteration <- D$Iteration - (S@sim$warmup/nThin)
      }
      nBurnin <- S@sim$warmup
    }
    else {
      nBurnin <- 0
    }
    if (!stan_include_auxiliar) {
      D <- dplyr::filter(D, Parameter != "lp__")
    }
    if (sort) {
      D$Parameter <- factor(D$Parameter, levels = custom.sort(D$Parameter))
    }
    else {
      D$Parameter <- factor(D$Parameter)
    }
    processed <- TRUE
    D <- dplyr::as_tibble(D)
  }
  if (class(S) == "list") {
    D <- NULL
    for (i in 1:length(S)) {
      samples.c <- dplyr::as_tibble(read.table(S[[i]], 
                                               sep = ",", header = TRUE, colClasses = "numeric", 
                                               check.names = FALSE))
      D <- dplyr::bind_rows(D, tidyr::gather(samples.c, 
                                             Parameter) %>% dplyr::mutate(Iteration = rep(1:(dim(samples.c)[1]), 
                                                                                          dim(samples.c)[2]), Chain = i) %>% dplyr::select(Iteration, 
                                                                                                                                           Chain, Parameter, value))
    }
    if (!stan_include_auxiliar) {
      D <- D[grep("__$", D$Parameter, invert = TRUE), 
      ]
      if (sort) {
        D$Parameter <- factor(D$Parameter, levels = custom.sort(D$Parameter))
      }
      else {
        D$Parameter <- factor(D$Parameter)
      }
    }
    nBurnin <- as.integer(gsub("warmup=", "", 
                               scan(S[[i]], "", skip = 12, nlines = 1, quiet = TRUE)[2]))
    nThin <- as.integer(gsub("thin=", "", scan(S[[i]], 
                                               "", skip = 13, nlines = 1, quiet = TRUE)[2]))
    processed <- TRUE
  }
  if (class(S) == "mcmc.list" | class(S) == "mcmc" | 
      processed) {
    if (!is.na(family)) {
      requireNamespace("coda")
      if (!processed) {
        location.family <- grep(family, dimnames(S[[1]])[[2]])
        S <- S[, location.family, drop = FALSE]
      }
      else {
        D <- D %>% filter(grepl(family, Parameter)) %>% 
          mutate(Parameter = as.factor(as.character(Parameter)))
      }
    }
    if (!processed) {
      lS <- length(S)
      D <- NULL
      if (lS == 1 | class(S) == "mcmc") {
        if (lS == 1 & class(S) == "mcmc.list") {
          s <- S[[1]]
        }
        else {
          s <- S
        }
        if (keep_original_order) {
          parameter.names.original.order <- dimnames(s)[[2]]
        }
        D <- dplyr::mutate(ggs_chain(s), Chain = 1) %>% 
          dplyr::select(Iteration, Chain, Parameter, 
                        value)
        nBurnin <- (attributes(s)$mcpar[1]) - (1 * attributes(s)$mcpar[3])
        nThin <- attributes(s)$mcpar[3]
      }
      else {
        if (keep_original_order) {
          parameter.names.original.order <- dimnames(S[[1]])[[2]]
        }
        for (l in 1:lS) {
          s <- S[l][[1]]
          D <- dplyr::bind_rows(D, dplyr::mutate(ggs_chain(s), 
                                                 Chain = l))
        }
        D <- dplyr::select(D, Iteration, Chain, Parameter, 
                           value)
        nBurnin <- (attributes(s)$mcpar[1]) - (1 * attributes(s)$mcpar[3])
        nThin <- attributes(s)$mcpar[3]
      }
      if (sort) {
        D$Parameter <- factor(D$Parameter, levels = custom.sort(D$Parameter))
      }
      else {
        D$Parameter <- factor(D$Parameter)
      }
      if (keep_original_order) {
        D$Parameter <- factor(D$Parameter, levels = parameter.names.original.order)
      }
      D <- dplyr::arrange(D, Parameter, Chain, Iteration)
    }
    attr(D, "nChains") <- length(unique(D$Chain))
    attr(D, "nParameters") <- length(unique(D$Parameter))
    attr(D, "nIterations") <- max(D$Iteration)
    if (is.numeric(burnin) & length(burnin) == 1) {
      attr(D, "nBurnin") <- burnin
    }
    else if (is.logical(burnin)) {
      if (burnin) {
        attr(D, "nBurnin") <- nBurnin
      }
      else {
        attr(D, "nBurnin") <- 0
      }
    }
    else {
      stop("burnin must be either logical (TRUE/FALSE) or a numerical vector of length one.")
    }
    attr(D, "nThin") <- nThin
    if (is.character(description)) {
      attr(D, "description") <- description
    }
    else {
      if (!is.na(description)) {
        message("description is not a text string. The name of the imported object is used instead.")
      }
      if (exists("mDescription")) {
        attr(D, "description") <- mDescription
      }
      else {
        attr(D, "description") <- as.character(sys.call()[2])
      }
    }
    if (!is.na(family)) {
      D <- get_family(D, family = family)
    }
    if (length(which(class(par_labels) %in% c("data.frame", 
                                              "tbl_df"))) >= 1) {
      if (length(which(c("Parameter", "Label") %in% 
                       names(par_labels))) == 2) {
        aD <- attributes(D)
        levels(D$Parameter)[which(levels(D$Parameter) %in% 
                                    par_labels$Parameter)] <- as.character(par_labels$Label[match(levels(D$Parameter)[which(levels(D$Parameter) %in% 
                                                                                                                              par_labels$Parameter)], par_labels$Parameter)])
        L <- dplyr::as_tibble(data.frame(Parameter = par_labels$Label, 
                                         ParameterOriginal = par_labels$Parameter)) %>% 
          mutate(Parameter = as.character(Parameter))
        D <- suppressWarnings(dplyr::left_join(D, L, 
                                               by = "Parameter"))
        D <- D %>% dplyr::select(Iteration, Chain, Parameter, 
                                 value, ParameterOriginal)
        if (class(D$Parameter) == "character") {
          if (sort) {
            D$Parameter <- factor(D$Parameter, levels = custom.sort(D$Parameter))
          }
          else {
            D$Parameter <- factor(D$Parameter)
          }
        }
        attr(D, "nChains") <- aD$nChains
        attr(D, "nParameters") <- aD$nParameters
        attr(D, "nIterations") <- aD$nIterations
        attr(D, "nBurnin") <- aD$nBurnin
        attr(D, "nThin") <- aD$nThin
        attr(D, "description") <- aD$description
        if (dim(par_labels)[2] > 2) {
          aD <- attributes(D)
          L.noParameter <- dplyr::as_tibble(par_labels) %>% 
            dplyr::select(-Parameter) %>% dplyr::mutate(Label = as.character(Label))
          D <- suppressWarnings(dplyr::left_join(D, L.noParameter, 
                                                 by = c(Parameter = "Label")))
          if (class(D$Parameter) == "character") {
            if (sort) {
              D$Parameter <- factor(D$Parameter, levels = custom.sort(D$Parameter))
            }
            else {
              D$Parameter <- factor(D$Parameter)
            }
          }
        }
        attr(D, "nChains") <- aD$nChains
        attr(D, "nParameters") <- aD$nParameters
        attr(D, "nIterations") <- aD$nIterations
        attr(D, "nBurnin") <- aD$nBurnin
        attr(D, "nThin") <- aD$nThin
        attr(D, "description") <- aD$description
      }
      else {
        stop("par_labels must include at least columns called 'Parameter' and 'Label'.")
      }
    }
    else {
      if (!is.na(par_labels)) {
        stop("par_labels must be a data frame or a tibble.")
      }
    }
    if (splitting) {
      original.attributes <- attributes(D)
      if ((original.attributes$nIterations%%2) != 0) {
        D <- dplyr::filter(D, Iteration < max(Iteration))
        original.attributes$nIterations <- original.attributes$nIterations - 
          1
      }
      D <- D %>% dplyr::mutate(second.half = ifelse(Iteration > 
                                                      (original.attributes$nIterations/2), 1, 0)) %>% 
        dplyr::mutate(Chain = as.integer((((Chain - 1) * 
                                             2) + second.half) + 1)) %>% dplyr::mutate(Iteration = as.integer(ifelse(second.half == 
                                                                                                                       1, Iteration - (original.attributes$nIterations/2), 
                                                                                                                     Iteration))) %>% dplyr::select(Parameter, Chain, 
                                                                                                                                                    Iteration, value)
      attr(D, "nChains") <- original.attributes$nChains * 
        2
      attr(D, "nParameters") <- original.attributes$nParameters
      attr(D, "nIterations") <- original.attributes$nIterations/2
      attr(D, "nBurnin") <- original.attributes$nBurnin
      attr(D, "nThin") <- original.attributes$nThin
      attr(D, "description") <- original.attributes$description
    }
    if (is.null(attributes(D)$nBurnin)) 
      attr(D, "nBurnin") <- 0
    if (is.null(attributes(D)$nThin)) 
      attr(D, "nThin") <- 1
    return(D)
  }
  else {
    stop("ggs is not able to transform the input object into a ggs object suitable for ggmcmc.")
  }
}