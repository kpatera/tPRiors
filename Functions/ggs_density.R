ggs_density<-function (D, family = NA, rug = FALSE, hpd = FALSE, greek = FALSE) 
{
  if (!is.na(family)) {
    D <- get_family(D, family = family)
  }
  if (attributes(D)$nChains <= 1) {
    f <- ggplot(D, aes(x = value))
  }
  else {
    f <- ggplot(D, aes(x = value, colour = as.factor(Chain), 
                       fill = as.factor(Chain)))
  }
  f <- f + geom_density(alpha = 0.3) + scale_fill_discrete(name = "Chain") + 
    scale_colour_discrete(name = "Chain")
  if (!greek) {
    f <- f + facet_wrap(~Parameter, ncol = 2, scales = "free")
  }
  else {
    f <- f + facet_wrap(~Parameter, ncol = 2, scales = "free", 
                        labeller = label_parsed)
  }
  if (rug) 
    f <- f + geom_rug(alpha = 0.1)
  if (hpd) {
    ciD <- ci(D)
    f <- f + geom_segment(data = ciD, size = 0.5, inherit.aes = FALSE, 
                          aes(x = low, xend = high, y = 0, yend = 0)) + geom_segment(data = ciD, 
                                                                                     size = 1, inherit.aes = FALSE, aes(x = Low, xend = High, 
                                                                                                                        y = 0, yend = 0))
  }
  return(f)
}
