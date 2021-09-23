# OPEN - Allow for the ability to change different options (i.e. mean median percentile)  
#for different prior specifications. So can we move this option at the beginning of each prior tag?
#---- Load packages ----#
packages.list<-c("shiny","shinythemes","shinyWidgets","shinydashboard","shinycssloaders",
                 "PriorGen","R2jags","ggplot2","tidyr","rjags","xlsx","writexl",
                 "shinyjs","rmarkdown","markdown","plotly","ggmcmc","knitr","shinyalert",
                 "grid","DT","readxl","gridExtra","psych","rootSolve","devtools","gdata","gtools","waiter") # ,
#packages.new <- packages.list[!(packages.list %in% installed.packages()[,"Package"])]
#install.packages(packages.new);rm(packages.new)
lapply(packages.list, require, character.only = TRUE); rm(packages.list)
#devtools::install_github('ropensci/plotly')
#require("ropensci/plotly")
#---- Load functions and extra functionss ----#
Funcs<-c("findbeta2.R","findbetaqq2.R","findbetamupsi.R","findbetamupsi2.R","ggs_density.R",
         "get_family.R","multiroot.R","perturb.R","gss.R")
invisible(lapply(Funcs,function(i){ source(paste("Functions/",i,sep="")) }))
Model1.mcmc<<-NULL
#---- Create temp variable for Rmarkdown reports and plotting ----#
fb<-list(1,1)

#---- Load pre-loaded datasets ----#
require(readxl)
Example1_4Studies <<- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_4studies.xlsx"))
Example2_40Studies <<- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_Attica.xlsx"))
#Example3_102Studies <<- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_NonAttica.xlsx"))
Example4_129studies <<- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d.xlsx"))
name_data<-"sidebar user input"
# Example2_40Studies_2cols <<- structure(list(positive = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
#                                                          1, 0, 1, 1, 2, 3, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 2, 0, 2, 1, 2, 
#                                                          2, 2, 2, 3, 3, 3, 3, 3), n = c(107, 92, 114, 121, 105, 92, 93, 
#                                                                                         80, 112, 94, 130, 145, 92, 109, 145, 101, 94, 132, 129, 146, 
#                                                                                         142, 102, 111, 107, 143, 90, 110, 86, 96, 109, 99, 113, 88, 90, 
#                                                                                         114, 83, 120, 133, 136, 127)), class = "data.frame", row.names = c(NA, 
#                                                                                                                                                            -40L))
