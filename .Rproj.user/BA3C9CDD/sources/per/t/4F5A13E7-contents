# Boxplots for regions?
# Timer for analysis.
# Check conditions multiple+true

#---- Load packages ----#
packages.list<-c("shiny","shinythemes","shinyWidgets","shinydashboard",
                 "PriorGen","R2jags","ggplot2","tidyr","rjags",
                 "shinyjs","rmarkdown","markdown","plotly","ggmcmc","knitr","shinyalert",
                 "grid","DT","readxl","gridExtra","psych","rootSolve","devtools") # ,
#packages.new <- packages.list[!(packages.list %in% installed.packages()[,"Package"])]
#install.packages(packages.new);rm(packages.new)
lapply(packages.list, require, character.only = TRUE); rm(packages.list)

#---- Load functions and extra functionss ----#
Funcs<-c("findbeta2.R","findbetaqq2.R","findbetamupsi.R","findbetamupsi2.R","ggs_density.R",
         "get_family.R","multiroot.R","perturb.R","gss.R")
invisible(lapply(Funcs,function(i){ source(paste("Functions/",i,sep="")) }))

#---- Create temp variable for Rmarkdown reports and plotting ----#
fb<-list(1,1)

#---- Load pre-loaded datasets ----#
require(readxl)
Data1 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_4studies.xlsx"))
Data2 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_Attica.xlsx"))
Data3 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_NonAttica.xlsx"))
Data4 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d.xlsx"))
name_data<-"sidebar user input"
