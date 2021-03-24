# Fix nthin, nchains and nniter of multiple!!!

#---- Load packages ----#
packages.list<-c("shiny","shinythemes","shinyWidgets","shinydashboard",
                 "PriorGen","R2jags","ggplot2","tidyr","rjags",
                 "shinyjs","rmarkdown","markdown","plotly","ggmcmc","knitr","shinyalert",
                 "grid","DT","readxl","gridExtra","psych","rootSolve","perturb","devtools") # ,
#install.packages("Rtools")
#install.packages("/APre_IWA/Functions/perturb_2.10.tar.gz", repos = NULL, type="source",
#                 dependencies = TRUE)
#packages.new <- packages.list[!(packages.list %in% installed.packages()[,"Package"])]
#install.packages(packages.new);rm(packages.new)
#install.packages("DT"),
#install.packages("shinyalert")
#install.packages("shinydashboard")
# install.packages("shiny")
# install.packages("shinywidgets")
# install.packages("shinythemes")
# install.packages("shinydashboard")
# install.packages("shinyjs")
# install.packages("rmarkdown")
# install.packages("markdown")
# install.packages("R2jags")
# install.packages("plotly")
# install.packages("knitr")
# install.packages("grid")
# install.packages("psych")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("tidyr")
# install.packages("ggmcmc")
# install.packages("PriorGen")
# install.packages("readxl")
lapply(packages.list, require, character.only = TRUE); rm(packages.list)

#---- Load functions and extra functionss ----#
Funcs<-c("findbeta2.R","findbetaqq2.R","findbetamupsi.R","findbetamupsi2.R","ggs_density.R",
         "get_family.R","multiroot.R","perturb.R","gss.R")
invisible(lapply(Funcs,function(i){ source(paste("Functions/",i,sep="")) }))

#---- Create temp variable for Rmarkdown reports and plotting ----#
fb<-list(1,1)

#---- Load pre-loaded datasets ----#

Data1 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_4studies.xlsx"))
Data2 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_Attica.xlsx"))
Data3 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_NonAttica.xlsx"))
Data4 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d.xlsx"))

