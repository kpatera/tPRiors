# Check how to inlcude random fixed seed

#2 Add conditions for sliders in data input

# Fix rmarkdown for multiple error priorsprior not found 

# boxplot , grafima me ouritsa

# Dinatotita na katevazei dedomena .rdata 

# English time rmarkdown

#install.packages("shinydashboard")
packages.list<-c("shiny","shinythemes","shinyWidgets","shinydashboard",
                 "PriorGen","R2jags","ggplot2","tidyr","rjags",
                 "shinyjs","rmarkdown","markdown","plotly","ggmcmc","knitr",
                 "grid","DT","readxl","gridExtra","psych","rootSolve","perturb") # ,
                 #  ,,
library(devtools)
#install.packages("Rtools")
#install.packages("/APre_IWA/Functions/perturb_2.10.tar.gz", repos = NULL, type="source",
#                 dependencies = TRUE)
#packages.new <- packages.list[!(packages.list %in% installed.packages()[,"Package"])]
#install.packages(packages.new);rm(packages.new)
#install.packages("DT"),
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

# https://cran.r-project.org/web/packages/ggmcmc/vignettes/using_ggmcmc.html
#library(waiter)

source("Functions/findbeta2.R")
source("Functions/findbetaqq2.R")
source("Functions/findbetamupsi.R")
source("Functions/findbetamupsi2.R")
source("Functions/findbetamupsi3.R")
source("Functions/findbetamupsi4.R")
source("Functions/ggs_density.R")
source("Functions/get_family.R")
source("Functions/multiroot.R")
source("Functions/perturb.R")
source("Functions/gss.R")

fb<-list(1,1)
# pop<-"Single population"
# zero<-"No"
# pre<-"True prevalence"
# metric<-"Mean"

library(readxl)
Data1 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_4studies.xlsx"))
Data2 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_Attica.xlsx"))
Data3 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_NonAttica.xlsx"))
Data4 <- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d.xlsx"))

