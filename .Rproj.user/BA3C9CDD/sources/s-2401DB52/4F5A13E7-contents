# Boxplots for regions?


# OPEN (Moderate) Timer for priors/analysis.
# 2. OPEN (Large) - Allow for the ability to change different options (i.e. mean median percentile)  
  #for different prior specifications. So can we move this option at the beginning of each prior tag?




#True prevalence, n populations
# DONE (small) - For mu, psi priors it is better if you could give them the plot of a Be(mu*psi, psi*(1-mu)) 
#that will help them visualise what their prior is telling about prevalence overall.
# DONE (moderate) YOU ARE HERE! - Se and Sp priors should be standard (not with mu psi). The four last options should be removed.
# DONE (moderate) - Se/Sp priors and zero prevalence priors should not have the mu,psi options.
# 4. DONE (Moderate) - We need to include the option for non-informative priors for each prior tag 
#(where we can have convergence with non informative priors - all apparent prevalence options).
# DONE - Collect info for shiny competition
#https://blog.rstudio.com/2021/03/11/time-to-shiny/
#https://rstudio.cloud/project/2459882
# 6. DONE (CHECK) - Η μπάρα παίζει στα priors (το ξέρεις..)

# 5. DONE (Moderate similar to 1) - Model setup: perhaps have the update only after clicking the “Fix model”?
# 3. DONE (Small + 1) - Percentile specifications to have limit/step of 1 (i.e. 0.90, 0.91, 0.92 etc)
# 1. DONE (Moderate) - Make the priors to update not in real time but once we hit the “set” button. 
# This will also help with the bar that moves?
# OPEN (DONE?) - Φαίνεται πως όταν αλλάζω κάτι στα mu psi των Se/Sp αλλάζει και το mu/spi. 
#Κάποια παράμετρος πρέπει να είναι κοινή/ξεχασμένη.
# DONE - Maybe limit MCMC number to 3?
# DONE - TRUE or FALSE to be replaced with Yes/No (easier for non experts).
# DONE - Further, T/F (Y/N) should precede the specification of the percentile. 
# DONE - Is the percentile the upper limit of the Median ?” is logical to precede the percentile specification 
  #(i.e. move this one up). So 
#Specify your prior belief about the mean:
# DONE - Specify the level of confidence that the true value of the mean is greater or lower than the percentile.value:
# DONE - Is the percentile the upper limit of the mean? (Yes/No)
# DONE - Specify the lower/upper limit for the mean/median/mode at the specified level of confidence:
# Apparent prevalence, 1 pop
# DONE - Priors: Add a tag specifying that this is the prevalence prior we are specifying
# DONE (discuss) - If we are not giving the option to account for zero prevalence this option 
  #does not have to appear at all?
  




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
Example1_4Studies <<- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_4studies.xlsx"))
Example2_40Studies <<- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_Attica.xlsx"))
Example3_102Studies <<- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d_NonAttica.xlsx"))
Example4_129studies <<- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d.xlsx"))
name_data<-"sidebar user input"
