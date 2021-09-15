Names<-c("Zaheer","Clara","Sintayehu","Neville","Dmitry","Marcel","Sanjaya",
"Ali Mertcan","Fatih","Pavle","Alexia","Peter Francis","Yasin","Xhelil",
"Eda","Jewel","Naime","Anthony","Domniki")
seed<-as.numeric(Sys.time())
set.seed(seed)
Groups<-data.frame(Names=sample(Names,size = length(Names),replace = F),
                   Groups_MCMC=c(rep(1:9,each=2),9),
                   Groups_tPRiors=c(rep(1:3,each=6),3))

Groups

