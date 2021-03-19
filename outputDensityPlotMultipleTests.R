invisible(require(plotly))
invisible(library(readxl))
invisible(library(ggplot2))
Dataa<- data.frame(read_excel("Data/efs31678e-sup-0004-annex_d.xlsx"))
Dataa$test<-factor(Dataa$test)
Dataa$region<-factor(Dataa$region)
Dataa$country<-factor(Dataa$country)
Dataa$Ap<-Dataa$positive/Dataa$n
Dataa$Ap_01<-Dataa$Ap>0.1
#Density plof to present multiple distributions
p <- ggplot(Dataa, aes(Ap, fill = test)) + geom_density(alpha = 0.2)
fig <- ggplotly(p)
fig

# Boxplot for presentating data.
fig <- plot_ly(Dataa, x = ~test region, y = ~Ap, color = ~, type = "box")
fig <- fig %>% layout(boxmode = "group")

fig
