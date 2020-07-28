#### Comparison plots for CMSY, SPICT and ICES assessments
library (ggplot2); theme_set(theme_bw())
library(spict)
library (reshape)
library (gridExtra)
library (egg)
library(reshape2)

stocklist <- c("Cod6a", "Cod7ek", 
               "CodFaroe", "CodNS", 
               "Had6b","Had7bk" , "HadNS" , 
               "Ple7a", "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")

Cod6a.data = read.csv("Data/Cod_6_a/Cod.6a.Data.csv")
Cod7ek.data = read.csv("Data/Cod_7_ek/Cod.7ek.Data.csv")
CodFaroe.data = read.csv("Data/Cod_Faroe/Cod.Faroe.Data.csv")
CodNS.data = read.csv("Data/Cod_NS/Cod.NS.Data.csv")
Had6b.data = read.csv("Data/Had_6_b/Had.6b.Data.csv")
Had7bk.data = read.csv("Data/Had_7_bk/Had.7bk.Data.csv")
HadNS.data = read.csv("Data/Had_NS/Had.NS.Data.csv")
Ple7a.data = read.csv("Data/Ple_7_a/Ple.7a.Data.csv")
Ple7hk.data = read.csv("Data/Ple_7_hk/Ple.7hk.Data.csv")
Sol7a.data = read.csv("Data/Sol_7_a/Sol.7a.Data.csv")
Sol7fg.data = read.csv("Data/Sol_7_fg/Sol.7fg.Data.csv")
Sol7hk.data = read.csv("Data/Sol_7_hk/Sol.7hk.Data.csv")
Sol2024.data = read.csv("Data/Sol_2024/Sol.2024.Data.csv")
Whg6a.data = read.csv("Data/Whg_6_a/Whg.6a.Data.csv")
Whg7a.data = read.csv("Data/Whg_7_a/Whg.7a.Data.csv")
Whg7bk.data = read.csv("Data/Whg_7_bk/Whg.7bk.Data.csv")
WhgNS.data = read.csv("Data/Whg_NS/Whg.NS.Data.csv")

for (i in stocklist){

  data <- get((paste0 (i, ".data")), envir = .GlobalEnv)   

  data$variable <- "Catch"

  plot.catch <- ggplot(data)+
    geom_line(aes(x= year, y = catch/1000), size = 1)+
    facet_wrap(~data$variable )+
    ylab ("000 tonnes") +
    theme(axis.text.y = element_text(angle = 90))
  
  data2 <- melt(data, id.vars = "year")
  index.list <- c("index1", "index2", "index3", "index4", "index5", "index6")
  data3 <- data2[data2$variable %in% index.list, ]
  data3$value <- as.numeric(data3$value)
  
  for (j in unique(data3$variable)){
  data3$indexmax[data3$variable == j] <-  max(data3$value[data3$variable == j], na.rm = T)} 
  
  data3$propvalue <- data3$value/data3$indexmax  
  data3$title <- "Indices"
    
  plot.index <- ggplot(data3)+
    geom_line(aes(x= year, y = propvalue,  colour = variable, linetype = variable))+
    facet_wrap(~title )+
    ylab ("Index trend")+  
    theme(legend.title = element_blank())

 
full.ts <- ggarrange(plot.catch, plot.index,  ncol= 2, left = i)    

assign(paste0(i, ".tsplot"), full.ts)
    
}



pdf("Timeseries Plots/catchindex1.pdf", width = 6.69291, height = 12)
catchindex1 <-  grid.arrange(Cod6a.tsplot, Cod7ek.tsplot,
                             CodFaroe.tsplot, CodNS.tsplot, Had6b.tsplot, Had7bk.tsplot, ncol = 1)
dev.off()

pdf("Timeseries Plots/catchindex2.pdf", width = 6.69291, height = 12)
catchindex2 <-  grid.arrange(HadNS.tsplot, Ple7a.tsplot,
                             Ple7hk.tsplot, Sol7a.tsplot, Sol7fg.tsplot, Sol7hk.tsplot, ncol = 1)
dev.off()

pdf("Timeseries Plots/catchindex3.pdf", width = 6.69291, height = 10)
catchindex3 <-  grid.arrange(Sol2024.tsplot, Whg6a.tsplot, 
                             Whg7a.tsplot, Whg7bk.tsplot, WhgNS.tsplot, ncol = 1)
dev.off()

