###########################################
#### Jacobian comparison plots in a loop
###########################################
library (reshape)
library (ggplot2); theme_set(theme_bw())
library (gridExtra)
library (egg)
source ("jacobian prior functions.r")
# source ("jacobian plot functions.r")
source("Data/Ices ref points.r")
load("Data/Assessment Time Series Results.RData")

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

stocklist <- c("Cod6a", "Cod7ek", 
               "CodFaroe", "CodNS", 
               "Had6b","Had7bk" , "HadNS" , 
               "Ple7a", "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")

Gate = data.frame()

for (i in stocklist){
  
  data <- get((paste0 (i, ".data")), envir = .GlobalEnv) 
  
  
  ##################################################
  #### Priors for CMSY - used to plot biomass range points
  ct.raw <- data$catch# / 1000
  ct <- ma(data$catch)
  # Identify number of years and start/end years
  yr <- data$year # functions use this quantity
  nyr <- length(yr)
  start.yr <- min(yr)
  end.yr <- max(yr)
  
  pb.resilience = "Medium"
  pb.r.low = NA
  pb.r.hi = NA
  if (is.na(pb.r.low))  {if (pb.resilience == "High"){pb.r.low = 0.6; pb.r.hi = 1.5; start.r =  c(0.6,1.5)}
    else if (pb.resilience == "Medium"){pb.r.low = 0.2; pb.r.hi = 0.8; start.r =  c(0.2,0.8)}
    else if (pb.resilience == "Low"){pb.r.low = 0.05; pb.r.hi = 0.5; start.r =  c(0.05,0.5)}
    else if (pb.resilience == "Very low"){pb.r.low = 0.015; pb.r.hi = 0.1; start.r =  c(0.015,0.1)}}
  
  endb.hi<- endb.low <-intb.hi<- intb.low <-stb.hi<- stb.low <- NA
  
  startbio <- startbio_prior(stb.low, stb.hi, start.yr)
  int_params <- intbio_prior(intb.low, intb.hi, int.yr, start.yr, end.yr, startbio, yr, ct)
  intbio <- int_params[[1]]
  int.yr <- int_params[[2]]
  endbio <- endbio_prior(endb.low, endb.hi, nyr, ct.raw, ct)
  start.k <- k_prior(endbio, start.r, ct)


  ## produce data table
  test <- res[res$Stock == i & res$variable == "B_BMSY" 
              & res$Method != "SPiCT", ]
  
  
  test$Prior_L[test$Year == start.yr] <- startbio[1]*2
  test$Prior_U[test$Year == start.yr] <- startbio[2]*2
  test$Prior_L[test$Year == int.yr] <- intbio[1]*2
  test$Prior_U[test$Year == int.yr] <- intbio[2]*2
  test$Prior_L[test$Year == end.yr] <- endbio[1]*2
  test$Prior_U[test$Year == end.yr] <- endbio[2]*2
  
  Gate <- rbind(Gate, test)
  
  
}

# pdf(paste0("CMSY_Gates.pdf"), width = 8, height = 12)
# ggplot(Gate)+
#   geom_line(aes(x =Year, y = value, colour = Method))+
#   scale_color_manual(values=c("black", "#D55E00"))+
#   geom_point(x = Gate$Year, y = Gate$Prior_L)+
#   geom_point(x = Gate$Year, y = Gate$Prior_U)+
#   facet_wrap(facets = Gate$Stock, ncol = 4, scales = "free_y")+
#   theme(legend.title = element_blank(), 
#         legend.position = "top",
#         axis.title = element_blank())
# dev.off()


Gate$Zero <- 0 # use to make sure the y axis extends to 0

pdf(paste0("CMSY_Gates.pdf"), width =  6.69291, height = 12)
ggplot(Gate)+
  geom_line(aes(x =Year, y = value, colour = Method))+
  scale_color_manual(values=c("black", "#D55E00"))+
  geom_point(x = Gate$Year, y = Gate$Prior_L)+
  geom_point(x = Gate$Year, y = Gate$Prior_U)+
  geom_blank(aes(x = Gate$Year, y = Gate$Zero))+
  ylab("B/BMSY")+
  facet_wrap(facets = Gate$Stock, ncol = 4, scales = "free_y")+
  theme(legend.title = element_blank(), 
        legend.position = "top",
        axis.title.x = element_blank())
dev.off()

