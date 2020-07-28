library(kobe)
library (ggplot2)
library (reshape2)
library (ggrepel)

load ("Data/Assessment Time Series Results.RData")

res$Stock <- factor(res$Stock, levels = c("Cod6a", "Cod7ek", 
                                          "CodFaroe", "CodNS", 
                                          "Had6b","Had7bk" , "HadNS" , 
                                          "Ple7a", "Ple7hk" , "Sol7a", 
                                          "Sol7fg" , "Sol7hk" , "Sol2024",
                                          "Whg6a", "Whg7a" , 
                                          "Whg7bk" , "WhgNS"))

test <- res[res$Stock== "Cod7ek" , ]
test2 <- dcast(test, Year + Method ~  variable, value.var = "value")



spict_year <- test2[test2$Method == "SPiCT", ]
spict_year <- spict_year[spict_year$Year %in% c(min(spict_year$Year), 2017) ,]
cmsy_year <- test2[test2$Method == "CMSY", ]
cmsy_year <- cmsy_year[cmsy_year$Year %in% c(min(cmsy_year$Year), 2017) ,]
ICES_year <- test2[test2$Method == "ICES", ]
ICES_year <- ICES_year[ICES_year$Year %in% c(min(ICES_year$Year), 2017) ,]


label_years <- rbind(spict_year, cmsy_year, ICES_year)


kobePhase(test2, xlim = c(0, max(test2$B_BMSY)), ylim = c(0, max(test2$F_FMSY)))+
  geom_point(aes(B_BMSY, F_FMSY))+
  geom_path(aes(B_BMSY, F_FMSY))+
  facet_grid(~Method)+
  geom_text_repel (data = label_years, aes(x= B_BMSY, y = F_FMSY, label = Year))

##############################
#### all data ####
stocklist <- c("Cod6a", "Cod7ek", 
               "CodFaroe", "CodNS", 
               "Had6b","Had7bk" , "HadNS" , 
               "Ple7a", "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")

res_wide <- dcast(res, Stock + Year + Method ~  variable, value.var = "value")

res1 <- res_wide[res_wide$Stock %in% stocklist[1:6], ]

pdf("Kobe Plots/kobeplots1.pdf", width =  6.69291, height = 12)
kobePhase(res1, xlim = c(0, 3), ylim = c(0, max(res1$F_FMSY)))+
  geom_point(aes(B_BMSY, F_FMSY))+
  geom_path(aes(B_BMSY, F_FMSY))+
  facet_grid(vars(Stock), vars(Method), scales = "free")+
  geom_text_repel (data = res1[res1$Year == 2017, ], aes(x= B_BMSY, y = F_FMSY, label = Year))
dev.off()

res2 <- res_wide[res_wide$Stock %in% stocklist[7:12], ]

pdf("Kobe Plots/kobeplots2.pdf", width =  6.69291, height = 12)
kobePhase(res2, xlim = c(0, 3), ylim = c(0, max(res2$F_FMSY)))+
  geom_point(aes(B_BMSY, F_FMSY))+
  geom_path(aes(B_BMSY, F_FMSY))+
  facet_grid(vars(Stock), vars(Method), scales = "free")+
  geom_text_repel (data = res2[res2$Year == 2017, ], aes(x= B_BMSY, y = F_FMSY, label = Year))
dev.off()

res3 <- res_wide[res_wide$Stock %in% stocklist[13:17], ]

pdf("Kobe Plots/kobeplots3.pdf", width =  6.69291, height = 12)
kobePhase(res3, xlim = c(0, 3), ylim = c(0, max(res3$F_FMSY)))+
  geom_point(aes(B_BMSY, F_FMSY))+
  geom_path(aes(B_BMSY, F_FMSY))+
  facet_grid(vars(Stock), vars(Method), scales = "free")+
  geom_text_repel (data = res3[res3$Year == 2017, ], aes(x= B_BMSY, y = F_FMSY, label = Year))
dev.off()