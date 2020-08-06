library (ggplot2); theme_set(theme_bw())
library(gridExtra)
library(kableExtra)
source ("production_uncertainty_pb.r")

stocklist <- c("Cod6a", "Cod7ek", 
               "CodFaroe", "CodNS", 
               "Had6b","Had7bk" , "HadNS" , 
               "Ple7a", "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")

#### load SPICT results
spictfiles = list.files("SPICT_Results/Spict_n2r")
spictfileloc = "SPICT_Results/Spict_n2r/"
spictfiles = paste0(spictfileloc, spictfiles)
lapply(spictfiles, load, .GlobalEnv)

i = "Cod6a" #testing only

pred <- data.frame()
emp <- data.frame()
kbounds <- data.frame()

for (i in stocklist){
  
  spict <- get((paste0 (i, ".spict")), envir = .GlobalEnv) 
  test <- plot_production(spict, plot_it = FALSE)  
  
  tmpp <- test$pred
  tmpp$Stock <- i
  pred <- rbind (pred, tmpp)
  
  tmpe <- test$emp
  tmpe$Stock <- i
  emp <- rbind (emp, tmpe)
  
  tmpk <- data.frame(Stock = i, 
                     kest = test$Kbounds[1],
                     klower = test$Kbounds[2],
                     kupper = test$Kbounds[3])
  kbounds <- rbind(kbounds, tmpk)
}


#### one stock plot

# mystock = "Cod6a"
# 
# emp.df <- emp[emp$Stock == mystock,]
# pred.df <- pred[pred$Stock == mystock, ]
# kbounds.df <- kbounds[kbounds$Stock == mystock, ]
# 
# ylim <- range(emp.df$sp, na.rm = TRUE)
# # max sample is either 1.1 times max sp from index, or is k
# maxx <- ifelse(max(emp.df$Bt, na.rm = TRUE) < kbounds.df$kest, kbounds.df$kest, max(emp.df$Bt, na.rm = TRUE))
# xlim <- c(0, maxx)
# 
# pdf(paste0("Surplus_Production_Plots/", mystock, "_sp.pdf"), width = 6, height = 4)
# ggplot(pred.df )+
#   geom_ribbon(aes(x = B, ymin = Plwr, ymax = Pupr),
#               fill = "grey70")+
#   geom_line(aes(x = B, y = P))+
#   coord_cartesian(ylim = ylim, xlim =xlim)+
#   xlab ("Biomass")+
#   ylab ("Surplus production")+
#   geom_hline(yintercept = 0, linetype = "dashed")+
#   geom_vline(xintercept = c(kbounds.df$klower, (kbounds.df$kupper)), linetype = "dashed")+
#   geom_point(emp.df,  mapping = aes(x = Bt, y = sp,
#                                       colour = as.factor(index), shape = as.factor(index)))+
#   theme(legend.position = "none")
# dev.off()


###############################################
####     Facet plot style

# Doesn't work well

# ggplot(pred)+
#   geom_ribbon(aes(x = B, ymin = Plwr, ymax = Pupr), 
#               fill = "grey70")+
#   geom_line(aes(x = B, y = P))+
# #  coord_cartesian(ylim = range(emp$sp, na.rm = TRUE), 
# #                  xlim = c(0, 1.1 * ifelse(max(emp$Bt, na.rm = TRUE) < 
# #                                             kbounds$kest, kbounds$kest, max(emp$Bt, na.rm = TRUE))))+
# #  coord_cartesian(ylim = ylim)+
#   xlab ("Biomass")+
#   ylab ("Surplus production")+
#   geom_hline(yintercept = 0, linetype = "dashed")+
# #  geom_vline(xintercept = c(kbounds$klower, kbounds$kupper), linetype = "dashed")+
#   geom_point(emp,  mapping = aes(x = Bt, y = sp, 
#                                       colour = as.factor(index), shape = as.factor(index)))+
#   theme(legend.position = "none")+
#   facet_wrap(~ Stock, ncol = 4, scales = "free")


##################################
### Loop round

for (mystock in stocklist){

emp.df <- emp[emp$Stock == mystock,]
emp.df[,4:7] <- emp.df[,4:7]/1000
pred.df <- pred[pred$Stock == mystock, ]
pred.df[,1:4] <- pred.df[,1:4]/1000
kbounds.df <- kbounds[kbounds$Stock == mystock, ]
kbounds.df[,2:4] <- kbounds.df[,2:4]/1000

ylim <- range(emp.df$sp, na.rm = TRUE)
# max sample is either 1.1 times max sp from index, or is k
#maxx <- ifelse(max(emp.df$Bt, na.rm = TRUE) < kbounds.df$kest, kbounds.df$kest, max(emp.df$Bt, na.rm = TRUE))
maxx <- max(emp.df$Bt, na.rm = TRUE)
xlim <- c(0, maxx)

if(mystock == "HadNS"){
  sp.plot <- 
    ggplot(pred.df )+
    geom_ribbon(aes(x = B, ymin = Plwr, ymax = Pupr),
                fill = "grey70")+
    geom_line(aes(x = B, y = P))+
    coord_cartesian(ylim = ylim, xlim =xlim)+
    facet_wrap(~Stock )+
    scale_x_continuous(breaks = c(0, signif(xlim, 1)/2, signif(xlim, 1))) +
    geom_hline(yintercept = 0, linetype = "dashed")+
    geom_vline(xintercept = c(kbounds.df$klower, (kbounds.df$kupper)), linetype = "dashed")+
    geom_point(emp.df,  mapping = aes(x = Bt, y = sp, shape = as.factor(index), colour = as.factor(index)))+
    theme(legend.position = "none", axis.title = element_blank(), 
          axis.text.y = element_text(angle = 90, hjust = 0.5, size = 8), axis.text.x = element_text(size = 6))
}else{
sp.plot <- ggplot(pred.df )+
  geom_ribbon(aes(x = B, ymin = Plwr, ymax = Pupr),
              fill = "grey70")+
  geom_line(aes(x = B, y = P))+
  coord_cartesian(ylim = ylim, xlim =xlim)+
  facet_wrap(~Stock )+
  scale_x_continuous(breaks = c(0, signif(xlim, 1)/2, signif(xlim, 1))) +
  geom_hline(yintercept = 0, linetype = "dashed")+
  geom_vline(xintercept = c(kbounds.df$klower, (kbounds.df$kupper)), linetype = "dashed")+
  geom_point(emp.df,  mapping = aes(x = Bt, y = sp, shape = as.factor(index), colour = as.factor(index)))+
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 8), axis.text.x = element_text(size = 6))
}

pdf(paste0("Surplus_Production_Plots/", mystock, "_sp.pdf"), width = 6, height = 4)
print(sp.plot)
dev.off()

assign(paste0(mystock, ".plot"), sp.plot)
}

#######################################
###     Combine into one plot

# pdf("Surplus_Production_Plots/All_Stocks_SurplusProduction_empirical lim.pdf", width = 6.69291, height = 12)
# grid.arrange(Cod6a.plot, Cod7ek.plot, CodFaroe.plot, CodNS.plot, Had6b.plot,
#              Had7bk.plot, HadNS.plot, Ple7a.plot, Ple7hk.plot, Sol7a.plot, Sol7fg.plot,
#              Sol7hk.plot, Sol2024.plot, Whg6a.plot, Whg7a.plot, Whg7bk.plot, WhgNS.plot,  ncol = 4)
# dev.off()

pdf("Surplus_Production_Plots/All_Stocks_SurplusProduction_colour.pdf", width =  6.69291, height = 12)
grid.arrange(arrangeGrob (Cod6a.plot, Cod7ek.plot, CodFaroe.plot, CodNS.plot, Had6b.plot,
             Had7bk.plot, HadNS.plot, Ple7a.plot, Ple7hk.plot, Sol7a.plot, Sol7fg.plot,
             Sol7hk.plot, Sol2024.plot, Whg6a.plot, Whg7a.plot, Whg7bk.plot, WhgNS.plot,  ncol = 4,
             bottom= grid::textGrob("Biomass (000 tonnes)", gp=grid::gpar(fontsize=10)),
             left = grid::textGrob("Surplus Production (000 tonnes)", gp=grid::gpar(fontsize=10), rot = 90)))
dev.off()


# kbounds2 <- kbounds
# kbounds2[,2:4] <- round(kbounds2[,2:4],0)
# colnames(kbounds2) <- c("Stock", "k", "k lower", "k upper")
# 
# k_all = tableGrob(kbounds2, rows = NULL)
# 
# pdf("Surplus_Production_Plots/K_Bounds.pdf", width = 8, height = 8)
# grid.arrange(k_all, as.table=TRUE)
# dev.off()

# k1 <- tableGrob(kbounds2[1:6,], rows = NULL)
# k2 <- tableGrob(kbounds2[7:12,], rows = NULL)
# k3 <- tableGrob(kbounds2[13:17,], rows = NULL)
# 
# pdf("Surplus_Production_Plots/All_Stocks_SurplusProduction_k1.pdf", width = 8, height = 12)
# grid.arrange(Cod6a.plot, Cod7ek.plot, CodFaroe.plot, CodNS.plot, Had6b.plot,
#              Had7bk.plot, HadNS.plot, Ple7a.plot, Ple7hk.plot, Sol7a.plot, Sol7fg.plot,
#              Sol7hk.plot, Sol2024.plot, Whg6a.plot, Whg7a.plot, Whg7bk.plot, WhgNS.plot, k1, k2, k3,  ncol = 4,
#              as.table=TRUE)
# dev.off()
# 
# library(cowplot)
# pdf("Surplus_Production_Plots/All_Stocks_SurplusProduction_k2.pdf", width = 8, height = 12)
# plot_grid(Cod6a.plot, Cod7ek.plot, CodFaroe.plot, CodNS.plot, Had6b.plot,
#           Had7bk.plot, HadNS.plot, Ple7a.plot, Ple7hk.plot, Sol7a.plot, Sol7fg.plot,
#           Sol7hk.plot, Sol2024.plot, Whg6a.plot, Whg7a.plot, Whg7bk.plot, WhgNS.plot, k1, k2, k3, ncol = 4)
# dev.off()

# library (sjPlot)
# tab_df(kbounds2,
#        file="kbounds_test.doc") 

# write.csv(kbounds2, "SPICT K bounds.csv")


ggplot(pred.df )+
  geom_ribbon(aes(x = B, ymin = Plwr, ymax = Pupr),
              fill = "grey70")+
  geom_line(aes(x = B, y = P))+
  coord_cartesian(ylim = ylim, xlim =xlim)+
  facet_wrap(~Stock )+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_x_continuous(breaks = c(0, signif(xlim, 1)/2, signif(xlim, 1))) +
  geom_vline(xintercept = c(kbounds.df$klower, (kbounds.df$kupper)), linetype = "dashed")+
  geom_point(emp.df,  mapping = aes(x = Bt, y = sp, shape = as.factor(index), colour = as.factor(index)))+
  theme(legend.position = "none", axis.title = element_blank(), 
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 8), axis.text.x = element_text(size = 6))

round(xlim, -3)
signif(xlim, 1)
