#### Jacobian plots for CMSY2 ####
library (ggplot2)

load("Jacobian/CMSY2/CMSY2_Sensitivity_all_stocks.RData")

stocklist <- c("Cod6a", "Cod7ek", 
               "CodFaroe", "CodNS", 
               "Had6b","Had7bk" , "HadNS" , 
               "Ple7a", "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")

Res_all <- melt (all_stocks_res, id.vars = c("Rep", "Stock", "trial"))

prior.names = c("rlow", "rhi", "stblow", "stbhi", "intblow", "intbhi", "endblow", "endbhi")

Res_all$Rep = ordered(Res_all$Rep, levels = prior.names)
#Res$X1 = ordered(Res$X1, levels = c("Start", "Mid", "End"))


#### Plots ####

# pdf("Jacobian/CMSY2_Plots/Jacobian1.pdf", width =  8, height = 12)
# ggplot(Res_all[Res_all$Stock %in% stocklist[1:6], ])+
#   geom_point(aes(x = Rep, y = value))+
#   facet_grid(vars(Stock), vars(variable), scales = "free")+
#   xlab ("Priors")+
#   ylab("Difference in B/BMSY")+
#   geom_hline(yintercept = 0, colour = "red")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
#         panel.spacing.x=unit(0, "lines"),
#         panel.spacing.y=unit(0.2, "lines"))
# dev.off()
# 
# pdf("Jacobian/CMSY2_Plots/Jacobian2.pdf", width =  8, height = 12)
# ggplot(Res_all[Res_all$Stock %in% stocklist[7:12], ])+
#   geom_point(aes(x = Rep, y = value))+
#   facet_grid(vars(Stock), vars(variable), scales = "free")+
#   xlab ("Priors")+
#   ylab("Difference in B/BMSY")+
#   geom_hline(yintercept = 0, colour = "red")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
#         panel.spacing.x=unit(0, "lines"),
#         panel.spacing.y=unit(0.2, "lines"))
# dev.off()
# 
# pdf("Jacobian/CMSY2_Plots/Jacobian3.pdf", width =  8, height = 12)
# ggplot(Res_all[Res_all$Stock %in% stocklist[13:17], ])+
#   geom_point(aes(x = Rep, y = value))+
#   facet_grid(vars(Stock), vars(variable), scales = "free")+
#   xlab ("Priors")+
#   ylab("Difference in B/BMSY")+
#   geom_hline(yintercept = 0, colour = "red")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
#         panel.spacing.x=unit(0, "lines"),
#         panel.spacing.y=unit(0.2, "lines"))
# dev.off()



###### create a median plot for all stocks

# Res2 <- ddply(Res_all, .(Stock, Rep, variable), summarise,
#               Median = median(value))
# 
# pdf("Jacobian/CMSY2_Plots/Jacobian_Median.pdf", width =  6.69291, height = 4)
# ggplot(Res2)+
#   geom_point(aes(x = Rep, y = Median))+
#   facet_wrap(facets = "variable")+
#   xlab ("Priors")+
#   ylab("Median Difference in B/BMSY")+
#   geom_hline(yintercept = 0, colour = "red")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
#         panel.spacing.x=unit(0, "lines"))
# dev.off()


##################################################################
#### Box plot trials ####
##################################################################

Res2 <- ddply(Res_all, .(Stock, Rep, variable), summarise,
              Median = median(value))


Res2$prior [Res2$Rep == "rlow"] <- "r"
Res2$prior [Res2$Rep == "rhi"] <- "r"
Res2$prior [Res2$Rep == "stblow"] <- "start depletion"
Res2$prior [Res2$Rep == "stbhi"] <- "start depletion"
Res2$prior [Res2$Rep == "intblow"] <- "int depletion"
Res2$prior [Res2$Rep == "intbhi"] <- "int depletion"
Res2$prior [Res2$Rep == "endblow"] <- "end depletion"
Res2$prior [Res2$Rep == "endbhi"] <- "end depletion"

Res2$limit [Res2$Rep == "rlow"] <- "lower"
Res2$limit [Res2$Rep == "rhi"] <- "upper"
Res2$limit [Res2$Rep == "stblow"] <- "lower"
Res2$limit [Res2$Rep == "stbhi"] <- "upper"
Res2$limit [Res2$Rep == "intblow"] <- "lower"
Res2$limit [Res2$Rep == "intbhi"] <- "upper"
Res2$limit [Res2$Rep == "endblow"] <- "lower"
Res2$limit [Res2$Rep == "endbhi"] <- "upper"

prior.names2 = c("r", "start depletion", "int depletion", "end depletion")

Res2$prior = ordered(Res2$prior, levels = prior.names2)


levels(Res2$variable) <-c('CMSY_Start',  'CMSY_Mid', 'CMSY_End') 

pdf("Jacobian/CMSY2_Plots/Jacobian_Median_box.pdf", width =  6.69291, height = 4)
ggplot(Res2, aes(x=prior, y=Median, colour = limit)) + 
  geom_boxplot(size = 0.4)+
  scale_color_manual(values=c( "#D55E00", "#56B4E9"))+
  facet_wrap(facets = "variable", labeller = label_parsed)+
  xlab ("Priors")+
  ylab(expression(Median~Difference~of~B/B[MSY]))+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6),
        panel.spacing.x=unit(0, "lines"),
        legend.title = element_blank())
dev.off()


#### SPiCt Jacobian ####

load("Jacobian/Spict_Jacobian_results.RDat")

r_orig <- all_res[all_res$Rep == "r",]
r_low <- all_res[all_res$Rep == "r_low",]
r_hi <- all_res[all_res$Rep == "r_hi",]

r_low$SPiCT_Start <- r_low$B_BMSY_start - r_orig$B_BMSY_start
r_low$SPiCT_Mid <- r_low$B_BMSY_mid - r_orig$B_BMSY_mid
r_low$SPiCT_End <- r_low$B_BMSY_end - r_orig$B_BMSY_end

r_hi$SPiCT_Start <- r_hi$B_BMSY_start - r_orig$B_BMSY_start
r_hi$SPiCT_Mid <- r_hi$B_BMSY_mid - r_orig$B_BMSY_mid
r_hi$SPiCT_End <- r_hi$B_BMSY_end - r_orig$B_BMSY_end

spictj <- rbind (r_low, r_hi)
spictj <- spictj[, c(1, 6:8)]

spictj <- melt(spictj, id.vars = "Rep")

spictj$prior [spictj$Rep == "r_low"] <- "r"
spictj$prior [spictj$Rep == "r_hi"] <- "r"

spictj$limit [spictj$Rep == "r_low"] <- "lower"
spictj$limit [spictj$Rep == "r_hi"] <- "upper"

# # pdf("Jacobian/CMSY2_Plots/Jacobian_spict_box.pdf", width =  6.69291, height = 4)
# ggplot(spictj, aes(x=prior, y=value, colour = limit)) +
#   geom_boxplot(size = 0.4)+
#   scale_color_manual(values=c( "#D55E00", "#56B4E9"))+
#   facet_wrap(facets = "variable", labeller = label_parsed)+
#   xlab ("Priors")+
#   ylab(expression(Median~Difference~of~B/B[MSY]))+
#   geom_hline(yintercept = 0, linetype = "dotted") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.6),
#         panel.spacing.x=unit(0, "lines"),
#         legend.title = element_blank())
# #dev.off()
# 


###### combine cmsy and spict
Res2$Stock <- NULL
colnames (Res2)[3] <- "value"
Res3 <- rbind(Res2, spictj)

library(tidyr)

Res4 <- Res3 %>% separate(variable, c("Method", "time"))
time_order <- c("Start", "Mid", "End")

Res4$time = ordered(Res4$time, levels = time_order)

pdf("Jacobian/CMSY2_Plots/Jacobian_cmsy2_spict_box.pdf", width =  6.69291, height = 4)
ggplot(Res4, aes(x=prior, y=value, colour = limit)) + 
  geom_boxplot(size = 0.4)+
  facet_grid(~Method + time, scales = "free_x", space = "free")+
  scale_color_manual(values=c( "#D55E00", "#56B4E9"))+
  xlab ("Priors")+
  ylab(expression(Median~Difference~of~B/B[MSY]))+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6),
        panel.spacing.x=unit(0, "lines"),
        legend.title = element_blank(),
        legend.position=c(.4,.8))
dev.off()

#############################################################
####   Plots by stock for supplemental in boxplot format ####
#############################################################


Res_all$prior [Res_all$Rep == "rlow"] <- "r"
Res_all$prior [Res_all$Rep == "rhi"] <- "r"
Res_all$prior [Res_all$Rep == "stblow"] <- "start depletion"
Res_all$prior [Res_all$Rep == "stbhi"] <- "start depletion"
Res_all$prior [Res_all$Rep == "intblow"] <- "int depletion"
Res_all$prior [Res_all$Rep == "intbhi"] <- "int depletion"
Res_all$prior [Res_all$Rep == "endblow"] <- "end depletion"
Res_all$prior [Res_all$Rep == "endbhi"] <- "end depletion"

Res_all$limit [Res_all$Rep == "rlow"] <- "lower"
Res_all$limit [Res_all$Rep == "rhi"] <- "upper"
Res_all$limit [Res_all$Rep == "stblow"] <- "lower"
Res_all$limit [Res_all$Rep == "stbhi"] <- "upper"
Res_all$limit [Res_all$Rep == "intblow"] <- "lower"
Res_all$limit [Res_all$Rep == "intbhi"] <- "upper"
Res_all$limit [Res_all$Rep == "endblow"] <- "lower"
Res_all$limit [Res_all$Rep == "endbhi"] <- "upper"

prior.names2 = c("r", "start depletion", "int depletion", "end depletion")

Res_all$prior = ordered(Res_all$prior, levels = prior.names2)

Res_all$variable <- as.character(Res_all$variable)
Res_all$variable[Res_all$variable == "B_BMSY_start"] <- "Start" 
Res_all$variable[Res_all$variable == "B_BMSY_mid"] <- "Mid" 
Res_all$variable[Res_all$variable == "B_BMSY_end"] <- "End" 
Res_all$variable <- as.factor(Res_all$variable)

Res_all$variable = ordered(Res_all$variable, levels = time_order)

pdf("Jacobian/CMSY2_Plots/Jacobian_box1.pdf", width =  6.69291, height = 12)
ggplot(Res_all[Res_all$Stock %in% stocklist[1:6], ], aes(x = prior, y = value, colour = limit))+
  geom_boxplot(size = 0.4)+
  facet_grid(vars(Stock), vars(variable), scales = "free")+
  xlab ("Priors")+
  ylab(expression(Median~Difference~of~B/B[MSY]))+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6),
        panel.spacing.x=unit(0, "lines"),
        legend.title = element_blank(),
        legend.position = "bottom")  
dev.off()

pdf("Jacobian/CMSY2_Plots/Jacobian_box2.pdf", width =  6.69291, height = 12)
ggplot(Res_all[Res_all$Stock %in% stocklist[7:12], ], aes(x = prior, y = value, colour = limit))+
  geom_boxplot(size = 0.4)+
  facet_grid(vars(Stock), vars(variable), scales = "free")+
  xlab ("Priors")+
  ylab(expression(Median~Difference~of~B/B[MSY]))+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6),
        panel.spacing.x=unit(0, "lines"),
        legend.title = element_blank(),
        legend.position = "bottom")  
dev.off()

pdf("Jacobian/CMSY2_Plots/Jacobian_box3.pdf", width =  6.69291, height = 12)
ggplot(Res_all[Res_all$Stock %in% stocklist[13:17], ], aes(x = prior, y = value, colour = limit))+
  geom_boxplot(size = 0.4)+
  facet_grid(vars(Stock), vars(variable), scales = "free")+
  xlab ("Priors")+
  ylab(expression(Median~Difference~of~B/B[MSY]))+
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6),
        panel.spacing.x=unit(0, "lines"),
        legend.title = element_blank(),
        legend.position = "bottom")  
dev.off()

#################################################
#### stats for paper ####
#################################################

p0u <- Res2[Res2$X2 == "p0u", ]
p0u_start <- p0u[p0u$X1 == "Start", ]
median (p0u_start$Median)

p0u_mid <- p0u[p0u$X1 == "Mid", ]
median (p0u_mid$Median)

pTu <- Res2[Res2$X2 == "pTu" & Res2$X1 == "End", ]
median (pTu$Median)

# what about not reduce data set
p0u <- Res_all[Res_all$X2 == "p0u", ]
p0u_start <- p0u[p0u$X1 == "Start", ]
median (p0u_start$value)

p0u_mid <- p0u[p0u$X1 == "Mid", ]
median (p0u_mid$value)

pTu <- Res_all[Res_all$X2 == "pTu" & Res_all$X1 == "End", ]
median (pTu$value)


Res3 <- ddply(Res2, .(X1, X2), summarise,
              Median = median(Median))

Res4 <- dcast(Res3, X2 ~ X1)
write.csv(Res4, "Median Jacobian test.csv")

