###########################################
#### Jacobian comparison plots in a loop
###########################################

library (reshape)
library (ggplot2); theme_set(theme_bw())
library (gridExtra)
library (egg)
library(plyr)
source ("jacobian prior functions.r")
source ("jacobian plot functions.r")


stocklist <- c("Cod6a", "Cod7ek", 
               "CodFaroe", "CodNS", 
               "Had6b","Had7bk" , "HadNS" , 
               "Ple7a", "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")

Res_all <- data.frame()

for (i in stocklist){

load(paste0("Jacobian/Results/", i, "_Jacobian_Repeats.RData"))

#################################################
### Jacobian plots

prior.names = c("rl", "ru", "kl", "ku", "p0l", "p0u", "pMl", "pMu", "pTl", "pTu")

Res <- jacobian.plot.prep(J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, prior.names)

Res$Stock <- i

Res_all <- rbind(Res_all, Res)

}

######
## To convert the jacobian output
# changed the proportional_influence code to make this step no longer necessary
if (revised){
Res_all$value <- Res_all$value/5
}

# pdf("Jacobian/Final/multiJacobian1.pdf", width = 8, height = 12)
# multijacob1 <-  grid.arrange(Cod6a.jplot, Cod7a.jplot, Cod7ek.jplot,
#              CodFaroe.jplot, CodNS.jplot, Had6b.jplot, ncol = 1)
# dev.off()
# 
# pdf("Jacobian/Final/multiJacobian2.pdf", width = 8, height = 12)
# multijacob2 <-  grid.arrange(Had7bk.jplot, HadNS.jplot, Ple7a.jplot,
#                              Ple7hk.jplot, Sol7a.jplot, Sol7fg.jplot, ncol = 1)
# dev.off()
# 
# pdf("Jacobian/Final/multiJacobian3.pdf", width = 8, height = 12)
# multijacob3 <-  grid.arrange(Sol7hk.jplot,  Sol2024.jplot, Whg6a.jplot, 
#                              Whg7a.jplot, Whg7bk.jplot, WhgNS.jplot, ncol = 1)
# dev.off()

pdf("Jacobian/Final/Jacobian1.pdf", width =  8, height = 12)
ggplot(Res_all[Res_all$Stock %in% stocklist[1:6], ])+
  geom_point(aes(x = X2, y = value))+
  facet_grid(vars(Stock), vars(X1), scales = "free")+
  xlab ("Priors")+
  ylab("Difference in B/BMSY")+
    geom_hline(yintercept = 0, colour = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0.2, "lines"))
dev.off()

pdf("Jacobian/Final/Jacobian2.pdf", width =  8, height = 12)
ggplot(Res_all[Res_all$Stock %in% stocklist[7:12], ])+
  geom_point(aes(x = X2, y = value))+
  facet_grid(vars(Stock), vars(X1), scales = "free")+
  xlab ("Priors")+
  ylab("Difference in B/BMSY")+
  geom_hline(yintercept = 0, colour = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0.2, "lines"))
dev.off()

pdf("Jacobian/Final/Jacobian3.pdf", width =  8, height = 12)
ggplot(Res_all[Res_all$Stock %in% stocklist[13:17], ])+
  geom_point(aes(x = X2, y = value))+
  facet_grid(vars(Stock), vars(X1), scales = "free")+
  xlab ("Priors")+
  ylab("Difference in B/BMSY")+
  geom_hline(yintercept = 0, colour = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
        panel.spacing.x=unit(0, "lines"),
        panel.spacing.y=unit(0.2, "lines"))
dev.off()


###### create a median plot for all stocks

Res2 <- ddply(Res_all, .(Stock, X1, X2), summarise,
              Median = median(value))

pdf("Jacobian/Final/Jacobian_Median.pdf", width =  6.69291, height = 4)
ggplot(Res2)+
  geom_point(aes(x = X2, y = Median))+
  facet_wrap(facets = "X1")+
  xlab ("Priors")+
  ylab("Median Difference in B/BMSY")+
  geom_hline(yintercept = 0, colour = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2),
        panel.spacing.x=unit(0, "lines"))
dev.off()

#### paper stats for paper
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
