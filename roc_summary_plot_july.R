##---------------------
## Plot the ROC points
## CM, PB: 25/02/2019
## 
##---------------------
FFmsy_dat <- data.frame(model = c("SPICT", "CMSY"),
                        TPR = c(151/418, 384/418),
                        FPR = c(11/56, 43/56))
FFmsy_dat$distance <- with(FFmsy_dat, sqrt((TPR - 1)^2 + (FPR - 0)^2))

BBmsy_dat <- data.frame(model = c("SPICT", "CMSY"),
                        TPR = c(185/423, 409/423),
                        FPR = c(4/51, 33/51))
BBmsy_dat$distance <- with(BBmsy_dat, sqrt((TPR - 1)^2 + (FPR - 0)^2))

# ##
# with(FFmsy_dat, plot(FFmsy_FPR, FFmsy_TPR, pch = c(15, 19), xlim = c(0, 1), ylim = c(0, 1), bty = "l", col = "darkgrey"))
# abline(c(0, 1), col = 1, lty = 3)
# with(FFmsy_dat, segments(rep(0, 2), rep(1, 2), FFmsy_FPR, FFmsy_TPR, lty = 2, col = "darkgrey"))
# ##
# with(BBmsy_dat, points(BBmsy_FPR, BBmsy_TPR, pch = c(15, 19)))
# with(BBmsy_dat, segments(rep(0, 2), rep(1, 2), BBmsy_FPR, BBmsy_TPR, lty = 2))
# legend("bottomright",
#        legend = c("FFmsy SPiCT", "FFmsy CMSY",
#                   "BBmsy SPiCT", "BBmsy CMSY"),
#        col = c(rep("darkgrey", 2), rep(1, 2)),
#        pch = c(15, 19, 15, 19),
#        lty = 2, bty = "n")
# text((FFmsy_dat$FFmsy_FPR) / 2,
# (1 + FFmsy_dat$FFmsy_TPR) / 2,
# labels = round(FFmsy_dat$distance, 2),
# col = "darkgrey"
# )
# 
# text((BBmsy_dat$BBmsy_FPR) / 2,
# (1 + BBmsy_dat$BBmsy_TPR) / 2,
# labels = round(BBmsy_dat$distance, 2),
# col = 1
# )

##############################################
library (ggplot2); theme_set(theme_bw())

BBmsy_dat$variable <- "B/BMSY"
FFmsy_dat$variable <- "F/FMSY"

dat <- rbind (BBmsy_dat, FFmsy_dat)
dat$distance = round(dat$distance, 2)

labs1 <- c(expression(B/B[MSY]), expression(F/F[MSY]))

pdf("RO Plot July.pdf", width = 6.69, height = 6.69)
ggplot(dat)+
  geom_abline(intercept = 0, slope = 1, linetype = "dotted")+
  geom_segment(data = dat, aes(x = FPR, y = TPR, xend = 0, yend = 1), linetype = "dashed")+
  geom_point(aes(x = FPR, y = TPR, colour = model, shape = variable),  size = 3)+
  scale_color_manual(values=c("#D55E00", "#56B4E9"))+
  scale_shape_discrete(labels = labs1) +
  xlim (c(0,1))+ ylim (c(0,1))+
  xlab("False Positive Rate")+
  ylab("True Positive Rate")+
  theme (legend.title = element_blank(), legend.position = c(0.8, 0.4))+
  annotate("text", label = dat$distance, x= dat$FPR + 0.05, y= dat$TPR)
dev.off()
