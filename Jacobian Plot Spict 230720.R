library (reshape2)

load("Jacobian/Spict_Jacobian_results.RDat")

r_orig <- all_res[all_res$Rep == "r",]
r_low <- all_res[all_res$Rep == "r_low",]
r_hi <- all_res[all_res$Rep == "r_hi",]

r_low$SPiCT_start <- r_low$B_BMSY_start - r_orig$B_BMSY_start
r_low$SPiCT_mid <- r_low$B_BMSY_mid - r_orig$B_BMSY_mid
r_low$SPiCT_end <- r_low$B_BMSY_end - r_orig$B_BMSY_end

r_hi$SPiCT_start <- r_hi$B_BMSY_start - r_orig$B_BMSY_start
r_hi$SPiCT_mid <- r_hi$B_BMSY_mid - r_orig$B_BMSY_mid
r_hi$SPiCT_end <- r_hi$B_BMSY_end - r_orig$B_BMSY_end

Res2 <- rbind (r_low, r_hi)
Res3 <- Res2[, c(1, 6:8)]

Res3 <- melt(Res3, id.vars = "Rep")

Res3$prior [Res3$Rep == "r_low"] <- "r"
Res3$prior [Res3$Rep == "r_hi"] <- "r"

Res3$limit [Res3$Rep == "r_low"] <- "lower"
Res3$limit [Res3$Rep == "r_hi"] <- "upper"

pdf("Jacobian/CMSY2_Plots/Jacobian_spict_box.pdf", width =  6.69291, height = 4)
ggplot(Res3, aes(x=prior, y=value, colour = limit)) +
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
