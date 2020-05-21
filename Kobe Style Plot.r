library(reshape2)
library (ggplot2); theme_set(theme_bw())
library (ggrepel)

load("Data/Assessment Time Series Results.RData")
res <- res[res$Stock != "Cod7a", ]

test <- res[res$Year == 2017 & res$Stock !="Whg7a",]
test2 <- res[res$Year == 2015 & res$Stock =="Whg7a" ,]
test <- rbind (test, test2)
test_ICES <- test[test$Method== "ICES",]
test <- test[test$Method != "ICES",]

test3 <- merge(test, test_ICES, by = c("Stock", "Year", "variable"))
colnames(test3) <- c("Stock", "Year", "variable", "Method", "Est.Value", "ICES", "ICES.Value")

#######################################################
### Try a similar plot but relative to ICES rather than the refpoints

test3$Diff = test3$Est.Value - test3$ICES.Value

test4 <- dcast(test3, Stock + Year +  Method ~ variable)

pdf("Kobe plot Est-ICES.pdf", width =  6.69291)
ggplot (test4, aes(x= B_BMSY, y = F_FMSY, label = test4$Stock, colour = Method))+
  scale_color_manual(values=c("#D55E00", "#56B4E9"))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  # xlim (c(-6.6, 6.6))+
  # ylim (c(-4.4, 4.4))+
  xlab("Assessed B/BMSY - ICES B/BMSY")+
  ylab("Assessed F/FMSY - ICES F/FMSY")+
  geom_label_repel (aes(x= B_BMSY, y = F_FMSY, label = test4$Stock))+
  theme(legend.position = c(0.9,0.9), legend.title = element_blank())
dev.off()

# ff= test3[test3$variable == "F_FMSY", ]

# ggplot (ff, aes(x= Est.Value, y = ICES.Value, label = ff$Stock, colour = Method))+
#   geom_text()+
#   geom_hline(yintercept = 1)+
#   geom_vline(xintercept = 1)+
#   annotate("text", x = 3, y = 3, label = "Agreed Overfishing")+
#   annotate("text", x = 0, y = 4, label = "Not 
#            recognising 
#            overfishing")+
#   annotate("text", x = 0, y = 0.5, label = "Agreed
#            Sustainable
#            Fishing")+
#   annotate("text", x = 3, y = 0.5, label = "Not recognising underfishing")
# ggsave ("F_FMSY ICES Agreement.pdf")
# 
# #####################################################
# bb= test3[test3$variable == "B_BMSY", ]
# 
# ggplot (bb, aes(x= Est.Value, y = ICES.Value, label = bb$Stock, colour = Method))+
#   geom_text()+
#   geom_hline(yintercept = 1)+
#   geom_vline(xintercept = 1)+
#   annotate("text", x = 2, y = 5, label = "Agreed healthy biomass")+
#   annotate("text", x = 0.5, y = 5, label = "Not identifying a 
#            healthy biomass")+
#   annotate("text", x = 0.5, y = 0.5, label = "Agreed
#            low biomass")+
#   annotate("text", x = 2, y = 0.5, label = "Not recognising low biomass")
# ggsave ("B_BMSY ICES Agreement.pdf")
