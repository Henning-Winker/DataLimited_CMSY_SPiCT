library(reshape2)
library (kableExtra)
library (ggplot2); theme_set(theme_bw())
library (ggrepel)

load("Data/Assessment Time Series Results.RData")
res <- res[res$Stock != "Cod7a", ]

##########################################################
full<- dcast(res, Stock + Year + variable ~ Method)

full <- full[full$Stock != "Cod7a", ]
full <- full[complete.cases(full),]

full$CMSY.MAPE <- abs((full$CMSY- full$ICES)/full$ICES)
full$SPICT.MAPE <- abs((full$SPiCT- full$ICES)/full$ICES)

MAPE <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c("Stock", "CMSY.B_BMSY", "CMSY.F_FMSY", "SPiCT.B_BMSY", "SPiCT.F_FMSY",
       "CMSY.B_BMSY.Corr", "CMSY.F_FMSY.Corr", "SPiCT.B_BMSY.Corr", "SPiCT.F_FMSY.Corr")
colnames(MAPE) <- x

for (i in unique(full$Stock)){

  temp <- data.frame(matrix(ncol = 9, nrow = 1))
  x <- c("Stock", "CMSY.B_BMSY.MAPE", "CMSY.F_FMSY.MAPE", "SPiCT.B_BMSY.MAPE", "SPiCT.F_FMSY.MAPE",
         "CMSY.B_BMSY.Corr", "CMSY.F_FMSY.Corr", "SPiCT.B_BMSY.Corr", "SPiCT.F_FMSY.Corr")
  colnames(temp) <- x  
  
  temp$Stock <- i
  
  bbmsy <-  full[full$variable == "B_BMSY" & full$Stock == i,]
  ffmsy <-  full[full$variable == "F_FMSY" & full$Stock == i,]
  
  temp$CMSY.B_BMSY.MAPE <-  median(bbmsy$CMSY.MAPE, na.rm = T)
  temp$CMSY.F_FMSY.MAPE <-  median(ffmsy$CMSY.MAPE, na.rm = T)
  
  temp$SPiCT.B_BMSY.MAPE <-  median(bbmsy$SPICT.MAPE, na.rm = T)
  temp$SPiCT.F_FMSY.MAPE <-  median(ffmsy$SPICT.MAPE, na.rm = T)

  temp$CMSY.B_BMSY.Corr <- cor(bbmsy$ICES, bbmsy$CMSY, method = "spearman", use = "complete")
  temp$CMSY.F_FMSY.Corr <- cor(ffmsy$ICES, ffmsy$CMSY, method = "spearman", use = "complete")

  temp$SPiCT.B_BMSY.Corr <- cor(bbmsy$ICES, bbmsy$SPiCT, method = "spearman", use = "complete")
  temp$SPiCT.F_FMSY.Corr <- cor(ffmsy$ICES, ffmsy$SPiCT, method = "spearman", use = "complete")


MAPE = rbind(MAPE, temp)
}

summary (MAPE)

########  add a summary row
#MAPE[18,] <- sapply(MAPE, median)   
MAPE[18,1] <- "Median"
MAPE[18,2] <- median (MAPE[,2], na.rm = T)
MAPE[18,3] <- median (MAPE[,3], na.rm = T)
MAPE[18,4] <- median (MAPE[,4], na.rm = T)
MAPE[18,5] <- median (MAPE[,5], na.rm = T)
MAPE[18,6] <- median (MAPE[,6], na.rm = T)
MAPE[18,7] <- median (MAPE[,7], na.rm = T)
MAPE[18,8] <- median (MAPE[,8], na.rm = T)
MAPE[18,9] <- median (MAPE[,9], na.rm = T)

MAPE[, 2:9] = round(MAPE[, 2:9], 2)

MAPE %>%
  kable() %>%
  kable_styling()%>%
  row_spec(18, bold = T, color = "white", background = "grey")

# write.csv(MAPE, "CMSY and SPICT comparison stats TS match.csv")


test <- res[res$Year == 2017 & res$Stock !="Whg7a",]
test2 <- res[res$Year == 2015 & res$Stock =="Whg7a" ,]
test <- rbind (test, test2)
test_ICES <- test[test$Method== "ICES",]
test <- test[test$Method != "ICES",]

test3 <- merge(test, test_ICES, by = c("Stock", "Year", "variable"))
colnames(test3) <- c("Stock", "Year", "variable", "Method", "Est.Value", "ICES", "ICES.Value")

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

#######################################################
### Try a similar plot but relative to ICES rather than the refpoints

test3$Diff = test3$Est.Value - test3$ICES.Value

test4 <- dcast(test3, Stock + Year +  Method ~ variable)

pdf("Kobe plot Est-ICES.pdf", width =  6.69291, height = 6.69291)
ggplot (test4, aes(x= B_BMSY, y = F_FMSY, label = test4$Stock, colour = Method))+
  scale_color_manual(values=c("#D55E00", "#56B4E9"))+
  geom_point()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_label_repel (aes(x= B_BMSY, y = F_FMSY, label = test4$Stock))
dev.off()


