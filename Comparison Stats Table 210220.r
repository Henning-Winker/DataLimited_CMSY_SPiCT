library(reshape2)
library (kableExtra)
library (ggplot2); theme_set(theme_bw())
library (ggrepel)
library (plyr)
library(dplyr)

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

write.csv(MAPE, "CMSY and SPICT comparison stats review.csv")

################################################################
####    Outcome matching
################################################################
ro <- dcast(res, Stock + Year + variable ~ Method)
ro <- ro[complete.cases(ro),]

roFF <- ro[ro$variable == "F_FMSY", ]
roBB <- ro[ro$variable == "B_BMSY", ]

roFF$FFMSY_Spict <- ifelse(roFF$ICES > 1 & roFF$SPiCT >1, "True Overfishing", 
                           ifelse(roFF$ICES < 1 & roFF$SPiCT <1, "True Sustainable Fishing",
                                  ifelse(roFF$ICES > 1 & roFF$SPiCT <1, "False Sustainable Fishing", "False Overfishing")))

roFF$FFMSY_CMSY <- ifelse(roFF$ICES > 1 & roFF$CMSY >1, "True Overfishing", 
                          ifelse(roFF$ICES < 1 & roFF$CMSY <1, "True Sustainable Fishing",
                                 ifelse(roFF$ICES > 1 & roFF$CMSY <1, "False Sustainable Fishing", "False Overfishing")))

spict_count <- as.data.frame(count(roFF, FFMSY_Spict))
colnames (spict_count) <- c("Prediction", "SPICT")
cmsy_count <- as.data.frame(count(roFF, FFMSY_CMSY))
colnames (cmsy_count) <- c("Prediction", "CMSY")
FFMSY_outcome <- merge(spict_count, cmsy_count)

##########################################################################
####    B/BMSY

roBB$BBMSY_Spict <- ifelse(roBB$ICES > 1 & roBB$SPiCT >1, "True Healthy Stock", 
                           ifelse(roBB$ICES < 1 & roBB$SPiCT <1, "True Depleted Stock",
                                  ifelse(roBB$ICES > 1 & roBB$SPiCT <1, "False Depleted Stock", "False Healthy Stock")))

roBB$BBMSY_CMSY <- ifelse(roBB$ICES > 1 & roBB$CMSY >1, "True Healthy Stock", 
                          ifelse(roBB$ICES < 1 & roBB$CMSY <1, "True Depleted Stock",
                                 ifelse(roBB$ICES > 1 & roBB$CMSY <1, "False Depleted Stock", "False Healthy Stock")))


spict_count <- as.data.frame(count(roBB, BBMSY_Spict))
colnames (spict_count) <- c("Prediction", "SPICT")
cmsy_count <- as.data.frame(count(roBB, BBMSY_CMSY))
colnames (cmsy_count) <- c("Prediction", "CMSY")
BBMSY_outcome <- merge(spict_count, cmsy_count)


outcome <- rbind(FFMSY_outcome, BBMSY_outcome)
write.csv(outcome , "RO Outcome Table review.csv")

# BBMSY_Table = tableGrob(BBMSY_outcome, rows = NULL)
# 
# FFMSY_Table = tableGrob(FFMSY_outcome, rows = NULL)

spict_ff <- roFF %>% group_by(Stock, FFMSY_Spict) %>% tally() 
spict_ff <- dcast(spict_ff, Stock ~ FFMSY_Spict)
spict_ff$Method <- "SPiCT"

cmsy_ff <- roFF %>% group_by(Stock, FFMSY_CMSY) %>% tally() 
cmsy_ff <- dcast(cmsy_ff, Stock ~ FFMSY_CMSY)
cmsy_ff$Method <- "CMSY"

ro_ff_counts <- cbind(spict_ff, cmsy_ff)
write.csv(ro_ff_counts, "Outcome by stocks FF.csv")

### BBMSY

spict_bb <- roBB %>% group_by(Stock, BBMSY_Spict) %>% tally() 
spict_bb <- dcast(spict_bb, Stock ~ BBMSY_Spict)
spict_bb$Method <- "SPiCT"

cmsy_bb <- roBB %>% group_by(Stock, BBMSY_CMSY) %>% tally() 
cmsy_bb <- dcast(cmsy_bb, Stock ~ BBMSY_CMSY)
cmsy_bb$Method <- "CMSY"

ro_bb_counts <- cbind(spict_bb, cmsy_bb)
write.csv(ro_bb_counts, "Outcome by stocks BB.csv")
