library(FLCore)
library (plyr)


Had.NS.Data <-read.csv("Data\\Had_NS\\Had_NS Catch pb.csv")

waa.df <- read.csv("Data\\Had_NS\\waa.pb.csv")

waa.df.ind1 = waa.df[ , c(1, 3:7)]
waa.df.ind2 = waa.df[ , 1:7]

### Indicator 1
options(digits=9)
ind1 = read.csv("Data\\Had_NS\\NS_IBTS_Q1.csv")
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 1/8,
                       index1 = rowSums(ind1[, ages] * waa.df.ind1[match(ind1$year, waa.df.ind1$year), -1]))



Had.NS.Data = merge(Had.NS.Data, ind1.bio, all.x = T)

## Indicator 2
ind2 = read.csv("Data\\Had_NS\\NS_IBTS_Q3.csv")
ind2= ind2[-2] ## remove age0 
ages <- names(ind2)[-1]
ind2.bio <- data.frame(year = ind1$year,
                       year.index2 = ind1$year + 5/8,
                       index2 = rowSums(ind1[, ages] * waa.df.ind1[match(ind1$year, waa.df.ind2$year), -1]))

Had.NS.Data = merge(Had.NS.Data, ind2.bio, all.x = T)


write.csv(Had.NS.Data, "Data\\Had_NS\\Had.NS.Data.csv")

