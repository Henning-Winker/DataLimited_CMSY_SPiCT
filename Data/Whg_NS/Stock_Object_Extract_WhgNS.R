library(FLCore)
library (plyr)


Whg.NS.Data <-read.csv("Data\\Whg_NS\\Whg_NS Catch pb.csv")

waa.df <- read.csv("Data\\Whg_NS\\waa.pb.csv")

waa.df.ind1 = waa.df[ , c(1, 3:7)]
waa.df.ind2 = waa.df[ , 1:7]

### Indicator 1
options(digits=9)
ind1 = read.csv("Data\\Whg_NS\\NS_IBTS_Q1.csv")
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 1/8,
                       index1 = rowSums(ind1[, ages] * waa.df.ind1[match(ind1$year, waa.df.ind1$year), -1]))



Whg.NS.Data = merge(Whg.NS.Data, ind1.bio, all.x = T)

## Indicator 2
ind2 = read.csv("Data\\Had_NS\\NS_IBTS_Q3.csv")
ages <- names(ind2)[-1]
ind2.bio <- data.frame(year = ind2$year,
                       year.index2 = ind2$year + 5/8,
                       index2 = rowSums(ind2[, ages] * waa.df.ind2[match(ind2$year, waa.df.ind2$year), -1]))

Whg.NS.Data = merge(Whg.NS.Data, ind2.bio, all.x = T)



write.csv(Whg.NS.Data, "Data\\Whg_NS\\Whg.NS.Data.csv")
