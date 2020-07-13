library(FLCore)
library (plyr)

### Load stock object
had <-stock
units(had)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))

catch (had)

### Extract catch data from the stock object
had.data = as.data.frame(catch(had))
had.data = had.data[ , c(2,7)]
colnames(had.data) = c("year", "catch")


catch.wt(had)
waa.pb = as.data.frame(stock.wt(had))
waa.pb = waa.pb[ , c(1:2,7)]

waa.pb= reshape(waa.pb, idvar = "year", timevar = "age", direction = "wide")
names (waa.pb) = c("year", "age0", "age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8")

waa.pb= waa.pb[-2]

### Indicator 1
options(digits=9)
ind1 = read.csv("Data\\Had_7_bk\\WIBTS.csv")
ind1= ind1[-2] ## remove age0 
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 7/8,
                       index1 = rowSums(ind1[, ages] * waa.pb[match(ind1$year, waa.pb$year), -1]))



Had.7bk.Data = merge(had.data, ind1.bio, all.x = T)

## Indicator 2
ind2 = read.csv("Data\\Had_7_bk\\IRE Gad.csv")
ind2= ind2[-2] ## remove age0 
ages <- names(ind2)[-1]
ind2.bio <- data.frame(year = ind2$year,
                       year.index2 = ind2$year + 4/8,
                       index2 = rowSums(ind2[, ages] * waa.pb[match(ind2$year, waa.pb$year), -1]))

Had.7bk.Data = merge(Had.7bk.Data,  ind2.bio, all.x = T)

write.csv(Had.7bk.Data, "Data\\Had_7_bk\\Had.7bk.Data.csv")

