library(FLCore)
library (plyr)

### Load stock object
cod<-readFLStock("Data\\Cod_7_ek\\index2018disfinal.txt")
units(cod)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))

### Extract catch data from the stock object
cod.data = as.data.frame(landings(cod))
# or normally
# cod.data = as.data.frame(catch(cod))
Cod.7ek.Data = cod.data[ , c(2,7)]
colnames(Cod.7ek.Data) = c("year", "catch")

waa.stock <- read.csv("Data\\Cod_7_ek\\waa.stock.pb.csv")
waa.catch <- read.csv("Data\\Cod_7_ek\\waa.catch.pb.csv")

### Indicator 1
options(digits=9)
ind1 = read.csv("Data\\Cod_7_ek\\FR-OTDEF-Q234.csv")
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 5/8,
                       index1 = rowSums(ind1[, ages] * waa.catch[match(ind1$year, waa.catch$year), -1]))



Cod.7ek.Data = merge(Cod.7ek.Data, ind1.bio, all.x = T)

## Indicator 2
ind2 = read.csv("Data\\Cod_7_ek\\IR-GFS FR-EVHOE Q4.csv")
ages <- names(ind2)[-1]
ind2.bio <- data.frame(year = ind2$year,
                       year.index2 = ind2$year + 7/8,
                       index2 = rowSums(ind2[, ages] * waa.stock[match(ind2$year, waa.stock$year), -1]))

Cod.7ek.Data = merge(Cod.7ek.Data, ind2.bio, all.x = T)


write.csv(Cod.7ek.Data, "Data\\Cod_7_ek\\Cod.7ek.Data.csv")
