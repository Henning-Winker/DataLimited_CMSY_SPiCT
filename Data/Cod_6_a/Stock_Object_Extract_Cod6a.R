library(FLCore)
library (plyr)

### Load stock object
discards <-read.csv("Data\\Cod_6_a\\cod via discards tonnes pb.csv")
discards$discards = rowSums(discards[, 2:8])

landings = read.csv("Data\\Cod_6_a\\cod via landings tonnes pb.csv")
landings$landings = rowSums(landings[, 2:8])

Cod.6a.Data = cbind (landings, discards)
Cod.6a.Data$catch = Cod.6a.Data$landings + Cod.6a.Data$discards

Cod.6a.Data = Cod.6a.Data [ ,c(1,19)]

waa.df <-read.csv("Data\\Cod_6_a\\waa.pb.csv")

### Indicator 1
#options(digits=9)
ind1 = read.csv("Data\\Cod_6_a\\ScoGFSQ1.csv")
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 1/8,
                       index1 = rowSums(ind1[, ages] * waa.df[match(ind1$year, waa.df$year), -1]))

Cod.6a.Data = merge(Cod.6a.Data, ind1.bio, all.x = T)

### Indicator 2
#options(digits=9)
ind2 = read.csv("Data/Cod_6_a/IR_WCGFS_Q3.csv")
ind2= ind2[-2] ## remove age0 
ages <- names(ind2)[-1]
waa.wip = waa.df[, 1:4]
ind2.bio <- data.frame(year = ind2$year,
                       year.index2 = ind2$year + 5/8,
                       index2 = rowSums(ind2[, ages] * waa.wip[match(ind2$year, waa.wip$year), -1]))

Cod.6a.Data = merge(Cod.6a.Data, ind2.bio, all.x = T)

### Indicator 3
#options(digits=9)
ind3 = read.csv("Data/Cod_6_a/IRGFS_WIBTS_Q4.csv")
ages <- names(ind3)[-1]
waa.wip = waa.df[, 1:5]
ind3.bio <- data.frame(year = ind3$year,
                       year.index3 = ind3$year + 7/8,
                       index3 = rowSums(ind3[, ages] * waa.wip[match(ind3$year, waa.wip$year), -1]))

Cod.6a.Data = merge(Cod.6a.Data, ind3.bio, all.x = T)

### Indicator 4
#options(digits=9)
ind4 = read.csv("Data/Cod_6_a/ScoGFSQ4.csv")
ages <- names(ind4)[-1]
waa.wip = waa.df[, 1:7]
ind4.bio <- data.frame(year = ind4$year,
                       year.index4 = ind4$year + 7/8,
                       index4 = rowSums(ind4[, ages] * waa.wip[match(ind4$year, waa.wip$year), -1]))

Cod.6a.Data = merge(Cod.6a.Data, ind4.bio, all.x = T)


### Indicator 5
#options(digits=9)
ind5 = read.csv("Data/Cod_6_a/UKSGFS_WIBTS_Q1.csv")
ages <- names(ind5)[-1]
## use waa from 2017 for 2018
waa.wip = waa.df

ind5.bio <- data.frame(year = ind5$year,
                       year.index5 = ind5$year + 1/8,
                       index5 = rowSums(ind5[, ages] * waa.wip[match(ind5$year, waa.wip$year), -1]))

Cod.6a.Data = merge(Cod.6a.Data, ind5.bio, all.x = T)

### Indicator 6
#options(digits=9)
ind6 = read.csv("Data/Cod_6_a/UKSGFS_WIBTS_Q4.csv")
ind6= ind6[-2] ## remove age0 
ages <- names(ind6)[-1]
waa.wip = waa.df

ind6.bio <- data.frame(year = ind6$year,
                       year.index6 = ind6$year + 1/8,
                       index6 = rowSums(ind6[, ages] * waa.wip[match(ind6$year, waa.wip$year), -1]))

Cod.6a.Data = merge(Cod.6a.Data, ind6.bio, all.x = T)


write.csv(Cod.6a.Data, "Data\\Cod_6_a\\Cod.6a.Data.csv")
