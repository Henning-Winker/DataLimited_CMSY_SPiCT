library(FLCore)
library (plyr)


Cod.7a.Data <-read.csv("Data\\Cod_7_a\\COD7A Catch pb.csv")

waa.df <-read.csv("Data\\Cod_7_a\\waa.cm.csv")
waa.df= waa.df[-2]

### Indicator 1
options(digits=9)
ind1 = read.csv("Data\\Cod_7_a\\nigfsq4.csv")
ind1= ind1[-2] ## remove age0 
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 7/8,
                                    index1 = rowSums(ind1[, ages] * waa.df[match(ind1$year, waa.df$year), -1]))



Cod.7a.Data = merge(Cod.7a.Data, ind1.bio, all.x = T)

## Indicator 2
ind2 = read.csv("Data\\Cod_7_a\\nigfsq1.csv")
ind2= ind2[-2] ## remove age0 
ages <- names(ind2)[-1]
ind2.bio <- data.frame(year = ind2$year,
                       year.index2 = ind2$year + 1/8,
                       index2 = rowSums(ind2[, ages] * waa.df[match(ind2$year, waa.df$year), -1]))

Cod.7a.Data = merge(Cod.7a.Data, ind2.bio, all.x = T)

## Indicator 3
ind3 = read.csv("Data\\Cod_7_a\\ukfsp.csv")
ind3= ind3[-2] ## remove age0 
ages <- names(ind3)[-1]
ind3.bio <- data.frame(year = ind3$year,
                       year.index3 = ind3$year + 4/8,
                       index3 = rowSums(ind3[, ages] * waa.df[match(ind3$year, waa.df$year), -1]))

Cod.7a.Data = merge(Cod.7a.Data, ind3.bio, all.x = T)


write.csv(Cod.7a.Data, "Data\\Cod_7_a\\Cod.7a.Data.csv")
