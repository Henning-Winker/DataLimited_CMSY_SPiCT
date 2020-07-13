library(FLCore)
library (plyr)


Cod.Faroe.Data <-read.csv("Data\\Cod_Faroe\\CODFaroe Catch pb.csv")

waa.df <- read.csv("Data\\Cod_Faroe\\waa.pb.csv")


### Indicator 1
options(digits=9)
ind1 = read.csv("Data\\Cod_Faroe\\Cod Faroe Spring Survey.csv")
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 2/8,
                       index1 = rowSums(ind1[, ages] * waa.df[match(ind1$year, waa.df$year), -1]))



Cod.Faroe.Data = merge(Cod.Faroe.Data, ind1.bio, all.x = T)

## Indicator 2
ind2 = read.csv("Data\\Cod_Faroe\\Cod Faroe Summer Survey.csv")
ages <- names(ind2)[-1]
ind2.bio <- data.frame(year = ind2$year,
                       year.index2 = ind2$year + 4/8,
                       index2 = rowSums(ind2[, ages] * waa.df[match(ind2$year, waa.df$year), -1]))

Cod.Faroe.Data = merge(Cod.Faroe.Data, ind2.bio, all.x = T)


write.csv(Cod.Faroe.Data, "Data\\Cod_Faroe\\Cod.Faroe.Data.csv")
