library(FLCore)
library (plyr)


Sol.2024.Data <-read.csv("Data\\Sol_2024\\Sol2024 Catch pb.csv")

waa.df <-read.csv("Data\\Sol_2024\\waa.pb.csv")


indices <- readFLIndices("Data\\Sol_2024\\survey pb.txt")

### Indicator 1
ind1 = as.data.frame(indices [[1]]@index)
ind1= ind1[ , c(1:2,7)]
ind1 = reshape(ind1, idvar = "year", timevar = "age", direction = "wide")
names (ind1) = c("year",  "age1", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9")
ind1= ind1[-2] ## remove age1 
ages <- names(ind1)[-1]

waa.wip = waa.df[-2]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 7/8,
                       index1 = rowSums(ind1[, ages] * waa.wip[match(ind1$year, waa.wip$year), -1]))


Sol.2024.Data = merge(Sol.2024.Data, ind1.bio, all.x = T)


### Indicator 2
ind2 = as.data.frame(indices [[2]]@index)
ind2= ind2[ , c(1:2,7)]
ind2 = reshape(ind2, idvar = "year", timevar = "age", direction = "wide")
names (ind2) = c("year", "age2", "age3", "age4", "age5", "age6", "age7", "age8")

ages <- names(ind2)

waa.wip = waa.df[, ages]
ages= ages[-1]

ind2.bio <- data.frame(year = ind2$year,
                       year.index2 = ind2$year,
                       index2 = rowSums(ind2[, ages] * waa.wip[match(ind2$year, waa.wip$year), -1]))

Sol.2024.Data = merge(Sol.2024.Data, ind2.bio, all.x = T)


### Indicator 3
ind3 = as.data.frame(indices [[3]]@index)
ind3= ind3[ , c(1:2,7)]
ind3 = reshape(ind3, idvar = "year", timevar = "age", direction = "wide")
names (ind3) = c("year", "age2", "age3", "age4", "age5", "age6")

ages <- names(ind3)

waa.wip = waa.df[, ages]
ages= ages[-1]

ind3.bio <- data.frame(year = ind3$year,
                       year.index3 = ind3$year,
                       index3 = rowSums(ind3[, ages] * waa.wip[match(ind3$year, waa.wip$year), -1]))


Sol.2024.Data = merge(Sol.2024.Data, ind3.bio, all.x = T)


write.csv(Sol.2024.Data, "Data\\Sol_2024\\Sol.2024.Data.csv")
