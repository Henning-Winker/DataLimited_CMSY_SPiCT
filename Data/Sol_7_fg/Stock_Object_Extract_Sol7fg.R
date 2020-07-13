library(FLCore)
library (plyr)


Sol.7fg.Data = read.csv("Data\\Sol_7_fg\\sol7fg catch pb.csv")

waa.df <-read.csv("Data\\Sol_7_fg\\waa.pb.csv")

tun.indices <- readFLIndices("Data\\Sol_7_fg\\tun.txt")

#tun.indices [[2]]

### Indicator 1
ind1 = as.data.frame(tun.indices [[1]]@index)
ind1= ind1[ , c(1:2,7)]
ind1 = reshape(ind1, idvar = "year", timevar = "age", direction = "wide")
names (ind1) = c("year",  "age2", "age3", "age4", "age5", "age6", "age7", "age8", 
                 "age9", "age10", "age11", "age12", "age13", "age14")
ages <- names(ind1)

waa.wip = waa.df[, ages]
ages= ages[-1]

ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year,
                       index1 = rowSums(ind1[, ages] * waa.wip[match(ind1$year, waa.wip$year), -1]))


Sol.7fg.Data = merge(Sol.7fg.Data, ind1.bio, all.x = T)

### Indicator 2
ind2 = as.data.frame(tun.indices [[2]]@index)
ind2= ind2[ , c(1:2,7)]
ind2 = reshape(ind2, idvar = "year", timevar = "age", direction = "wide")
names (ind2) = c("year",  "age2", "age3", "age4", "age5", "age6", "age7", "age8", 
                 "age9", "age10", "age11", "age12", "age13", "age14")
ages <- names(ind2)

waa.wip = waa.df[, ages]
ages= ages[-1]

ind2.bio <- data.frame(year = ind2$year,
                       year.index2 = ind2$year,
                       index2 = rowSums(ind2[, ages] * waa.wip[match(ind2$year, waa.wip$year), -1]))


Sol.7fg.Data = merge(Sol.7fg.Data, ind2.bio, all.x = T)



### Indicator 3
ind3 = as.data.frame(tun.indices [[3]]@index)
ind3= ind3[ , c(1:2,7)]
ind3 = reshape(ind3, idvar = "year", timevar = "age", direction = "wide")
names (ind3) = c("year",  "age2", "age3", "age4", "age5", "age6", "age7", "age8", 
                 "age9", "age10", "age11", "age12", "age13", "age14")
ages <- names(ind3)

waa.wip = waa.df[, ages]
ages= ages[-1]

ind3.bio <- data.frame(year = ind3$year,
                       year.index3 = ind3$year,
                       index3 = rowSums(ind3[, ages] * waa.wip[match(ind3$year, waa.wip$year), -1]))


Sol.7fg.Data = merge(Sol.7fg.Data, ind3.bio, all.x = T)


### Indicator 4
ind4 = as.data.frame(tun.indices [[4]]@index)
ind4= ind4[ , c(1:2,7)]
ind4 = reshape(ind4, idvar = "year", timevar = "age", direction = "wide")
names (ind4) = c("year",  "age1", "age2", "age3", "age4", "age5")
ind4 <- ind4[-2]
ages <- names(ind4)

waa.wip = waa.df[, ages]
ages= ages[-1]

ind4.bio <- data.frame(year = ind4$year,
                       year.index4 = ind4$year + 7/8,
                       index4 = rowSums(ind4[, ages] * waa.wip[match(ind4$year, waa.wip$year), -1]))


Sol.7fg.Data = merge(Sol.7fg.Data, ind4.bio, all.x = T)


write.csv(Sol.7fg.Data, "Data\\Sol_7_fg\\Sol.7fg.Data.csv")
