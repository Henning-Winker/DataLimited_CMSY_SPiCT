library(FLCore)
library (plyr)

### Load stock object
sol=readFLStock('Data\\Sol_7_hk\\sol7jkID.txt')

units(sol)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))


### Extract catch data from the stock object
sol.data = as.data.frame(landings(sol))
sol.data = sol.data[ , c(2,7)]
colnames(sol.data) = c("year", "catch")

waa.df = as.data.frame(stock.wt (sol))

waa.df= waa.df[ , c(1:2,7)]
waa.df = reshape(waa.df, idvar = "year", timevar = "age", direction = "wide")
names (waa.df) = c("year", "age2", "age3", "age4", "age5", "age6", "age7", "age8", "age9", 
                   "age10", "age11", "age12", "age13", "age14", "age15", "age16")


#### Survey indices import
sol.indices=readFLIndices('Data\\Sol_7_hk\\sol7jkTU.txt')

### Indicator 1
ind1 = as.data.frame(sol.indices [[1]]@index)
ind1= ind1[ , c(1:2,7)]
ind1 = reshape(ind1, idvar = "year", timevar = "age", direction = "wide")
names (ind1) = c("year",  "age2", "age3", "age4", "age5", "age6", "age7", "age8", 
                 "age9", "age10", "age11", "age12", "age13", "age14", "age15", "age16")
ages <- names(ind1)[-1]

waa.wip = waa.df
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year,
                       index1 = rowSums(ind1[, ages] * waa.wip[match(ind1$year, waa.wip$year), -1]))

sol.data = merge(sol.data, ind1.bio, all.x = T)


write.csv(sol.data, "Data\\Sol_7_hk\\Sol.7hk.Data.csv")
