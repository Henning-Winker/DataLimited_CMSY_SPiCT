library(FLCore)
library (plyr)


Ple.7a.Data = read.csv("Data\\Ple_7_a\\ple7a catch pb.csv")

waa.df = read.csv("Data\\Ple_7_a\\waa.pb.csv")

tun.indices <- readFLIndices("Data\\Ple_7_a\\surveys pb.txt")

### Indicator 1
ind1 = as.data.frame(tun.indices [[1]]@index)
ind1= ind1[ , c(1:2,7)]
ind1 = reshape(ind1, idvar = "year", timevar = "age", direction = "wide")
names (ind1) = c("year", "age1","age2", "age3", "age4", "age5", "age6", "age7")
ages <- names(ind1)[-1]

waa.wip = waa.df[ , 1:8]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year,
                       index1 = rowSums(ind1[, ages] * waa.wip[match(ind1$year, waa.wip$year), -1]))

Ple.7a.Data = merge(Ple.7a.Data, ind1.bio, all.x = T)

write.csv(Ple.7a.Data, "Data\\ple_7_a\\Ple.7a.Data.csv")
