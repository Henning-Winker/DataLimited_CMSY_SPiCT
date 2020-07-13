library(FLCore)
library (plyr)


catch <-read.csv("Data\\Had_6_b\\Had6b catch pb.csv")
catch$catch = rowSums(catch[, 2:8])


Had.6b.Data = catch [ ,c(1,9)]


waa.df <- read.csv("Data\\Had_6_b\\waa.pb.csv")
waa.df= waa.df[-2] ## remove age0 

### Indicator 1
options(digits=9)
ind1 = read.csv("Data\\Had_6_b\\SCOGFS.csv")
ind1= ind1[-2] ## remove age0 
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 5/8,
                       index1 = rowSums(ind1[, ages] * waa.df[match(ind1$year, waa.df$year), -1]))



Had.6b.Data = merge(Had.6b.Data, ind1.bio, all.x = T)


write.csv(Had.6b.Data, "Data\\Had_6_b\\Had.6b.Data.csv")
