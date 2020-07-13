library(FLCore)
library (plyr)

### Load stock object
ple=readFLStock('Data\\Ple_7_hk\\PLE7jkID.txt')
ple.indices=readFLIndices('Data\\Ple_7_hk\\PLE7jkTU.txt')


units(ple)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))

catch (ple)
landings(ple)
discards (ple)

### Extract catch data from the stock object
ple.data = as.data.frame(landings(ple))
Ple.7hk.Data = ple.data[ , c(2,7)]
colnames(Ple.7hk.Data) = c("year", "catch")

waa.df <-read.csv("Data\\Ple_7_hk\\waa.pb.csv")

### Indicator 1
#options(digits=9)
ind1 = read.csv("Data\\Ple_7_hk\\IRL_VMS.csv")
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year,
                       index1 = rowSums(ind1[, ages] * waa.df[match(ind1$year, waa.df$year), -1]))

Ple.7hk.Data = merge(Ple.7hk.Data, ind1.bio, all.x = T)

write.csv(Ple.7hk.Data, "Data\\Ple_7_hk\\Ple.7hk.Data.csv")
