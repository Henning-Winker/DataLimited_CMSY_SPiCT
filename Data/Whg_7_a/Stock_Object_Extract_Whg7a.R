library(FLCore)
library (plyr)

### Load stock object
whg <-stock
units(whg)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))

catch (whg)

waa.df <- read.csv("Data\\Whg_7_a\\waa.pb.csv")

### Extract catch data from the stock object
Whg.7a.data = as.data.frame(catch(whg))
Whg.7a.data = Whg.7a.data[ , c(2,7)]
colnames(Whg.7a.data) = c("year", "catch")



#### Cod Survey indices import
whg.indices <- tun

whg.indices [[1]]
whg.indices [[2]]
whg.indices [[3]]
whg.indices [[4]]


### Indicator 1
options(digits=9)
ind1 = read.csv("Data\\Whg_7_a\\NIGFSQ1.csv")
ages <- names(ind1)[-1]
waa.df.temp = waa.df[, c(1, 3:8)]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 1/8,
                       index1 = rowSums(ind1[, ages] * waa.df.temp[match(ind1$year, waa.df.temp$year), -1]))

Whg.7a.data = merge(Whg.7a.data, ind1.bio, all.x = T)

### Indicator 2
options(digits=9)
ind2 = read.csv("Data\\Whg_7_a\\NIGFSQ4.csv")
ages <- names(ind2)[-1]
ind2.bio <- data.frame(year = ind2$year,
                       year.index2 = ind2$year + 7/8,
                       index2 = rowSums(ind2[, ages] * waa.df[match(ind2$year, waa.df$year), -1]))

Whg.7a.data = merge(Whg.7a.data, ind2.bio, all.x = T)

### Indicator 3
options(digits=9)
ind3 = read.csv("Data\\Whg_7_a\\UKBTSQ3.csv")
ages <- names(ind3)[-1]
waa.df.temp = waa.df[, c(1:3)]
ind3.bio <- data.frame(year = ind3$year,
                       year.index3 = ind3$year + 6/8,
                       index3 = rowSums(ind3[, ages] * waa.df.temp[match(ind3$year, waa.df.temp$year), -1]))

Whg.7a.data = merge(Whg.7a.data, ind3.bio, all.x = T)


write.csv(Whg.7a.data, "Data\\Whg_7_a\\Whg.7a.Data.csv")
