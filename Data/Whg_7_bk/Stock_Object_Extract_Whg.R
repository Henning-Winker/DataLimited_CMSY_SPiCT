library(FLCore)
library (plyr)

### Load stock object
whg <-stock
units(whg)[1:17] <- as.list(c(rep(c("tonnes","thousands","kg"),4), "NA", "NA", "f", "NA", "NA"))

catch (whg)

### Extract catch data from the stock object
Whg.7bk.data = as.data.frame(catch(whg))
Whg.7bk.data = Whg.7bk.data[ , c(2,7)]
colnames(Whg.7bk.data) = c("year", "catch")

waa.df <- read.csv("Data\\Whg_7_bk\\waa.pb.csv")
waa.df.ind1 = waa.df[ , 1:7]

# waa= as.data.frame(stock.wt(whg))

### Indicator 1
options(digits=9)
ind1 = read.csv("Data\\Whg_7_bk\\IGFS.csv")
ages <- names(ind1)[-1]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 7/8,
                       index1 = rowSums(ind1[, ages] * waa.df.ind1[match(ind1$year, waa.df.ind1$year), -1]))



Whg.7bk.data = merge(Whg.7bk.data, ind1.bio, all.x = T)


write.csv(Whg.7bk.data, "Data\\Whg_7_bk\\Whg.7bk.data.csv")
