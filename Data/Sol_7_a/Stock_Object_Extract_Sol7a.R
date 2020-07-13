library(FLCore)
library (plyr)


Sol.7a.Data = read.csv("Data\\Sol_7_a\\sol7a catch pb.csv")

waa.df <-read.csv("Data\\Sol_7_a\\waa.pb.csv")

### Indicator 1
#options(digits=9)
ind1 = read.csv("Data\\Sol_7_a\\UK_BTS_Q3.csv")
ages <- names(ind1)[-1]
waa.df = waa.df[, 1:7]
ind1.bio <- data.frame(year = ind1$year,
                       year.index1 = ind1$year + 5/8,
                       index1 = rowSums(ind1[, ages] * waa.df[match(ind1$year, waa.df$year), -1]))

Sol.7a.Data = merge(Sol.7a.Data, ind1.bio, all.x = T)


write.csv(Sol.7a.Data, "Data\\Sol_7_a\\Sol.7a.Data.csv")
