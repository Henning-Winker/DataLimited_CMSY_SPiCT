library(spict)
options(scipen = 999)


Cod6a.data = read.csv("Data/Cod_6_a/Cod.6a.Data.csv")
Cod7ek.data = read.csv("Data/Cod_7_ek/Cod.7ek.Data.csv")
CodFaroe.data = read.csv("Data/Cod_Faroe/Cod.Faroe.Data.csv")
CodNS.data = read.csv("Data/Cod_NS/Cod.NS.Data.csv")
Had6b.data = read.csv("Data/Had_6_b/Had.6b.Data.csv")
Had7bk.data = read.csv("Data/Had_7_bk/Had.7bk.Data.csv")
HadNS.data = read.csv("Data/Had_NS/Had.NS.Data.csv")
Ple7a.data = read.csv("Data/Ple_7_a/Ple.7a.Data.csv")
Ple7hk.data = read.csv("Data/Ple_7_hk/Ple.7hk.Data.csv")
Sol7a.data = read.csv("Data/Sol_7_a/Sol.7a.Data.csv")
Sol7fg.data = read.csv("Data/Sol_7_fg/Sol.7fg.Data.csv")
Sol7hk.data = read.csv("Data/Sol_7_hk/Sol.7hk.Data.csv")
Sol2024.data = read.csv("Data/Sol_2024/Sol.2024.Data.csv")
Whg6a.data = read.csv("Data/Whg_6_a/Whg.6a.Data.csv")
Whg7a.data = read.csv("Data/Whg_7_a/Whg.7a.Data.csv")
Whg7bk.data = read.csv("Data/Whg_7_bk/Whg.7bk.Data.csv")
WhgNS.data = read.csv("Data/Whg_NS/Whg.NS.Data.csv")

## how much to increase/decrease the prior by
p = 0.1

stocklist <- c("Cod6a", "Cod7ek", 
               "CodFaroe", "CodNS",
               "Had6b" , "Had7bk" , "HadNS" , 
               "Ple7a",  "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")


all_res <- data.frame(matrix(ncol=5,nrow=0, 
                             dimnames=list(NULL, c("Rep", "B_BMSY_start", "B_BMSY_mid", "B_BMSY_end", "Stock"))))


for (i in stocklist){
  
  data = get(paste0(i, ".data"))
  
  No.Index = (ncol(data) - 3)/2
  
  inp <- list()
  inp$obsC <- data$catch
  inp$timeC <- data$year
  
  ### Use one abundance index or all
  if (No.Index == 1){
    ### Use only one abundance index
    index = data[complete.cases(data$index1), ]
    inp$obsI <- index$index1
    inp$timeI <- index$year.index1
  } else {
    if (No.Index == 2){
      ### Use 2 abundance index
      index1 = data[complete.cases(data$index1), ]
      index2 = data[complete.cases(data$index2), ]
      inp$obsI[[1]] <- index1$index1
      inp$obsI[[2]] <- index2$index2
      inp$timeI <- list(index1$year.index1, index2$year.index2)
    }
    else if (No.Index == 3){
      ### Use 3 abundance index
      index1 = data[complete.cases(data$index1), ]
      index2 = data[complete.cases(data$index2), ]
      index3 = data[complete.cases(data$index3), ]
      inp$obsI[[1]] <- index1$index1
      inp$obsI[[2]] <- index2$index2
      inp$obsI[[3]] <- index3$index3
      inp$timeI <- list(index1$year.index1, index2$year.index2, index3$year.index3)
    } else if(No.Index == 4){
      ### Use 4 abundance index
      index1 = data[complete.cases(data$index1), ]
      index2 = data[complete.cases(data$index2), ]
      index3 = data[complete.cases(data$index3), ]
      index4 = data[complete.cases(data$index4), ]
      inp$obsI[[1]] <- index1$index1
      inp$obsI[[2]] <- index2$index2
      inp$obsI[[3]] <- index3$index3
      inp$obsI[[4]] <- index4$index4
      inp$timeI <- list(index1$year, index2$year.index2, index3$year.index3, index4$year.index4)
    } else if (No.Index == 5){
      ### Use 5 abundance index
      index1 = data[complete.cases(data$index1), ]
      index2 = data[complete.cases(data$index2), ]
      index3 = data[complete.cases(data$index3), ]
      index4 = data[complete.cases(data$index4), ]
      index5 = data[complete.cases(data$index5), ]
      inp$obsI[[1]] <- index1$index1
      inp$obsI[[2]] <- index2$index2
      inp$obsI[[3]] <- index3$index3
      inp$obsI[[4]] <- index4$index4
      inp$obsI[[5]] <- index5$index5
      inp$timeI <- list(index1$year, index2$year.index2, index3$year.index3, index4$year.index4, index5$year.index5) 
    } else {
      ### Use 6 abundance index
      index1 = data[complete.cases(data$index1), ]
      index2 = data[complete.cases(data$index2), ]
      index3 = data[complete.cases(data$index3), ]
      index4 = data[complete.cases(data$index4), ]
      index5 = data[complete.cases(data$index5), ]
      index6 = data[complete.cases(data$index6), ]
      inp$obsI[[1]] <- index1$index1
      inp$obsI[[2]] <- index2$index2
      inp$obsI[[3]] <- index3$index3
      inp$obsI[[4]] <- index4$index4
      inp$obsI[[5]] <- index5$index5
      inp$obsI[[6]] <- index6$index6
      inp$timeI <- list(index1$year, index2$year.index2, index3$year.index3, index4$year.index4, index5$year.index5, index6$year.index6) 
    }}
  
  inp$timeC <- data$year[complete.cases(data$index1)]
  inp$obsC <- data$catch[complete.cases(data$index1)]
  
  inp$ini$logn <- log(2) 
  inp$phases$logn <- -1
  
  if (grepl("Cod", i)){r = 0.53}
  if (grepl("Ple", i)){r = 0.53}
  if (grepl("Had", i)){r = 0.5}
  if (grepl("Sol", i)){r = 0.5}
  if (grepl("Whg", i)){r = 0.49}
  
  inp$priors$logr <- c(log(r), 1, 1)
  
  res <- fit.spict(inp)
  
  inp.l <- inp
  inp.l$priors$logr <- c(log(r*(1-p)), 1, 1)
  
  res.l <- fit.spict(inp.l)

  inp.u <- inp
  inp.u$priors$logr <- c(log(r*(1+p)), 1, 1)
  
  res.u <- fit.spict(inp.u)
  
  start.yr <- inp$timeC[1]
  end.yr <- max(data$year)
  mid.yr <- end.yr- ((end.yr-start.yr)/2) 
  
  
  output <- data.frame(matrix(ncol=5,nrow=3, 
                                         dimnames=list(NULL, c("Rep", "B_BMSY_start", "B_BMSY_mid", "B_BMSY_end", "Stock"))))
  
  output$Stock <- i
  
  output$Rep[1] <- "r"
  output$B_BMSY_start[1]  <- get.par('logBBmsy', res, exp=TRUE)[as.character(start.yr), "est"]
  output$B_BMSY_mid[1]  <- get.par('logBBmsy', res, exp=TRUE)[as.character(mid.yr), "est"]
  output$B_BMSY_end[1]  <- get.par('logBBmsy', res, exp=TRUE)[as.character(end.yr), "est"]    
  
  output$Rep[2] <- "r_low"
  output$B_BMSY_start[2]  <- get.par('logBBmsy', res.l, exp=TRUE)[as.character(start.yr), "est"]
  output$B_BMSY_mid[2]  <- get.par('logBBmsy', res.l, exp=TRUE)[as.character(mid.yr), "est"]
  output$B_BMSY_end[2]  <- get.par('logBBmsy', res.l, exp=TRUE)[as.character(end.yr), "est"]  
  
  output$Rep[3] <- "r_hi"
  output$B_BMSY_start[3]  <- get.par('logBBmsy', res.u, exp=TRUE)[as.character(start.yr), "est"]
  output$B_BMSY_mid[3]  <- get.par('logBBmsy', res.u, exp=TRUE)[as.character(mid.yr), "est"]
  output$B_BMSY_end[3]  <- get.par('logBBmsy', res.u, exp=TRUE)[as.character(end.yr), "est"] 
  
 
  all_res <- rbind(all_res, output) 
}


save(all_res, file = "Jacobian/Spict_Jacobian_results.RDat")
