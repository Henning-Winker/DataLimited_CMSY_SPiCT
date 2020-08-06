library(datalimited2)
library(reshape2)
library(ggplot2)

Cod6a.data = read.csv("Data/Cod_6_a/Cod.6a.Data.csv")

# Cod6a.cmsy = cmsy2(year = Cod6a.data$year, catch = Cod6a.data$catch, r.low = 0.2, r.hi = 0.8)


###################################################
####    Needed Functions
###################################################

# Function to calculate moving average
ma <- function(x){
  x.1 <- stats::filter(x,rep(1/3,3),sides=1)
  x.1[1] <- x[1]
  x.1[2] <- (x[1]+x[2])/2
  return(x.1)
}

########################################################
###   Functions to calculate biomass priors

# Set start saturation prior
startbio_prior <- function(stb.low, stb.hi, start.yr){
  # use initial biomass range from input file if stated
  if(is.na(stb.low)==F & is.na(stb.hi)==F){
    startbio <- c(stb.low,stb.hi)
  }else{
    # if start year < 1960 assume high biomass
    if(start.yr < 1960){
      startbio <- c(0.5,0.9)
    }else{
      # else use medium prior biomass range
      startbio <- c(0.2,0.6)
    }
  }
  return(startbio)
}

# Set intermediate saturation prior
intbio_prior <- function(intb.low, intb.hi, int.yr, start.yr, end.yr, startbio, yr, ct){
  # get index of years with lowest and highest catch between start+3 and end-3 years
  min.yr.i <- which.min(ct[4:(length(ct)-3)])+3
  max.yr.i <- which.max(ct[4:(length(ct)-3)])+3
  min.ct <- ct[min.yr.i]
  max.ct <- ct[max.yr.i]
  # use year and biomass range for intermediate biomass from input file
  if(is.na(intb.low)==F & is.na(intb.hi)==F){
    int.yr   <- int.yr
    intbio   <- c(intb.low,intb.hi)
    # if contrast in catch is low, use initial range again in mid-year
  }else if(min(ct)/max(ct) > 0.6) {
    int.yr    <- as.integer(mean(c(start.yr, end.yr)))
    intbio    <- startbio
    # else if year of minimum catch is after max catch then use min catch
  }else if(min.yr.i > max.yr.i) {
    int.yr    <- yr[min.yr.i-1]
    if(startbio[1]>=0.5 &  (int.yr-start.yr) < (end.yr-int.yr) &
       (min.ct/max.ct) > 0.3) intbio <- c(0.2,0.6) else intbio <- c(0.01,0.4)
       # else use max catch
  } else {
    # assume that biomass range in year before maximum catch was high or medium
    int.yr    <- yr[max.yr.i-1]
    intbio    <- if((startbio[1]>=0.5 & (int.yr-start.yr) < (end.yr-int.yr))| # if initial biomass is high, assume same for intermediate
                    # ((min.ct/max.ct < 0.3 & (max.yr.i - min.yr.i) < 25))) c(0.5,0.9) else c(0.2,0.6) }
                    (((max.ct-min.ct)/max.ct)/(max.yr.i-min.yr.i) > 0.04)) c(0.5,0.9) else c(0.2,0.6) } # if incease is steep, assume high, else medium
  out <- list(intbio, int.yr)
  return(out)
}

# Set end saturation prior
endbio_prior <- function(endb.low, endb.hi, nyr, ct.raw, ct){
  # final biomass range from input file
  if(is.na(endb.low)==F & is.na(endb.hi)==F){
    endbio   <- c(endb.low,endb.hi)
  }else{
    # else use mean final catch/max catch to estimate final biomass
    rawct.ratio=ct.raw[nyr]/max(ct)
    endbio  <- if(ct[nyr]/max(ct) > 0.8) {c(0.4,0.8)} else if(rawct.ratio < 0.5) {c(0.01,0.4)} else {c(0.2,0.6)}
    
    # if default endbio is low (0.01-0.4), check whether the upper bound should be lower than 0.4 for depleted stocks
    if(endbio[2]==0.4){
      if(rawct.ratio< 0.05) {endbio[2] <- 0.1} else
        if(rawct.ratio< 0.15) {endbio[2] <- 0.2} else
          if(rawct.ratio< 0.35) {endbio[2] <- 0.3} else {endbio[2] <- 0.4}
    }
  }
  return(endbio)
}

# Set K prior
k_prior <- function(endbio, start.r, ct){
  # initial prior range of k values, assuming min k will be larger than max catch / prior for r
  if(mean(endbio) <= 0.5){
    start.k <- c(max(ct)/start.r[2],4*max(ct)/start.r[1])
  }else{
    start.k <- c(2*max(ct)/start.r[2],12*max(ct)/start.r[1])
  }
  return(start.k)
}


##################################################
### CM Jacobian function set up


Cod6a.data = read.csv("Data/Cod_6_a/Cod.6a.Data.csv")
data <-Cod6a.data

# Calculate 3-yr moving average (average of past 3 years)
ct.raw <- data$catch# / 1000
ct <- ma(data$catch)

# Identify number of years and start/end years
yr <- data$year # functions use this quantity
nyr <- length(yr)
start.yr <- min(yr)
end.yr <- max(yr)

# Prior information for resilience (r) of stock ---------------------------

# Can either classify as High, Medium, Low or Very low or manually set the boundaries
pb.resilience = "Medium"
## Lower r estimate
pb.r.low = NA
## Upper r estimate
pb.r.hi = NA

if (is.na(pb.r.low))  {if (pb.resilience == "High"){pb.r.low = 0.6; pb.r.hi = 1.5; start.r =  c(0.6,1.5)}
  else if (pb.resilience == "Medium"){pb.r.low = 0.2; pb.r.hi = 0.8; start.r =  c(0.2,0.8)}
  else if (pb.resilience == "Low"){pb.r.low = 0.05; pb.r.hi = 0.5; start.r =  c(0.05,0.5)}
  else if (pb.resilience == "Very low"){pb.r.low = 0.015; pb.r.hi = 0.1; start.r =  c(0.015,0.1)}}

endb.hi<- endb.low <-intb.hi<- intb.low <-stb.hi<- stb.low <- NA

startbio <- startbio_prior(stb.low, stb.hi, start.yr)
int_params <- intbio_prior(intb.low, intb.hi, int.yr, start.yr, end.yr, startbio, yr, ct)
intbio <- int_params[[1]]
int.yr <- int_params[[2]]
endbio <- endbio_prior(endb.low, endb.hi, nyr, ct.raw, ct)
start.k <- k_prior(endbio, start.r, ct)

#intermediate year as a position in the time series
int.yr.num <- int.yr - start.yr


############################################################
#### PB Sensitivity ####

## start it where we start the main cmsy run
start_prior <- c(rl = pb.r.low, ru = pb.r.hi,
#                 kl = start.k[1], ku = start.k[2],
                 p0l = startbio[1], p0u = startbio[2],
                 pMl = intbio[1], pMu = intbio[2],
                 pTl = endbio[1], pTu = endbio[2])

# Cod6a.cmsy = cmsy2(year = Cod6a.data$year, catch = Cod6a.data$catch, resilience = "Medium")


p= 0.1

cmsy_input <- matrix(data = start_prior, nrow = 17, ncol = 8, byrow = T)
cmsy_input[1,1] <- cmsy_input[1,1] * (1 + p)
cmsy_input[2,1] <- cmsy_input[2,1] * (1 - p)

cmsy_input[3,2] <- cmsy_input[3,2] * (1 + p)
cmsy_input[4,2] <- cmsy_input[4,2] * (1 - p)

cmsy_input[5,3] <- cmsy_input[5,3] * (1 + p)
cmsy_input[6,3] <- cmsy_input[6,3] * (1 - p)

cmsy_input[7,4] <- cmsy_input[7,4] * (1 + p)
cmsy_input[8,4] <- cmsy_input[8,4] * (1 - p)

cmsy_input[9,5] <- cmsy_input[9,5] * (1 + p)
cmsy_input[10,5] <- cmsy_input[10,5] * (1 - p)

cmsy_input[11,6] <- cmsy_input[11,6] * (1 + p)
cmsy_input[12,6] <- cmsy_input[12,6] * (1 - p)

cmsy_input[13,7] <- cmsy_input[13,7] * (1 + p)
cmsy_input[14,7] <- cmsy_input[14,7] * (1 - p)

cmsy_input[15,8] <- cmsy_input[15,8] * (1 + p)
cmsy_input[16,8] <- cmsy_input[16,8] * (1 - p)


cmsy_input

rep_names <- c("rlow_up", "rlow_down",  "rhi_up", "rhi_down",
               "stblow_up", "stblow_down", "stbhi_up", "stbhi_down",
               "intblow_up", "intblow_down", "intbhi_up", "intbhi_down",
               "endblow_up", "endblow_down", "endbhi_up", "endbhi_down", "Orig")



#### rep 1 ####
cmsy_output <- data.frame(Rep = rep_names, B_BMSY_start = NA,
                          B_BMSY_mid = NA, B_BMSY_end = NA)

ptm <- proc.time()
for (i in 1:17){
  temp <- cmsy2(year = Cod6a.data$year, catch = Cod6a.data$catch, resilience = NA,
                r.low = cmsy_input[i,1], r.hi =cmsy_input[i,2],
                stb.low = cmsy_input[i,3], stb.hi = cmsy_input[i,4],
                int.yr = int.yr,
                intb.low = cmsy_input[i,5], intb.hi = cmsy_input[i,6], 
                endb.low = cmsy_input[i,7], endb.hi = cmsy_input[i,8], verbose = T)
  
  cmsy_output$B_BMSY_start[i] <- temp$ref_ts$bbmsy[[1]]
  cmsy_output$B_BMSY_mid[i] <- temp$ref_ts$bbmsy[temp$ref_ts$year == int.yr]
  cmsy_output$B_BMSY_end[i] <- temp$ref_ts$bbmsy[length(temp$ref_ts$bbmsy)]
  
}


cmsy_output$Stock <- "Cod6a"
save(cmsy_output, file = "Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test1.RData")

timeOp1 = (proc.time() - ptm)/60


#### rep 2 ####
cmsy_output <- data.frame(Rep = rep_names, B_BMSY_start = NA,
                          B_BMSY_mid = NA, B_BMSY_end = NA)


ptm <- proc.time()
for (i in 1:17){
  temp <- cmsy2(year = Cod6a.data$year, catch = Cod6a.data$catch, resilience = NA,
                r.low = cmsy_input[i,1], r.hi =cmsy_input[i,2],
                stb.low = cmsy_input[i,3], stb.hi = cmsy_input[i,4],
                int.yr = int.yr,
                intb.low = cmsy_input[i,5], intb.hi = cmsy_input[i,6], 
                endb.low = cmsy_input[i,7], endb.hi = cmsy_input[i,8], verbose = T)
  
  cmsy_output$B_BMSY_start[i] <- temp$ref_ts$bbmsy[[1]]
  cmsy_output$B_BMSY_mid[i] <- temp$ref_ts$bbmsy[temp$ref_ts$year == int.yr]
  cmsy_output$B_BMSY_end[i] <- temp$ref_ts$bbmsy[length(temp$ref_ts$bbmsy)]
  
}


cmsy_output$Stock <- "Cod6a"
save(cmsy_output, file = "Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test2.RData")
timeOp2 = (proc.time() - ptm)/60

#### rep 3 ####
cmsy_output <- data.frame(Rep = rep_names, B_BMSY_start = NA,
                          B_BMSY_mid = NA, B_BMSY_end = NA)

ptm <- proc.time()
for (i in 1:17){
  temp <- cmsy2(year = Cod6a.data$year, catch = Cod6a.data$catch, resilience = NA,
                r.low = cmsy_input[i,1], r.hi =cmsy_input[i,2],
                stb.low = cmsy_input[i,3], stb.hi = cmsy_input[i,4],
                int.yr = int.yr,
                intb.low = cmsy_input[i,5], intb.hi = cmsy_input[i,6], 
                endb.low = cmsy_input[i,7], endb.hi = cmsy_input[i,8], verbose = T)
  
  cmsy_output$B_BMSY_start[i] <- temp$ref_ts$bbmsy[[1]]
  cmsy_output$B_BMSY_mid[i] <- temp$ref_ts$bbmsy[temp$ref_ts$year == int.yr]
  cmsy_output$B_BMSY_end[i] <- temp$ref_ts$bbmsy[length(temp$ref_ts$bbmsy)]
  
}


cmsy_output$Stock <- "Cod6a"
save(cmsy_output, file = "Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test3.RData")
timeOp3 = (proc.time() - ptm)/60


#### rep 4 ####
cmsy_output <- data.frame(Rep = rep_names, B_BMSY_start = NA,
                          B_BMSY_mid = NA, B_BMSY_end = NA)

ptm <- proc.time()
for (i in 1:17){
  temp <- cmsy2(year = Cod6a.data$year, catch = Cod6a.data$catch, resilience = NA,
                r.low = cmsy_input[i,1], r.hi =cmsy_input[i,2],
                stb.low = cmsy_input[i,3], stb.hi = cmsy_input[i,4],
                int.yr = int.yr,
                intb.low = cmsy_input[i,5], intb.hi = cmsy_input[i,6], 
                endb.low = cmsy_input[i,7], endb.hi = cmsy_input[i,8], verbose = T)
  
  cmsy_output$B_BMSY_start[i] <- temp$ref_ts$bbmsy[[1]]
  cmsy_output$B_BMSY_mid[i] <- temp$ref_ts$bbmsy[temp$ref_ts$year == int.yr]
  cmsy_output$B_BMSY_end[i] <- temp$ref_ts$bbmsy[length(temp$ref_ts$bbmsy)]
  
}


cmsy_output$Stock <- "Cod6a"
save(cmsy_output, file = "Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test4.RData")
timeOp4 = (proc.time() - ptm)/60

#### rep 5 ####
cmsy_output <- data.frame(Rep = rep_names, B_BMSY_start = NA,
                          B_BMSY_mid = NA, B_BMSY_end = NA)

ptm <- proc.time()
for (i in 1:17){
  temp <- cmsy2(year = Cod6a.data$year, catch = Cod6a.data$catch, resilience = NA,
                r.low = cmsy_input[i,1], r.hi =cmsy_input[i,2],
                stb.low = cmsy_input[i,3], stb.hi = cmsy_input[i,4],
                int.yr = int.yr,
                intb.low = cmsy_input[i,5], intb.hi = cmsy_input[i,6], 
                endb.low = cmsy_input[i,7], endb.hi = cmsy_input[i,8], verbose = T)
  
  cmsy_output$B_BMSY_start[i] <- temp$ref_ts$bbmsy[[1]]
  cmsy_output$B_BMSY_mid[i] <- temp$ref_ts$bbmsy[temp$ref_ts$year == int.yr]
  cmsy_output$B_BMSY_end[i] <- temp$ref_ts$bbmsy[length(temp$ref_ts$bbmsy)]
  
}


cmsy_output$Stock <- "Cod6a"
save(cmsy_output, file = "Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test5.RData")
timeOp5 = (proc.time() - ptm)/60

timeOp1
timeOp2
timeOp3
timeOp4
timeOp5

Rep2 <- c("rlow","rhi", "stblow", "stbhi",
          "intblow",  "intbhi", "endblow",  "endbhi")

load("Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test1.RData")
ups <- cmsy_output[c(1,3,5,7,9,11,13,15),]
ups$Rep <- Rep2

downs <- cmsy_output[c(2,4,6,8,10,12,14,16),]
downs$Rep <- Rep2

res <- ups
res$B_BMSY_start <- ups$B_BMSY_start - downs$B_BMSY_start
res$B_BMSY_mid <- ups$B_BMSY_mid - downs$B_BMSY_mid
res$B_BMSY_end <- ups$B_BMSY_end - downs$B_BMSY_end
res$trial <- 1
rep1 <- res

###
load("Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test2.RData")
ups <- cmsy_output[c(1,3,5,7,9,11,13,15),]
ups$Rep <- Rep2

downs <- cmsy_output[c(2,4,6,8,10,12,14,16),]
downs$Rep <- Rep2

res <- ups
res$B_BMSY_start <- ups$B_BMSY_start - downs$B_BMSY_start
res$B_BMSY_mid <- ups$B_BMSY_mid - downs$B_BMSY_mid
res$B_BMSY_end <- ups$B_BMSY_end - downs$B_BMSY_end
res$trial <- 2
rep2 <- res

###
load("Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test3.RData")
ups <- cmsy_output[c(1,3,5,7,9,11,13,15),]
ups$Rep <- Rep2

downs <- cmsy_output[c(2,4,6,8,10,12,14,16),]
downs$Rep <- Rep2

res <- ups
res$B_BMSY_start <- ups$B_BMSY_start - downs$B_BMSY_start
res$B_BMSY_mid <- ups$B_BMSY_mid - downs$B_BMSY_mid
res$B_BMSY_end <- ups$B_BMSY_end - downs$B_BMSY_end
res$trial <- 3
rep3 <- res

###
load("Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test4.RData")
ups <- cmsy_output[c(1,3,5,7,9,11,13,15),]
ups$Rep <- Rep2

downs <- cmsy_output[c(2,4,6,8,10,12,14,16),]
downs$Rep <- Rep2

res <- ups
res$B_BMSY_start <- ups$B_BMSY_start - downs$B_BMSY_start
res$B_BMSY_mid <- ups$B_BMSY_mid - downs$B_BMSY_mid
res$B_BMSY_end <- ups$B_BMSY_end - downs$B_BMSY_end
res$trial <- 4
rep4 <- res

###
load("Jacobian/CMSY2/CMSY2_Sensitivity_Cod6a_test5.RData")
ups <- cmsy_output[c(1,3,5,7,9,11,13,15),]
ups$Rep <- Rep2

downs <- cmsy_output[c(2,4,6,8,10,12,14,16),]
downs$Rep <- Rep2

res <- ups
res$B_BMSY_start <- ups$B_BMSY_start - downs$B_BMSY_start
res$B_BMSY_mid <- ups$B_BMSY_mid - downs$B_BMSY_mid
res$B_BMSY_end <- ups$B_BMSY_end - downs$B_BMSY_end
res$trial <- 5
rep5 <- res

all_res <- rbind(rep1, rep2, rep3, rep4, rep5)

test <- melt (all_res, id.vars=c("Rep", "Stock", "trial"))

prior.names = c("rlow", "rhi", "stblow", "stbhi", "intblow", "intbhi", "endblow", "endbhi")


test$Rep = ordered(test$Rep, levels = prior.names)

ggplot(test, aes(x= Rep, y = value))+
  geom_point()+
  facet_grid(~variable)
