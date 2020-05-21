library(nloptr)
library(reshape2)
library(ggplot2); theme_set(theme_bw())
library (datalimited)

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

get_cmsy_bbmsy <- function(theta, reps){
  ## theta is the here the vector of prior assumptions
  r_prior <- c(theta["rl"], theta["ru"])
  k_prior <- c(theta["kl"], theta["ku"]) 
  p0_prior <- c(theta["p0l"], theta["p0u"])
  pM_prior <- c(theta["pMl"], theta["pMu"])
  pT_prior <- c(theta["pTl"], theta["pTu"])
  ## run CMSY
  cmsy.res <- cmsy(yr = data$year, ct = data$catch, reps = reps, 
                   start_r = r_prior,
                   interyr_index = int.yr.num, 
                   interbio = pM_prior, 
                   # bio_step = 0.05,
                   start_k = k_prior,
                   startbio = p0_prior,
                   # sig_r = 0.05, 
                   revise_bounds = TRUE, 
                   finalbio = pT_prior)
  bbmsy_hat <- cmsy.res$bbmsy$bbmsy_q50
  res <- bbmsy_hat[c(1, round(length(bbmsy_hat)/2),length(bbmsy_hat))]
  names(res) <- c("BB0", "BBM", "BBT")
  return(res)
}


CodFaroe.data = read.csv("Data/Cod_Faroe/Cod.Faroe.Data.csv")
data <-CodFaroe.data

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


## runs the cmsy
# CodNS.dl <- cmsy(yr = data$year, ct = data$catch, reps = 1e6, 
#                  start_r = resilience("medium"),
#                  interyr_index = int.yr.num , 
#                  interbio = intbio, 
#                  # bio_step = 0.05,
#                  start_k = start.k,
#                  startbio = startbio,
#                  # sig_r = 0.05, 
#                  revise_bounds = TRUE, 
#                  finalbio = endbio)
# 
# ggplot(CodNS.dl$bbmsy, aes(year, bbmsy_q50)) + geom_line()  +
#   geom_ribbon(aes(ymin = bbmsy_q25, ymax = bbmsy_q75), alpha = 0.2) +
#   geom_ribbon(aes(ymin = bbmsy_q2.5, ymax = bbmsy_q97.5), alpha = 0.1) +
#   geom_hline(yintercept = 1, lty = 2) + theme_light()



## start it where we start the main cmsy run
start_theta <- c(rl = resilience("medium")[1], ru = resilience("medium")[2],
                 kl = start.k[1], ku = start.k[2],
                 p0l = startbio[1], p0u = startbio[2],
                 pMl = intbio[1], pMu = intbio[2],
                 pTl = endbio[1], pTu = endbio[2])

source("proportional_influence.r")

##### run the jacobian:
ptm <- proc.time()
J1 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)
timeOp = (proc.time() - ptm)/60
timeOp

J2 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)
J3 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)
J4 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)
J5 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)
J6 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)
J7 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)
J8 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)
J9 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)
J10 <- pinfl(x0 = start_theta, fn = get_cmsy_bbmsy, p = 0.1, reps = 2e6)

save (J1, J2, J3,J4,J5,J6,J7,J8,J9,J10, file = "Jacobian\\Results\\CodFaroe_Jacobian_Repeats.RData")



rownames(J1) <- c("Start", "Mid", "End")
colnames(J1) <- names(start_theta)
rownames(J2) <- c("Start", "Mid", "End")
colnames(J2) <- names(start_theta)
rownames(J3) <- c("Start", "Mid", "End")
colnames(J3) <- names(start_theta)
rownames(J4) <- c("Start", "Mid", "End")
colnames(J4) <- names(start_theta)
rownames(J5) <- c("Start", "Mid", "End")
colnames(J5) <- names(start_theta)
rownames(J6) <- c("Start", "Mid", "End")
colnames(J6) <- names(start_theta)
rownames(J7) <- c("Start", "Mid", "End")
colnames(J7) <- names(start_theta)
rownames(J8) <- c("Start", "Mid", "End")
colnames(J8) <- names(start_theta)
rownames(J9) <- c("Start", "Mid", "End")
colnames(J9) <- names(start_theta)
rownames(J10) <- c("Start", "Mid", "End")
colnames(J10) <- names(start_theta)



Jl1 <- melt(J1[, ncol(J1):1])
Jl2 <- melt(J2[, ncol(J2):1])
Jl3 <- melt(J3[, ncol(J3):1])
Jl4 <- melt(J4[, ncol(J4):1])
Jl5 <- melt(J5[, ncol(J5):1])
Jl6 <- melt(J6[, ncol(J6):1])
Jl7 <- melt(J7[, ncol(J7):1])
Jl8 <- melt(J8[, ncol(J8):1])
Jl9 <- melt(J9[, ncol(J9):1])
Jl10 <- melt(J10[, ncol(J10):1])


ggplot(Jl1, aes(x = Var1, y = Var2, fill = value)) +
  geom_raster() +
  geom_text(aes(label = round(value, 4))) +
  scale_fill_gradient2(low = "blue", mid = "white",
                       high = "red", midpoint = 0,
                       limits = c(-max(abs(J1)), max(abs(J1)))) +
  xlab("Biomass") +
  ylab("")


Res <- rbind (Jl1, Jl2, Jl3, Jl4, Jl5, Jl6, Jl7, Jl8, Jl9, Jl10)

lab_order = names(start_theta)
Res$Var2 =  ordered(Res$Var2, levels = lab_order)

png("Jacobian\\Plots\\jacobian_280120_CodFaroe.png")
ggplot(Res)+
  geom_point(aes(x = Var2, y = value))+
  facet_wrap(facets = Res$Var1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.2))
dev.off()
