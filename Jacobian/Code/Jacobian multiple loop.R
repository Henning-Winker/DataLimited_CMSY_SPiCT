options(scipen = 999)

Cod6a.data = read.csv("Data/Cod_6_a/Cod.6a.Data.csv")
Cod7a.data = read.csv("Data/Cod_7_a/Cod.7a.Data.csv")
Cod7ek.data = read.csv("Data/Cod_7_ek/Cod.7ek.Data.csv")
CodFaroe.data = read.csv("Data/Cod_Faroe/Cod.Faroe.Data.csv")
CodNS.data = read.csv("Data/Cod_NS/Cod.NS.Data.csv")
Had6b.data = read.csv("Data/Had_6_b/Had.6b.Data.csv")
Had7bk.data = read.csv("Data/Had_7_bk/Had.7bk.Data.csv")
HadNS.data = read.csv("Data/Had_NS/Had.NS.Data.csv")
Ple7a.data = read.csv("Data/Ple_7_a/Ple.7a.Data.csv")
Ple7fg.data = read.csv("Data/Ple_7_fg/Ple.7fg.Data.csv")
Ple7hk.data = read.csv("Data/Ple_7_hk/Ple.7hk.Data.csv")
Sol7a.data = read.csv("Data/Sol_7_a/Sol.7a.Data.csv")
Sol7fg.data = read.csv("Data/Sol_7_fg/Sol.7fg.Data.csv")
Sol7hk.data = read.csv("Data/Sol_7_hk/Sol.7hk.Data.csv")
Sol2024.data = read.csv("Data/Sol_2024/Sol.2024.Data.csv")
Whg6a.data = read.csv("Data/Whg_6_a/Whg.6a.Data.csv")
Whg7a.data = read.csv("Data/Whg_7_a/Whg.7a.Data.csv")
Whg7bk.data = read.csv("Data/Whg_7_bk/Whg.7bk.Data.csv")
WhgNS.data = read.csv("Data/Whg_NS/Whg.NS.Data.csv")

stocklist <- c("Cod6a", "Cod7a" , "Cod7ek", 
               "CodFaroe", "CodNS",
               "Had6b" , "Had7bk" , "HadNS" , 
               "Ple7a", "Ple7fg", 
               "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")


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

#########################################################
#### functions end
#########################################################


####Jacobian loop

for (i in stocklist){
  
  data = get(paste0(i, ".data"))

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

## start it where we start the main cmsy run
start_theta <- c(rl = resilience("medium")[1], ru = resilience("medium")[2],
                 kl = start.k[1], ku = start.k[2],
                 p0l = startbio[1], p0u = startbio[2],
                 pMl = intbio[1], pMu = intbio[2],
                 pTl = endbio[1], pTu = endbio[2])

##### run the jacobian:
Ja <- nl.jacobian(x0 = start_theta, fn = get_cmsy_bbmsy, heps = 0.001, reps = 5e5)
Jb <- nl.jacobian(x0 = start_theta, fn = get_cmsy_bbmsy, heps = 0.001, reps = 1e6)
Jc <- nl.jacobian(x0 = start_theta, fn = get_cmsy_bbmsy, heps = 0.001, reps = 1e6)
Jd <- nl.jacobian(x0 = start_theta, fn = get_cmsy_bbmsy, heps = 0.001, reps = 1e6)
Je <- nl.jacobian(x0 = start_theta, fn = get_cmsy_bbmsy, heps = 0.001, reps = 1e6)

rownames(Ja) <- c("Start", "Mid", "End")
colnames(Ja) <- names(start_theta)
rownames(Jb) <- c("Start", "Mid", "End")
colnames(Jb) <- names(start_theta)
rownames(Jc) <- c("Start", "Mid", "End")
colnames(Jc) <- names(start_theta)
rownames(Jd) <- c("Start", "Mid", "End")
colnames(Jd) <- names(start_theta)
rownames(Je) <- c("Start", "Mid", "End")
colnames(Je) <- names(start_theta)

Jla <- melt(Ja[, ncol(Jb):1])
Jlb <- melt(Jb[, ncol(Jb):1])
Jlc <- melt(Jc[, ncol(Jc):1])
Jld <- melt(Jd[, ncol(Jd):1])
Jle <- melt(Je[, ncol(Je):1])


Jla$rep = "a"
Jlb$rep = "b"
Jlc$rep = "c"
Jld$rep = "d"
Jle$rep = "e"

Res <- rbind (Jla, Jlb, Jlc, Jld, Jle)
Res$Vars = as.factor(paste0(Res$Var1, "_", Res$Var2))

# adjust order of factors for plot
lab_start = paste0("Start_",names(start_theta))
lab_mid = paste0("Mid_",names(start_theta))
lab_end = paste0("End_",names(start_theta))
lab_order = c(lab_start, lab_mid, lab_end)
Res$Vars =  ordered(Res$Vars, levels = lab_order)

ggplot (Res)+
  geom_point(aes(x = Vars, y = value))+
  theme(axis.text.x = element_text(angle = 90))

png(paste0("Outputs\\Jacobian\\jacobian_multiple_", i, ".png"))
myplot <-ggplot (Res)+
  geom_point(aes(x = Vars, y = value))+
  theme(axis.text.x = element_text(angle = 90))
print(myplot)
dev.off()
}
