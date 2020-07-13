library(datalimited2)
library(reshape2)
library(ggplot2)

source ("Jacobian/jacobian_prior_funcs.r")

data <- read.csv("Data/Cod_6_a/Cod.6a.Data.csv")
stock <- "Cod6a"

#### set up priors using the seperate functions ####

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
start_prior <- c(rl = pb.r.low, ru = pb.r.hi,
                 #                 kl = start.k[1], ku = start.k[2],
                 p0l = startbio[1], p0u = startbio[2],
                 pMl = intbio[1], pMu = intbio[2],
                 pTl = endbio[1], pTu = endbio[2])

#### set up the sensitivity options

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

# names for output structure
Rep2 <- c("rlow","rhi", "stblow", "stbhi",
          "intblow",  "intbhi", "endblow",  "endbhi")

#### rep 1 ####
no.reps <- 5
all_res <- data.frame(matrix(ncol=6,nrow=0, 
                             dimnames=list(NULL, c("Rep", "B_BMSY_start", "B_BMSY_mid", "B_BMSY_end", "Stock", "trial"))))


for (i in no.reps){

cmsy_output <- data.frame(Rep = rep_names, B_BMSY_start = NA,
                          B_BMSY_mid = NA, B_BMSY_end = NA)

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


cmsy_output$Stock <- stock
### save raw output
save(cmsy_output, file = paste0("Jacobian/CMSY2/CMSY2_Sensitivity_", stock, "_rep", i, ".RData"))

#code to refine output and save
ups <- cmsy_output[c(1,3,5,7,9,11,13,15),]
ups$Rep <- Rep2

downs <- cmsy_output[c(2,4,6,8,10,12,14,16),]
downs$Rep <- Rep2

res <- ups
res$B_BMSY_start <- ups$B_BMSY_start - downs$B_BMSY_start
res$B_BMSY_mid <- ups$B_BMSY_mid - downs$B_BMSY_mid
res$B_BMSY_end <- ups$B_BMSY_end - downs$B_BMSY_end
res$trial <- i

all_res <- rbind(all_res, res)
}

save(all_res, file = paste0("Jacobian/CMSY2/CMSY2_Sensitivity_", stock, "_all.RData"))
