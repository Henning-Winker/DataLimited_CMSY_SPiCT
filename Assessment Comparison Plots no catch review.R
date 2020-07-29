#### Comparison plots for CMSY, SPICT and ICES assessments
library (ggplot2); theme_set(theme_bw())
library(spict)
library (reshape)
library (gridExtra)
library (egg)

stocklist <- c("Cod6a", "Cod7ek", 
               "CodFaroe", "CodNS", 
               "Had6b","Had7bk" , "HadNS" , 
               "Ple7a", "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")

Cod6a.data = read.csv("Data/Cod_6_a/Cod.6a.Data.csv")
# Cod7a.data = read.csv("Data/Cod_7_a/Cod.7a.Data.csv")
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

#### ICES Ref Points
# source("Data/Ices ref points2.r")
# 
# 
# Cod6a_Ices <- ICES[ICES$Stock== "Cod6a", ]
# 
# ICES <- ICES[ICES$Purpose == "Advice", ]
# ICES <- ICES[ICES$Year < 2018, ]
# ICES.df <- data.frame (Stock = ICES$Stock, 
#                        Year = ICES$Year,
#                     B_BMSY = ICES$B_BMSY,
#                     F_FMSY = ICES$F_FMSY, 
#                     Method = "ICES")
# 
# Cod6a_Ices <- Cod6a_Ices[Cod6a_Ices$Year < 2018, ]
# Cod6a.df <- data.frame (Stock = Cod6a_Ices$Stock, 
#                         Year = Cod6a_Ices$Year,
#                         B_BMSY = Cod6a_Ices$B_BMSY,
#                         F_FMSY = Cod6a_Ices$F_FMSY, 
#                         Method = "ICES")
# 
# ICES.df <- rbind(ICES.df, Cod6a.df)
#   
# # Whg6a.ices <- read.csv("Data/ICES assessment PB Whg6a.csv") 
# # ICES.df <- rbind (ICES.df, Whg6a.ices)
# 
# 
# #### load CMSY results
# cmsyfiles = list.files("CMSY_Results")
# cmsyfileloc = "CMSY_Results\\"
# cmsyfiles = paste0(cmsyfileloc, cmsyfiles)
# lapply(cmsyfiles, load, .GlobalEnv)
# 
# cmsy.df = data.frame()
# 
# for (i in stocklist){
# cmsy <- get((paste0 (i, ".cmsy")), envir = .GlobalEnv) 
# cmsy.temp <- data.frame (Stock = i,
#                        Year = cmsy$ref_ts$year,
#                       B_BMSY = cmsy$ref_ts$bbmsy,
#                       F_FMSY = cmsy$ref_ts$ffmsy,
#                       Method = "CMSY")
# cmsy.df = rbind(cmsy.df, cmsy.temp)
# }
# 
# #### load SPICT results
# spictfiles = list.files("SPICT_Results/Spict_n2r")
# spictfileloc = "SPICT_Results/Spict_n2r/"
# spictfiles = paste0(spictfileloc, spictfiles)
# lapply(spictfiles, load, .GlobalEnv)
# 
# spict.df = data.frame()
# 
# for (i in stocklist){
#   spict <- get((paste0 (i, ".spict")), envir = .GlobalEnv) 
#   spict.temp <- data.frame (Stock = i,
#                            Year =   rownames(get.par('logBBmsy', spict, exp=TRUE)),
#                            B_BMSY = get.par('logBBmsy', spict, exp=TRUE)[, "est"],
#                            F_FMSY = get.par('logFFmsy', spict, exp=TRUE)[, "est"],
#                            Method = "SPiCT")
#   
#   rownames(get.par('logBBmsy', spict, exp=TRUE))
#   
#   spict.df = rbind(spict.df, spict.temp)
# }
# 
# spict.df = spict.df[spict.df$Year %in% 1900:2018, ]
#  
# res = rbind(ICES.df, cmsy.df, spict.df)
# res$Year = as.numeric(res$Year)
# 
# res = melt(res, id.vars = c("Stock", "Year", "Method"))


load ("Data/Assessment Time Series Results.RData")

# save(res, file = "Data/Assessment Time Series Results.RData")


res$Stock <- factor(res$Stock, levels = c("Cod6a", "Cod7ek", 
                               "CodFaroe", "CodNS", 
                               "Had6b","Had7bk" , "HadNS" , 
                               "Ple7a", "Ple7hk" , "Sol7a", 
                               "Sol7fg" , "Sol7hk" , "Sol2024",
                               "Whg6a", "Whg7a" , 
                               "Whg7bk" , "WhgNS"))


levels(res$variable) <-c('B/B[MSY]',  'F/F[MSY]') 

pdf("Timeseries Plots/timeseries1_July.pdf", width = 8, height = 12)
ggplot (res[res$Stock %in% stocklist[1:6], ])+
  geom_line (aes(x= Year, y = value, colour = Method))+
  scale_color_manual(values=c("black", "#D55E00", "#56B4E9"))+
  facet_grid(vars(Stock), vars(variable), scales = "free", labeller = label_parsed)+
  scale_x_continuous(breaks = seq(1960,2020,by = 10))+
  geom_hline (yintercept = 1, linetype = "dashed")+
  theme (axis.title = element_blank(),
         legend.title = element_blank(),
         legend.position = "bottom",
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0.2, "lines"))
dev.off()

pdf("Timeseries Plots/timeseries2_July.pdf", width = 8, height = 12)
ggplot (res[res$Stock %in% stocklist[7:12], ])+
  geom_line (aes(x= Year, y = value, colour = Method))+
  scale_color_manual(values=c("black", "#D55E00", "#56B4E9"))+
  facet_grid(vars(Stock), vars(variable), scales = "free", labeller = label_parsed)+
  scale_x_continuous(breaks = seq(1960,2020,by = 10))+
  geom_hline (yintercept = 1, linetype = "dashed")+
  theme (axis.title = element_blank(),
         legend.title = element_blank(),
         legend.position = "bottom",
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0.2, "lines"))
dev.off()

res$Stock = factor(res$Stock, levels= stocklist)

pdf("Timeseries Plots/timeseries3_July.pdf", width = 8, height = 12)
ggplot (res[res$Stock %in% stocklist[13:17], ])+
  geom_line (aes(x= Year, y = value, colour = Method))+
  scale_color_manual(values=c("black", "#D55E00", "#56B4E9"))+
  facet_grid(vars(Stock), vars(variable), scales = "free", labeller = label_parsed)+
  scale_x_continuous(breaks = seq(1960,2020,by = 10))+
  geom_hline (yintercept = 1, linetype = "dashed")+
  theme (axis.title = element_blank(),
         legend.title = element_blank(),
         legend.position = "bottom",
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0.2, "lines"))
dev.off()

##### Highlighted stocks for detailed discussion

imp.stocks <- c("Cod7ek", "CodNS", "Had6b", "Sol7a", "Sol7fg", "WhgNS")

pdf("Timeseries Plots/timeseries_important_July.pdf", width =  6.69291, height = 12)
ggplot (res[res$Stock %in% imp.stocks, ])+
  geom_line (aes(x= Year, y = value, colour = Method))+
  scale_color_manual(values=c("black", "#D55E00", "#56B4E9"))+
  facet_grid(vars(Stock), vars(variable), scales = "free", labeller = label_parsed)+
  scale_x_continuous(breaks = seq(1960,2020,by = 10))+
  geom_hline (yintercept = 1, linetype = "dashed")+
  theme (axis.title = element_blank(),
         legend.title = element_blank(),
         legend.position = "bottom",
         panel.spacing.x=unit(0, "lines"),
         panel.spacing.y=unit(0.2, "lines"))
dev.off()
