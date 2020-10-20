###########################################
#### Jacobian comparison plots in a loop
###########################################
library (reshape)
library (ggplot2); theme_set(theme_bw())
library (gridExtra)
library (egg)
source ("jacobian prior functions.r")
# source ("jacobian plot functions.r")
source("Data/Ices ref points.r")
load("Data/Assessment Time Series Results.RData")

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

stocklist <- c("Cod6a", "Cod7ek", 
               "CodFaroe", "CodNS", 
               "Had6b","Had7bk" , "HadNS" , 
               "Ple7a", "Ple7hk" , "Sol7a", 
               "Sol7fg" , "Sol7hk" , "Sol2024",
               "Whg6a", "Whg7a" , 
               "Whg7bk" , "WhgNS")

Gate <- data.frame()
Bounds <- data.frame()

for (i in stocklist){
  
  data <- get((paste0 (i, ".data")), envir = .GlobalEnv) 
  
  
  ##################################################
  #### Priors for CMSY - used to plot biomass range points
  ct.raw <- data$catch# / 1000
  ct <- ma(data$catch)
  # Identify number of years and start/end years
  yr <- data$year # functions use this quantity
  nyr <- length(yr)
  start.yr <- min(yr)
  end.yr <- max(yr)
  
  pb.resilience = "Medium"
  pb.r.low = NA
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


  ## produce data table
  test <- res[res$Stock == i & res$variable == "B_BMSY" 
              & res$Method != "SPiCT", ]
  
  
  test$Prior_L[test$Year == start.yr] <- startbio[1]*2
  test$Prior_U[test$Year == start.yr] <- startbio[2]*2
  test$Prior_L[test$Year == int.yr] <- intbio[1]*2
  test$Prior_U[test$Year == int.yr] <- intbio[2]*2
  test$Prior_L[test$Year == end.yr] <- endbio[1]*2
  test$Prior_U[test$Year == end.yr] <- endbio[2]*2
  
  tempB <- data.frame(matrix(ncol=5,nrow=6, 
                             dimnames=list(NULL, c("Stock", "Year", "Start", "Mid", "End"))))
  tempB$Stock <- i
  tempB$Year[1:2] <- start.yr  
  tempB$Year[3:4] <- int.yr 
  tempB$Year[5:6] <- end.yr
  
  tempB$Start[1] <- startbio[1]*2
  tempB$Start[2] <- startbio[2]*2
  
  tempB$Mid[3] <- intbio[1]*2
  tempB$Mid[4] <- intbio[2]*2
  tempB$End[5] <- endbio[1]*2
  tempB$End[6] <- endbio[2]*2
  
  tempB2 <- merge(tempB,test[test$Method== "ICES", ], all.x =T)
  tempB2$bound_fit_start <- NA
  tempB2$bound_miss_start <- NA
  tempB2$bound_fit_mid <- NA
  tempB2$bound_miss_mid <- NA
  tempB2$bound_fit_end <- NA
  tempB2$bound_miss_end <- NA
  
  tempB2$value [is.na(tempB2$value)] <- test$value[1]
  tempB2$Prior_L[is.na(tempB2$Prior_L)] <- tempB2$Start[1]
  tempB2$Prior_U[is.na(tempB2$Prior_U)] <- tempB2$Start[2]
  
  tempB2$bound_fit_start[tempB2$value[1] > tempB2$Prior_L[1] && tempB2$value[1] < tempB2$Prior_U[1]] <- tempB2$Start
  tempB2$bound_miss_start[tempB2$value[1] < tempB2$Prior_L[1] || tempB2$value[1] > tempB2$Prior_U[1]] <- tempB2$Start
  tempB2$bound_fit_mid[tempB2$value[3] > tempB2$Prior_L[3] && tempB2$value[3] < tempB2$Prior_U[3]] <- tempB2$Mid
  tempB2$bound_miss_mid[tempB2$value[3] < tempB2$Prior_L[3] || tempB2$value[3] > tempB2$Prior_U[3]] <- tempB2$Mid
  tempB2$bound_fit_end[tempB2$value[5] > tempB2$Prior_L[5] && tempB2$value[5] < tempB2$Prior_U[5]] <- tempB2$End
  tempB2$bound_miss_end[tempB2$value[5] < tempB2$Prior_L[5] || tempB2$value[5] > tempB2$Prior_U[5]] <- tempB2$End
  
  Bounds <- rbind(Bounds, tempB2)
  Gate <- rbind(Gate, test)
  
  
}



Gate$Zero <- 0 # use to make sure the y axis extends to 0

# pdf(paste0("CMSY_Gates.pdf"), width =  6.69291, height = 10)
# ggplot(Gate)+
#   geom_line(aes(x =Year, y = value, colour = Method))+
#   scale_color_manual(values=c("black", "#D55E00"))+
#   geom_point(x = Gate$Year, y = Gate$Prior_L)+
#   geom_point(x = Gate$Year, y = Gate$Prior_U)+
#   geom_blank(aes(x = Gate$Year, y = Gate$Zero))+
#   ylab("B/BMSY")+
#   facet_wrap(facets = Gate$Stock, ncol = 4, scales = "free_y")+
#   theme(legend.title = element_blank(), 
#         legend.position = "top",
#         axis.title.x = element_blank())
# dev.off()


pdf(paste0("CMSY_Gates_review.pdf"), width =  6.69291, height = 8)
ggplot(Gate)+
  geom_line(aes(x =Year, y = value, colour = Method))+
  scale_color_manual(values=c("black", "#D55E00"))+
  geom_hline(yintercept = 1, linetype = "dotted", alpha = 0.6, colour = "grey80")+
  geom_blank(aes(x = Year, y = Zero))+
  scale_y_continuous(breaks = seq(0,4,by = 0.5))+
  scale_x_continuous(breaks = seq(1960,2020,by = 10))+
  ylab(expression(B/B[MSY]))+
  theme(legend.title = element_blank(), 
        legend.position = "top",
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5 ))+
  geom_line(data = Bounds, aes(x = Year, y = bound_fit_start), colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_fit_mid),colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_fit_end), colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_miss_start), linetype = "dashed", colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_miss_mid), linetype = "dashed",colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_miss_end), linetype = "dashed", colour = "grey70")+
  facet_wrap(~Stock, ncol = 4, scales = "free_y")
dev.off()


#### Functions to specify y axis labels seperately
# Taken from https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/

Facet <- ggproto(
  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- list()
    if (!is.null(x_scale)) {
      scales$x <- plyr::rlply(max(layout$SCALE_X), x_scale$clone())
    }
    if (!is.null(y_scale)) {
      scales$y <- plyr::rlply(max(layout$SCALE_Y), y_scale$clone())
    }
    scales
  },
)


scale_override <- function(which, scale) {
  if(!is.numeric(which) || (length(which) != 1) || (which %% 1 != 0)) {
    stop("which must be an integer of length 1")
  }
  
  if(is.null(scale$aesthetics) || !any(c("x", "y") %in% scale$aesthetics)) {
    stop("scale must be an x or y position scale")
  }
  
  structure(list(which = which, scale = scale), class = "scale_override")
}

CustomFacetWrap <- ggproto(
  "CustomFacetWrap", FacetWrap,
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    # make the initial x, y scales list
    scales <- ggproto_parent(FacetWrap, self)$init_scales(layout, x_scale, y_scale, params)
    
    if(is.null(params$scale_overrides)) return(scales)
    
    max_scale_x <- length(scales$x)
    max_scale_y <- length(scales$y)
    
    # ... do some modification of the scales$x and scales$y here based on params$scale_overrides
    for(scale_override in params$scale_overrides) {
      which <- scale_override$which
      scale <- scale_override$scale
      
      if("x" %in% scale$aesthetics) {
        if(!is.null(scales$x)) {
          if(which < 0 || which > max_scale_x) stop("Invalid index of x scale: ", which)
          scales$x[[which]] <- scale$clone()
        }
      } else if("y" %in% scale$aesthetics) {
        if(!is.null(scales$y)) {
          if(which < 0 || which > max_scale_y) stop("Invalid index of y scale: ", which)
          scales$y[[which]] <- scale$clone()
        }
      } else {
        stop("Invalid scale")
      }
    }
    
    # return scales
    scales
  }
)

facet_wrap_custom <- function(..., scale_overrides = NULL) {
  # take advantage of the sanitizing that happens in facet_wrap
  facet_super <- facet_wrap(...)
  
  # sanitize scale overrides
  if(inherits(scale_overrides, "scale_override")) {
    scale_overrides <- list(scale_overrides)
  } else if(!is.list(scale_overrides) || 
            !all(vapply(scale_overrides, inherits, "scale_override", FUN.VALUE = logical(1)))) {
    stop("scale_overrides must be a scale_override object or a list of scale_override objects")
  }
  
  facet_super$params$scale_overrides <- scale_overrides
  
  ggproto(NULL, CustomFacetWrap,
          shrink = facet_super$shrink,
          params = facet_super$params
  )
}

p_annoying_x_scale +
  facet_wrap_custom(~facet_name, scales = "free", ncol = 4, scale_overrides = list(
    scale_override(1, scale_x_continuous(breaks = c(5750, 5900))),
    scale_override(6, scale_x_continuous(breaks = c(17800, 17900)))
  ))


#### and the plot
pdf(paste0("CMSY_Gates_review.pdf"), width =  6.69291, height = 8)
ggplot(Gate)+
  geom_line(aes(x =Year, y = value, colour = Method))+
  scale_color_manual(values=c("black", "#D55E00"))+
  geom_hline(yintercept = 1, linetype = "dotted", alpha = 0.6, colour = "grey80")+
  geom_blank(aes(x = Year, y = Zero))+
  scale_y_continuous(breaks = seq(0,4,by = 0.5))+
  scale_x_continuous(breaks = seq(1960,2020,by = 10))+
  ylab(expression(B/B[MSY]))+
  theme(legend.title = element_blank(), 
        legend.position = "top",
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5 ))+
  geom_line(data = Bounds, aes(x = Year, y = bound_fit_start), colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_fit_mid),colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_fit_end), colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_miss_start), linetype = "dashed", colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_miss_mid), linetype = "dashed",colour = "grey70")+
  geom_line(data = Bounds, aes(x = Year, y = bound_miss_end), linetype = "dashed", colour = "grey70")+
  facet_wrap_custom(~Stock, ncol = 4, scales = "free_y", scale_overrides = list(
    scale_override(6, scale_y_continuous(breaks = seq(0,4,by = 1)))))
dev.off()

  