library (corrplot)
source ("production_uncertainty_pb.r")

#### load SPICT results
spictfiles = list.files("SPICT_Results/Spict_n2r")
spictfileloc = "SPICT_Results/Spict_n2r/"
spictfiles = paste0(spictfileloc, spictfiles)
lapply(spictfiles, load, .GlobalEnv)

colp <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", 
                               "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
                               "#4393C3", "#2166AC", "#053061"))) ## intiutively think cold is negative and blue

res = Sol7a.spict

plot_production(res, plot_it = T)  
corrplot(cov2cor(res$cov.fixed), method = "ellipse", type = "upper", col = colp(200), addCoef.col = "black", diag = FALSE)

res$diag.cov.random

get.cov(res, q, n)
