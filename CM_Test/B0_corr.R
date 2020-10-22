##-------------------------------
## check out correlation with B0
## CM, PB: Thu Oct 22 2020
##
##-------------------------------
library(spict)
library(corrplot)


##stock <- "Cod6a_Spict"
##stock <- "Cod7a_Spict"
##stock <- "Cod7ek_Spict"
stock <- "Solfg_Spict"
load(paste0(stock, ".RData"))

res <- get(paste0(strsplit(stock, "_")[[1]][1], ".spict"))

## fixed effect only correlation we looked at previously
colp <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", 
            "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
            "#4393C3", "#2166AC", "#053061"))) ## intiutively think cold is negative and blue


corrplot(cov2cor(res$cov.fixed), method = "ellipse", type = "upper", col = colp(200), addCoef.col = "black", diag = FALSE)

## own full correlation (fixed and random effects)
precision <- sdreport(res$obj, getJointPrecision = TRUE)

all_cov <- solve(precision$jointPrecision)

pars <- names(res$opt$par)
pars <- pars[pars != "logn"]
idx <- which(colnames(all_cov) %in% pars)

## first logB
idxB0 <- which(colnames(all_cov) == "logB")[1]

idall <- c(idx, idxB0)

corr_B0 <- cov2cor(all_cov[idall, idall])
colnames(corr_B0)[colnames(corr_B0) == "logB"] <- "logB0"
rownames(corr_B0)[rownames(corr_B0) == "logB"] <- "logB0"

pdf(paste0(stock, "_B0_correlation.pdf"), height = 8, width = 8)
corrplot(corr_B0, method = "ellipse", type = "upper", col = colp(200), addCoef.col = "black", diag = FALSE)
dev.off()
