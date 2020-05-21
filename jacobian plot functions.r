jacobian.plot.prep <- function(J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, prior.names){
  rownames(J1) <- c("Start", "Mid", "End")
  colnames(J1) <- prior.names
  rownames(J2) <- c("Start", "Mid", "End")
  colnames(J2) <- prior.names
  rownames(J3) <- c("Start", "Mid", "End")
  colnames(J3) <- prior.names
  rownames(J4) <- c("Start", "Mid", "End")
  colnames(J4) <- prior.names
  rownames(J5) <- c("Start", "Mid", "End")
  colnames(J5) <- prior.names
  rownames(J6) <- c("Start", "Mid", "End")
  colnames(J6) <- prior.names
  rownames(J7) <- c("Start", "Mid", "End")
  colnames(J7) <- prior.names
  rownames(J8) <- c("Start", "Mid", "End")
  colnames(J8) <- prior.names
  rownames(J9) <- c("Start", "Mid", "End")
  colnames(J9) <- prior.names
  rownames(J10) <- c("Start", "Mid", "End")
  colnames(J10) <- prior.names
  
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
  
  Res <- rbind (Jl1, Jl2, Jl3, Jl4, Jl5, Jl6, Jl7, Jl8, Jl9, Jl10)
  
  Res$X2 = ordered(Res$X2, levels = prior.names)
  Res$X1 = ordered(Res$X1, levels = c("Start", "Mid", "End"))
  Res
}
