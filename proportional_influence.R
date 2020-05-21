##---------------------
## Proportional influence (slope over a multiplicative change in parameter)
## CM, PB: 28/01/2020
##---------------------

## using some of code from nloptr::nl.jacobian
pinfl <- function (x0, fn, p, units = "unscaled", ...) 
{
    ## p is a proportional change in the parameter so the values
    ## used for the slope are (1-p) * x0 and (1+p) * x0
    if (!is.numeric(x0) || length(x0) == 0) 
        stop("Argument 'x' must be a non-empty numeric vector.")
    fun <- match.fun(fn)
    fn <- function(x) fun(x, ...)
    n <- length(x0)
    m <- length(fn(x0))
    slope <- matrix(NA, m, n)
    pp <- numeric(n)
    for (i in 1:n) {
        pp[i] <- p
        if(units == "parameter"){
            slope[, i] <- (fn((1 + pp) * x0) - fn((1 - pp) * x0))/(2 * p * x0[i])
        }
        if(units == "unscaled"){
            slope[, i] <- (fn((1 + pp) * x0) - fn((1 - pp) * x0))
        }
        
        pp[i] <- 0
    }
    return(slope)
}
