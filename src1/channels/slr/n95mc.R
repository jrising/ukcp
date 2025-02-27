library(sn)

use.fit <- NA
use.pointest <- F

fit.skewnormal <- function(mu, ci5, ci95) {
    res <- optim(c(mu, max(abs(c(ci5, ci95) - mu)), 0), function(params) {
        delta <- params[3] / sqrt(1 + params[3]^2)
        mu.pred <- params[1] + abs(params[2]) * delta * sqrt(2 / pi)
        as.pred <- qsn(c(.05, .95), params[1], abs(params[2]), params[3])
        return((mu - mu.pred)^2 + sum((c(ci5, ci95) - as.pred)^2))
    })
    return(c(res$par[1], abs(res$par[2]), res$par[3]))
}

r.twoskew <- function(fit, par.twoskew, nn=1) {
    if (!is.na(use.fit))
        fit <- use.fit
    p0 <- (fit - 1)*3 #sample(c(0, 3), 1) <- maintain same fit throughout
    if (use.pointest)
        return(rep(par.twoskew[p0+1], nn))
    rsn(nn, par.twoskew[p0+1], par.twoskew[p0+2], par.twoskew[p0+3])
}
