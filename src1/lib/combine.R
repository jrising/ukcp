library(sn)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan.code <- "
data {
  int<lower=0> J;          // number of schools
  real y[J];               // estimated treatment effects
  real<lower=0> sigma[J];  // s.e. of effect estimates
  //real alpha[J];           // skew-normal shape
}
parameters {
  real mu;
  real<lower=0> tau;
  vector[J] eta;
}
transformed parameters {
  vector[J] theta;
  theta = mu + tau * eta;
}
model {
  target += normal_lpdf(eta | 0, 1);
  //target += skew_normal_lpdf(y | theta, sigma, alpha);
  target += normal_lpdf(y | theta, sigma);
}"


## mcs is matrix of NxK
hier.combine <- function(mcs) {
    ## params <- apply(mcs, 2, function(mc) {
    ##     if (all(mc == mc[1]))
    ##         return(c(mc[1], 0, 0))
    ##     mod <- selm(mc ~ 1)
    ##     summary(extractSECdistr(mod))@dp
    ## })
    ## params[2, params[2, ] == 0] <- mean(params[2, ])
    ## data <- list(J=ncol(mcs), y=params[1,], sigma=params[2,], alpha=params[3,])

    mus <- apply(mcs, 2, mean)
    sds <- apply(mcs, 2, sd)

    sds[sds == 0] <- mean(sds)

    data <- list(J=ncol(mcs), y=mus, sigma=sds)
    fit <- stan(model_code=stan.code, data=data, open_progress=F)
    la <- extract(fit, permute=T)

    newmcs <- matrix(NA, nrow(mcs), ncol(mcs))
    for (kk in 1:ncol(mcs)) {
        newmu <- mean(la$theta[, kk])
        newsd <- sd(la$theta[, kk])
        ## Should be closer to average, and tighter bounds
        if (abs(mus[kk] - mean(mus)) < abs(newmu - mean(mus)))
            newmu <- mus[kk]
        if (newsd > sds[kk])
            newsd <- sds[kk]
        newmcs[, kk] <- (mcs[, kk] - mus[kk]) * (newsd / sds[kk]) + newmu
    }

    choices <- sample.int(ncol(mcs), nrow(mcs), replace=T)
    sapply(1:nrow(mcs), function(ii) newmcs[ii, choices[ii]])
}

