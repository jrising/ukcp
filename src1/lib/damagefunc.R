library(ggplot2)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## cdiff should be relative to the earliest period when we have
## appliable temperature (e.g., pre-industrial).

## impacts should be relative to some baseline period.

## In the end, we drop the intercept to make damages 0 at the baseline
## temperature.

stan.code.onefunc <- "
data {
  int<lower=0> N; // Scenarios x Years
  real cdiff[N];
  vector[N] impact;
  vector[N] impact_se;
}
transformed data {
  matrix[N, 3] preds;   // predictor matrix

  for (ii in 1:N) {
    preds[ii, 1] = 1;
    preds[ii, 2] = cdiff[ii];
    preds[ii, 3] = (cdiff[ii])^2;
  }
}
parameters {
  // Bayesian regression
  vector[3] coeff;
  real<lower=0> sigma;

  // True values
  vector[N] delta;
}
transformed parameters {
  vector[N] impact_true;
  impact_true = impact + impact_se .* delta;
}
model {
  // Fit with SEs
  impact_true ~ normal(preds * coeff, sigma);
  delta ~ normal(0, 1);
  sigma ~ cauchy(0, 2.5 * max(fabs(impact)));
}"

stan.code.onefunc.quadprior <- "
data {
  int<lower=0> N; // Scenarios x Years
  real cdiff[N];
  vector[N] impact;
  vector[N] impact_se;
  real quadmu;
  real quadse;
}
transformed data {
  matrix[N, 2] preds;   // predictor matrix
  vector[N] preds3;

  for (ii in 1:N) {
    preds[ii, 1] = 1;
    preds[ii, 2] = cdiff[ii];
    preds3[ii] = (cdiff[ii])^2;
  }
}
parameters {
  // Bayesian regression
  vector[2] coeff;
  real coeff3;
  real<lower=0> sigma;

  // True values
  vector[N] delta;
}
transformed parameters {
  vector[N] impact_true;
  impact_true = impact + impact_se .* delta;
}
model {
  // Fit with SEs
  impact_true ~ normal(preds * coeff + preds3 * coeff3, sigma);
  delta ~ normal(0, 1);
  sigma ~ cauchy(0, 2.5 * max(fabs(impact)));
  coeff3 ~ normal(quadmu, quadse);
}"

stan.code.twofuncs <- "
data {
  int<lower=0> N; // Scenarios x Years
  real cdiff[N];
  vector[N] impact1;
  vector[N] impact1_se;
  vector[N] impact2;
  vector[N] impact2_se;
}
transformed data {
  matrix[N, 3] preds;   // predictor matrix

  for (ii in 1:N) {
    preds[ii, 1] = 1;
    preds[ii, 2] = cdiff[ii];
    preds[ii, 3] = (cdiff[ii])^2;
  }
}
parameters {
  // Bayesian regression
  vector[3] coeff;
  real<lower=0> sigma;

  // True values
  vector[N] delta1;
  vector[N] delta2;
}
transformed parameters {
  vector[N] impact_true;
  vector[N] impact1_true;
  vector[N] impact2_true;

  impact1_true = impact1 + impact1_se .* delta1;
  impact2_true = impact2 + impact2_se .* delta2;
  impact_true = impact1_true + impact2_true;
}
model {
  // Fit with SEs
  impact_true ~ normal(preds * coeff, sigma);
  delta1 ~ normal(0, 1);
  delta2 ~ normal(0, 1);
}"

fit.damages <- function(cdiffs, impacts, ses, quadprior=F, draws=40000, force.spec='none', impact.draws=NULL) {
    if ((length(cdiffs) > 3 && force.spec == 'none') || force.spec == 'bayes') {
        stopifnot(is.null(impact.draws))

        stan.data <- list(N=length(cdiffs), cdiff=cdiffs,
                          impact=impacts, impact_se=ses)

        if (quadprior) {
            cdiff2 <- cdiffs^2
            mod <- lm(impacts ~ 0 + cdiff2)
            stan.data$quadmu <- mod$coeff
            stan.data$quadse <- 5 * sqrt(vcov(mod)[1, 1] + mean(ses)^2)
            fit <- stan(model_code=stan.code.onefunc.quadprior, data=stan.data, chains=8, iter=draws / 4)
        } else
            fit <- stan(model_code=stan.code.onefunc, data=stan.data, chains=8, iter=draws / 4)
        la <- extract(fit, permute=T)
        if (quadprior)
            la$coeff <- cbind(la$coeff, la$coeff3)
        la
    } else if ((length(cdiffs) == 3 && force.spec == 'none') || force.spec == 'quadols') {
        coeff <- matrix(NA, 0, 3)
        for (ii in 1:draws) {
            if (is.null(impact.draws))
                yy <- rnorm(length(cdiffs), impacts, ses)
            else
                yy <- impact.draws[ii,]
            cdiffs2 <- cdiffs^2
            mod <- lm(yy ~ cdiffs + cdiffs2)
            coeff <- rbind(coeff, mod$coeff)
        }
        list(coeff=coeff)
    } else if ((length(cdiffs) == 2 && force.spec == 'none') || force.spec == 'linols') {
        coeff <- matrix(NA, 0, 2)
        for (ii in 1:draws) {
            if (is.null(impact.draws))
                yy <- rnorm(length(cdiffs), impacts, ses)
            else
                yy <- impact.draws[ii,]
            mod <- lm(yy ~ cdiffs)
            coeff <- rbind(coeff, mod$coeff)
        }
        list(coeff=coeff)
    }
}

fit.damages.byregion <- function(regions, cdiffs, impacts, ses) {
    ddf <- data.frame() # damage functions
    pdf <- data.frame() # points
    allla <- list() # region -> la
    for (region in unique(regions)) {
        print(region)

        la <- fit.damages(cdiffs[regions == region], impacts[regions == region], ses[regions == region])

        pdf <- rbind(pdf, data.frame(region, T=cdiffs[regions == region], mu=impacts[regions == region] - mean(la$coeff[, 1]), ses[regions == region]))

        if (sum(regions == region) == 2) {
            rrddf <- data.frame(region=region, T=seq(0, 6, length.out=100))
            rrddf$mu <- sapply(rrddf$T, function(TT) mean(la$coeff[, 2] * TT))
            rrddf$ci25 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT, .25))
            rrddf$ci75 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT, .75))
        } else {
            rrddf <- data.frame(region=region, T=seq(0, 6, length.out=100))
            rrddf$mu <- sapply(rrddf$T, function(TT) mean(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2))
            rrddf$ci25 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .25))
            rrddf$ci75 <- sapply(rrddf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .75))
        }
        ddf <- rbind(ddf, rrddf)

        allla[[region]] <- la
    }

    list(ddf=ddf, pdf=pdf, allla=allla)
}

maybe.zeroneg <- function(T, values, zeroneg=-Inf, zeroneg.range=NA) {
    if (is.na(zeroneg.range))
        return(values)

    if (T > zeroneg & T < zeroneg + zeroneg.range)
        values[values < 0] <- values[values < 0] * (zeroneg.range - (T - zeroneg)) / zeroneg.range
    else if (T >= zeroneg + zeroneg.range)
        values[values < 0] <- 0
    values
}

get.plotinfo <- function(la, cdiffs, impacts, ses, zeroneg=-Inf, zeroneg.range=NA) {
    pdf <- data.frame(T=cdiffs, mu=impacts - mean(la$coeff[, 1]), ses)

    ddf <- data.frame(T=seq(0, 6, length.out=100))
    ddf$mu <- sapply(ddf$T, function(TT) mean(maybe.zeroneg(TT, la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, zeroneg, zeroneg.range)))
    ddf$ci25 <- sapply(ddf$T, function(TT) quantile(maybe.zeroneg(TT, la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, zeroneg, zeroneg.range), .25))
    ddf$ci75 <- sapply(ddf$T, function(TT) quantile(maybe.zeroneg(TT, la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, zeroneg, zeroneg.range), .75))

    list(ddf=ddf, pdf=pdf)
}

ggplot.damagefunc <- function(la, cdiffs, impacts, ses, zeroneg=-Inf, zeroneg.range=NA) {
    info <- get.plotinfo(la, cdiffs, impacts, ses, zeroneg=zeroneg, zeroneg.range=zeroneg.range)

    ggplot(info$ddf, aes(T, mu)) +
        geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
        geom_point(data=info$pdf) +
        theme_bw()
}
