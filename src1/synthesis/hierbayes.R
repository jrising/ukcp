setwd("~/Open Modeling Group Dropbox/UK Economic Risks/synthesis")

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

stan.code <- "
data {
  int<lower=0> J;          // number of schools
  real y[J];               // estimated treatment effects
  real<lower=0> sigma[J];  // s.e. of effect estimates
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
  target += normal_lpdf(y | theta, sigma);
}"

data <- list(J=2, y=c(0, 0), sigma=c(1, 1))
fit <- stan(model_code=stan.code, data=data, open_progress=F)

## results <- data.frame()
results <- read.csv("hierbayes-data.csv")
for (ii in nrow(results):1000) {
    if (ii %% 100 == 0)
        write.csv(results, "hierbayes-data.csv", row.names=F)
    y2 <- rexp(1)
    sigma2 <- rexp(1)
    data <- list(J=2, y=c(0, y2), sigma=c(1, sigma2))
    fit <- stan(fit=fit, data=data, open_progress=F, verbose=F)
    la <- extract(fit, permute=T)
    results <- rbind(results, data.frame(y2, sigma2, tau=mean(la$tau), tause=sd(la$tau)))
}
write.csv(results, "hierbayes-data.csv", row.names=F)

summary(lm(tau ~ y2 + sigma2, data=results))
summary(lm(log(tau) ~ log(y2) + log(sigma2), data=results))
summary(lm(log(tau) ~ y2 + log(sigma2), data=results))
summary(lm(log(tau) ~ y2 + log(sigma2) + y2 : log(sigma2), data=results))



data <- results
data$y22 <- data$y2^2
data$logsigma22 <- log(data$sigma2)^2

mod <- lm(log(tau) ~ poly(y2, 2) + poly(log(sigma2), 2) + y2 : log(sigma2) + y22 : log(sigma2) + y2 : logsigma22 + y22 : logsigma22, data=data)
library(Rcmdr)

library(MASS, pos=20)
stepwise(mod, direction='backward/forward', criterion='BIC')

summary(lm(log(tau) ~ poly(y2, 2) + poly(log(sigma2), 2) + y2 : log(sigma2) + y2 : logsigma22, data=data))

library(ggplot2)

ggplot(results, aes(y2, sigma2, colour=tau)) +
    geom_point() + scale_x_log10(limits=c(.1, 10)) + scale_y_log10(limits=c(.1, 10)) +
    scale_colour_gradient(trans='log10')

ggplot(results, aes(y2, tau)) +
    geom_point() + geom_smooth() + scale_y_log10()

ggplot(results, aes(sigma2, tau)) +
    geom_point() + geom_smooth() + scale_y_log10() + scale_x_log10()
