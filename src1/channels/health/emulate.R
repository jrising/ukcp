setwd("~/projects/ukcp/health")

library(readxl)

df <- read_excel("41598_2021_99156_MOESM2_ESM.xlsx", skip=1)
ukdf <- subset(df, ISO3 == "GBR")
ukdf2 <- data.frame(rcp=rep(c(unique(ukdf$`Climate Scenario`), ukdf$`Climate Scenario`), 3),
                    years=rep(c(rep("2001–2020", length(unique(ukdf$`Climate Scenario`))), ukdf$Year), 3),
                    tdiff=rep(c(rep(0, length(unique(ukdf$`Climate Scenario`))), ukdf$`Projected Temperature Increase`), 3),
                    gdppc=rep(c(rep(ukdf$`GDP Per_Capita_PPP`[1], length(unique(ukdf$`Climate Scenario`))),
                                ukdf$`Future GDP Per_Capita (SSP3)`), 3),
                    direction=rep(c("Cold-Related Mortality", "Heat-Related Mortality", "Heat-Related Mortality"),
                                  each=nrow(ukdf) + length(unique(ukdf$`Climate Scenario`))),
                    adapt=rep(c("Cold Model 3", "Without Income Adjustment",
                                "With Income Adjustment (SSP3 Income Growth)"),
                              each=nrow(ukdf) + length(unique(ukdf$`Climate Scenario`))),
                    mu=c(rep(0, length(unique(ukdf$`Climate Scenario`))),
                         ukdf$`Projected Change Cold-Related Mortality (Cold Model 3, % change mortality rate)`,
                         rep(0, length(unique(ukdf$`Climate Scenario`))),
                         ukdf$`Projected Change Heat-Related Mortality (Heat Model 4, % change mortality rate)...12`,
                         rep(0, length(unique(ukdf$`Climate Scenario`))),
                         ukdf$`Projected Change Heat-Related Mortality (Heat Model 4, % change mortality rate)...16`),
                    se=c(rep(0, length(unique(ukdf$`Climate Scenario`))),
                         ukdf$`Projected Change Cold-Related Mortality (standard error)`,
                         rep(0, length(unique(ukdf$`Climate Scenario`))),
                         ukdf$`Projected Change in Heat Realted Mortality (standard error)...13`,
                         rep(0, length(unique(ukdf$`Climate Scenario`))),
                         ukdf$`Projected Change in Heat Realted Mortality (standard error)...17`))
ukdf2$year <- (as.numeric(sapply(ukdf2$years, function(x) substring(x, 1, 4))) + as.numeric(sapply(ukdf2$years, function(x) substring(x, 6, 9))) + 1) / 2

library(ggplot2)

ggplot(ukdf2, aes(year, mu, colour=rcp)) +
    facet_wrap(~ direction + adapt, ncol=1) +
    geom_line()

ggplot(ukdf2, aes(tdiff, mu, group=rcp)) +
    facet_wrap(~ direction + adapt, ncol=1, scale="free_y") +
    geom_line(aes(colour=rcp)) + geom_ribbon(aes(ymin=mu - 1.96*se, ymax=mu + 1.96*se), alpha=.5)

library(dplyr)

ukdf3 <- ukdf2 %>% group_by(rcp, years, tdiff, gdppc) %>%
    summarize(cold.mu=mu[adapt == "Cold Model 3"],
              cold.se=se[adapt == "Cold Model 3"],
              heat.mu=mu[adapt == "Without Income Adjustment"],
              heat.se=se[adapt == "Without Income Adjustment"],
              hinc.mu=mu[adapt == "With Income Adjustment (SSP3 Income Growth)"],
              hinc.se=se[adapt == "With Income Adjustment (SSP3 Income Growth)"])

## Step 1: How much did UK warm from 1970-2000 to 2001-2020?

lmst <- read.csv("../worldclim/uk-lmst.csv")
lmst$tdiff <- c(0, lmst$lmst[-1] - lmst$lmst[1])
tdiff.2001.2020 <- mean(lmst$tdiff[lmst$period == '2021-2040'] / 2) # Average with 0 to get 2000-2020

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

## Version 1: Just fit quadratic, relative to pre-industiral

stan.code <- "
data {
  int<lower=0> N; // Scenarios x Years
  real baseline_tdiff;
  real tdiff[N];
  vector[N] impact;
  vector[N] impact_se;
}
transformed data {
  matrix[N, 3] preds;   // predictor matrix

  for (ii in 1:N) {
    preds[ii, 1] = 1;
    preds[ii, 2] = baseline_tdiff + tdiff[ii];
    preds[ii, 3] = (baseline_tdiff + tdiff[ii])^2;
  }
}
parameters {
  // Bayesian regression
  vector[3] coeff;
  real<lower=0> sigma;
}
model {
  // Fit with SEs
  impact ~ normal(preds * coeff, sigma + impact_se);
}"

stan.data <- list(N=nrow(ukdf3), baseline_tdiff=tdiff.2001.2020,
                  tdiff=ukdf3$tdiff, impact=ukdf3$cold.mu + ukdf3$heat.mu,
                  impact_se=sqrt(ukdf3$cold.se^2 + ukdf3$heat.se^2))

## Version 2: properly handle uncertainty

stan.code <- "
data {
  int<lower=0> N; // Scenarios x Years
  real baseline_tdiff;
  real tdiff[N];
  vector[N] impact1;
  vector[N] impact1_se;
  vector[N] impact2;
  vector[N] impact2_se;
}
transformed data {
  matrix[N, 3] preds;   // predictor matrix

  for (ii in 1:N) {
    preds[ii, 1] = 1;
    preds[ii, 2] = baseline_tdiff + tdiff[ii];
    preds[ii, 3] = (baseline_tdiff + tdiff[ii])^2;
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

stan.data <- list(N=nrow(ukdf3), baseline_tdiff=tdiff.2001.2020,
                  tdiff=ukdf3$tdiff, impact1=ukdf3$cold.mu, impact2=ukdf3$heat.mu,
                  impact1_se=ukdf3$cold.se, impact2_se=ukdf3$heat.se)

fit <- stan(model_code=stan.code, data=stan.data, chains=8, iter=10000)

la <- extract(fit, permute=T)

pdf <- data.frame(T=seq(0, 6, length.out=100))
pdf$mu <- sapply(pdf$T, function(TT) mean(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2))
pdf$ci25 <- sapply(pdf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .25))
pdf$ci75 <- sapply(pdf$T, function(TT) quantile(la$coeff[, 2] * TT + la$coeff[, 3] * TT^2, .75))

library(ggplot2)

ggplot(pdf, aes(T, mu)) +
    geom_line() + geom_ribbon(aes(ymin=ci25, ymax=ci75), alpha=.5) +
    geom_point(data=ukdf3, aes(x=tdiff + tdiff.2001.2020, y=cold.mu + heat.mu - mean(la$coeff[, 1]))) +
    scale_x_continuous("Difference in temperature from 1970-2000 (C)", expand=c(0, 0)) + coord_cartesian(ylim=c(-1, 6)) +
    theme_bw() + ylab("Change in mortality rate (%)")

save(la, file="Bressler et al la.RData")

## load("Bressler et al la.RData")

## Step 2: Construct regional values

library(PBSmapping)

shp <- importShapefile("../gadm36_GBR_shp/gadm36_GBR_0.shp")

library(raster)

temps <- raster("../worldclim/uk-future/uk_bioc_BCC-CSM2-MR_ssp370_2081-2100.nc")
temps.df <- as.data.frame(temps)

pops <- raster("../socioeconomics/LandScan Global 2019/lspop2019/w001001.adf")
pops2 <- aggregate(crop(pops, bbox(temps)), fact=20, fun=sum)

pops2.df <- as.data.frame(pops2, xy=T)
pops2.df$EID <- 1:nrow(pops2.df)
names(pops2.df)[1:2] <- c('X', 'Y')
events <- as.EventData(pops2.df, projection=1)

found <- findPolys(events, shp, maxRows=nrow(events))
pops2.df$is.uk <- F
pops2.df$is.uk[found$EID] <- T

mortrate <- 9 / 1000
totpop <- sum(pops2.df$w001001[found$EID], na.rm=T)
avgtdiff <- lmst$tdiff[lmst$period == '2081-2100' & lmst$model == 'BCC-CSM2-MR' & lmst$scenario == 'ssp370']

ukdeaths <- totpop * mortrate * mean(la$coeff[, 2] * avgtdiff + la$coeff[, 3] * avgtdiff^2) / 100

temps0 <- raster("../worldclim/uk-present/uk_bio.nc", band=1)
temps0.df <- as.data.frame(temps0)

griddeaths <- sapply(1:nrow(temps.df), function(ii) pops2.df$w001001[ii] * mortrate * mean(la$coeff[, 2] * (temps.df$variable[ii] - temps0.df$variable[ii]) + la$coeff[, 3] * (temps.df$variable[ii] - temps0.df$variable[ii])^2) / 100)

griddeath.total <- sum(griddeaths[found$EID], na.rm=T)

## Dang. Opposite sign. No scaling fix. Let's do simple adjusting for now.

offset <- (griddeath.total - ukdeaths) / nrow(found)

griddeaths2 <- griddeaths - offset

## Translate back into death rate

pops2.df$add.deaths <- griddeaths2

library(ggplot2)
library(scales)

ggplot(pops2.df, aes(X, Y, fill=add.deaths)) +
    geom_raster() + scale_fill_gradient2("Additional\nDeaths:", low=muted("blue"), high=muted("red")) +
    theme_bw() + scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous(NULL, expand=c(0, 0))
ggsave("Bressler et al eoc.png", width=5.5, height=6)

## Later version of projection

setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/project.R")

load("channels/health/Bressler et al la.RData")

results <- grid.project(la, 1970, 2001)

write.csv(results, "channels/health/Bressleretal-project.csv", row.names=F)

## Version 2: apply quadratic to regions

stan.code <- "
data {
  int<lower=0> N; // Scenarios x Years
  real tdiff[N];
  vector[N] impact;
  vector[N] impact_se;

  int<lower=0> R; // Regions
  real region_tdiff[R, N];
  real<lower=0> region_weight[R, N];
}
transformed data {
  matrix[N, 2] preds;   // predictor matrix
  matrix[N, 2] region_preds[R];   // predictor matrix

  for (ii in 1:N) {
    preds[ii, 1] = tdiff[ii];
    preds[ii, 2] = tdiff[ii]^2;
  }

  for (rr in 1:R) {
    for (ii in 1:N) {
      region_preds[ii, 1] = region_tdiff[ii];
      region_preds[ii, 2] = region_tdiff[ii]^2;
    }
  }
}
parameters {
  // Bayesian regression
  vector[2] coeff;
  real<lower=0> sigma;
}
model {
  // Fit with SEs
  impact ~ normal(preds * coeff, sigma + impact_se);

  // Apply for each region

}"

stan.data <- list(N=nrow(ukdf3), tdiff=ukdf3$tdiff, impact=ukdf3$cold.mu, impact_se=ukdf3$cold.se)

fit <- stan(model_code=stan.code, data=stan.data)






  real<lower=0> tau;

  real region_alpha[R];
  real region_beta[R];

  real<lower=0> eta;


  region_alpha ~ normal(0, sigma);
  region_beta ~ normal(1, tau);

  region_alpha * region_tdiff + region_beta *



    ## NOTE: Can't easily say how much UK warmed to 1970-2000, so

## ## Step 1: Calculate preindustrial temperature change to 2001-2020 and 1970-2000

## fairs <- read.csv("../gsat/ssps_26_70_ukproject_allmembers.csv")

## fairs.2001.2020 <- fairs %>% subset(year >= 2001 & year <= 2020) %>% group_by(run_id, scenario) %>% summarize(value=mean(value))
## preind.tdiff.2001.2020 <- mean(fairs.2001.2020$value)

## fairs.1970.2000 <- fairs %>% subset(year >= 1970 & year <= 2000) %>% group_by(run_id, scenario) %>% summarize(value=mean(value))
## preind.tdiff.1970.2000 <- mean(fairs.1970.2000$value)

