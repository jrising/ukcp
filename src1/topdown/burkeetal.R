setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(MASS)

source("topdown/driver.R")

gamma <- c(0.0127183, -0.0004871)
gammavcv <- matrix(c(0.0000143,-0.000000376,-3.76E-07,1.40E-08), 2, 2, byrow=T)
coeffs <- mvrnorm(max(alldraws$run_id) + 1, mu=gamma, Sigma=gammavcv, empirical=T)

setup <- function(scenario, run_id) {
    if (is.null(run_id))
        return(gamma)
    as.numeric(coeffs[run_id + 1,])
}

simulate <- function(coeffs, year, adm0s, tas) {
    tas2 <- tas^2
    ##data.frame(ADM0=adm0s, impact=cbind(tas, tas2) %*% coeffs, tas)
    data.frame(ADM0=adm0s, impact=cbind(tas, tas2) %*% coeffs)
}

## oneres <- country.project.single(setup, simulate)
## plot((oneres %>% filter(ADM0 == 'GBR'))$dimpact)
## oneres2 <- oneres %>% group_by(scenario, ADM0) %>% summarize(year=year, impact=cumsum(dimpact))
## plot((oneres2 %>% filter(scenario == 'ssp126' & ADM0 == 'GBR'))$impact)

results <- country.project(setup, simulate)

save(results, file="topdown/burkeetal-project.RData")
## load("topdown/burkeetal-project.RData")

results2 <- byscen(results, function(results) results %>% group_by(run_id, ADM0) %>% summarize(year=year, impact=cumsum(dimpact))) # impact=dimpact))

make.map("topdown/maps", "burkeetal", results2, loval=-1, hival=1)

## as.data.frame(results2[['ssp370']] %>% group_by(year) %>% summarize(impact=mean(impact, na.rm=T)))
## as.data.frame(subset(results2[['ssp370']], ADM0 == 'GBR') %>% group_by(year) %>% summarize(impact=mean(impact, na.rm=T)))
## as.data.frame(subset(results2[['ssp370']], ADM0 == 'USA') %>% group_by(year) %>% summarize(impact=mean(impact, na.rm=T)))

source("topdown/togdp.R")

finals <- calc.finals(results2)

subset(finals, measure == 'mean')

write.csv(finals, "topdown/burkeetal-final.csv", row.names=F)

source("topdown/togdp.R")
finals <- read.csv("topdown/burkeetal-final.csv")
finals$measure <- as.character(finals$measure)
finals$measure[finals$measure == 'ci95'] <- 'xxx'
finals$measure[finals$measure == 'ci05'] <- 'ci95'
finals$measure[finals$measure == 'xxx'] <- 'ci05'
final2tbl(finals)
