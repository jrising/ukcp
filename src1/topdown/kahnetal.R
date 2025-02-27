setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

library(MASS)

source("topdown/driver.R")

## difftas-pos: Difference between yearly average temperature and 30-year average, if positive [C]
## difftas-neg: Difference between yearly average temperature and 30-year average, if negative [C]
gamma <- c(-0.0376,-0.0451)
gammavcv <- matrix(c(0.00015876, 0, 0, 0.00049729), 2, 2, byrow=T)
coeffs <- mvrnorm(max(alldraws$run_id) + 1, mu=gamma, Sigma=gammavcv, empirical=T)

setup <- function(scenario, run_id) {
    if (is.null(run_id))
        return(gamma)
    as.numeric(coeffs[run_id + 1,])
}

all(get.country.temps('MIROC6', 'ssp126', '2041-2060')$ADM0 == get.country.temps('MIROC6', 'ssp370', '2041-2060')$ADM0)
allregion <- unique(get.country.temps('MIROC6', 'ssp126', '2041-2060')$ADM0)

last30 <- NULL
simulate <- function(coeffs, year, adm0s, tas) {
    rows <- as.numeric(factor(adm0s, levels=allregion))
    if (year == year1) {
        last30 <<- matrix(NA, length(allregion), 30)
        last30[rows, 1] <<- tas
    }

    clims <- rowMeans(last30, na.rm=T)
    diffs <- tas - clims[rows]

    posdiff <- pmax(diffs, 0)
    negdiff <- -pmin(diffs, 0)

    last30[rows, (year - 1981) %% 30 + 1] <<- tas

    data.frame(ADM0=adm0s, impact=coeffs[1] * posdiff + coeffs[2] * negdiff)
}

## oneres <- country.project.single(setup, simulate)
## subset(oneres, scenario == 'ssp370' & ADM0 == 'GBR')

results <- country.project(setup, simulate)

## as.data.frame(subset(results[['ssp370']], ADM0 == 'GBR') %>% group_by(year) %>% summarize(dimpact=mean(dimpact)))

save(results, file="topdown/kahnetal-project.RData")

results2 <- byscen(results, function(results) {
    results$impact <- results$dimpact
    results
})

source("topdown/togdp.R")

finals <- calc.finals(results2)

subset(finals, measure == 'mean')

write.csv(finals, "topdown/kahnetal-final.csv", row.names=F)

source("topdown/togdp.R")
finals <- read.csv("topdown/kahnetal-final.csv")
finals$measure <- as.character(finals$measure)
finals$measure[finals$measure == 'ci95'] <- 'xxx'
finals$measure[finals$measure == 'ci05'] <- 'ci95'
finals$measure[finals$measure == 'xxx'] <- 'ci05'
final2tbl(finals)
