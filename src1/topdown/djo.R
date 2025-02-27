setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("topdown/driver.R")
source("topdown/togdp.R")

gamma.poor <- -1.394 / 100
gamma.poor.se <- 0.408 / 100
coeff.poor <- rnorm(max(alldraws$run_id) + 1, gamma.poor, gamma.poor.se)

gdppcs <- read.csv("socioeconomics/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_3630804/API_NY.GDP.PCAP.KD_DS2_en_csv_v2_3630804.csv", skip=3)
gdppc0s <- data.frame()
for (ii in 1:nrow(gdppcs)) {
    ##nonnas <- which(!is.na(gdppcs[ii, 15:25])) # start in 1970s, like DJO
    ##nonnas <- which(!is.na(gdppcs[ii, -(1:14)])) # start in 1970s, like DJO
    nonnas <- which(!is.na(gdppcs[ii, 15:45])) # start in 1970s, like DJO
    if (length(nonnas) == 0)
        next
    year0 <- nonnas[1] + 1969
    gdppc0s <- rbind(gdppc0s, data.frame(ADM0=gdppcs$Country.Code[ii], year0, gdppc0=gdppcs[ii, paste0('X', year0)]))
}

gdppc0s$valid <- gdppc0s$ADM0 %in% comtrade.other$Partner.ISO
poors <- gdppc0s$ADM0[gdppc0s$gdppc0 < median(gdppc0s$gdppc0[gdppc0s$valid]) & gdppc0s$valid]

setup <- function(scenario, run_id) {
    if (is.null(run_id))
        return(gamma)
    coeff.poor[run_id + 1]
}

simulate <- function(coeff.poor, year, adm0s, tas) {
    data.frame(ADM0=adm0s, impact=tas * coeff.poor * (adm0s %in% poors))
}

## oneres <- country.project.single(setup, simulate)
## plot((oneres %>% filter(ADM0 == 'GBR'))$dimpact)
## oneres2 <- oneres %>% group_by(scenario, run_id, ADM0) %>% summarize(year=year, impact=cumsum(dimpact))
## plot((results2 %>% filter(scenario == 'ssp126' & ADM0 == 'GBR'))$impact)

results <- country.project(setup, simulate)

save(results, file="topdown/djo-project.RData")

results2 <- byscen(results, function(results) {
    results$impact <- results$dimpact
    results
})

finals <- calc.finals(results2)

subset(finals, measure == 'mean')

write.csv(finals, "topdown/djo-final.csv", row.names=F)

source("topdown/togdp.R")
finals <- read.csv("topdown/djo-final.csv")
finals$measure <- as.character(finals$measure)
finals$measure[finals$measure == 'ci95'] <- 'xxx'
finals$measure[finals$measure == 'ci05'] <- 'ci95'
finals$measure[finals$measure == 'xxx'] <- 'ci05'
final2tbl(finals)
