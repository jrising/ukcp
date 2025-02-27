## https://pdfs.semanticscholar.org/542c/47f8fe03a36eb9871115e8b226288c519032.pdf?_ga=2.131851215.1529047367.1645760744-972231136.1645760744
## https://www.piie.com/system/files/documents/wp19-5.pdf

df <- data.frame(source=rep(c('Latorre et al. 2019', 'Dhingra et al. 2017'), each=2), brexit=rep(c('Soft Brexit', 'Hard Brexit'), 2), scenario=c('Soft (Norway case)', 'Hard (WTO case)', 'Soft Brexit scenario', 'Hard Brexit scenario'), case=c('Latorre et al. 2019 Soft (Norway case)', 'Latorre et al. 2019 Hard (WTO case)', 'Dhingra et al. 2017 Soft Brexit scenario', 'Dhingra et al. 2017 Hard Brexit scenario'), dgdp=c(-1.23, -2.53, -1.34, -2.66), dexport=c(-7.54, -16.94, -9, -16), dimport=c(-6.44, -14.42, -14, -16), method=rep(c('GAMS', 'Eaton–Kortum'), each=2))

mod <- lm(dgdp ~ 0 + dexport + dimport, data=df)

if (F) {
    summary(mod)

    library(ggplot2)

    ggplot(df, aes(y=dgdp, colour=source, shape=brexit)) +
        geom_point(aes(x=dimport))

    ggplot(df, aes(y=dgdp, colour=source, shape=brexit)) +
        geom_point(aes(x=dexport)) + xlim(-17, 0) + ylim(-2.8, 0)

    library(xtable)

    tbl <- df[, c('source', 'scenario', 'dgdp', 'dexport', 'dimport', 'method')]
    names(tbl) <- c('Source', 'Scenario', '% GDP', '% Exports', '% Imports', 'Method')
    print(xtable(tbl), include.rownames=F)

    library(stargazer)
    stargazer(mod)
}

comtrade <- read.csv("topdown/comtrade.csv")
comtrade <- subset(comtrade, Trade.Flow != 'Re-Export') # included in exports
comtrade.world <- subset(comtrade, Partner.ISO == 'WLD')
comtrade.other <- subset(comtrade, Partner.ISO != 'WLD')

## Returns % decrease in GDP
calc.tradeloss <- function(impact, maxgrow) { # ADM0, fracloss
    df <- comtrade.other %>% left_join(impact, by=c('Partner.ISO'='ADM0'))
    ## Limit growth to growth of UK
    df$value.lost <- df$Trade.Value..US.. * pmax(df$fracloss, -maxgrow)

    fracloss.import <- sum(df$value.lost[df$Trade.Flow == 'Import'], na.rm=T) / comtrade.world$Trade.Value..US..[comtrade.world$Trade.Flow == 'Import']
    fracloss.export <- sum(df$value.lost[df$Trade.Flow == 'Export'], na.rm=T) / comtrade.world$Trade.Value..US..[comtrade.world$Trade.Flow == 'Export']

    ## Convert to %, just in case future model cares
    dgdp <- predict(mod, data.frame(dimport=-100 * fracloss.import, dexport=-100 * fracloss.export))
    -dgdp
}

calc.finals <- function(results2) {
    finals <- data.frame() # % GDP loss
    for (scn in c('ssp126', 'ssp370')) {
        for (per in c(2020, 2050, 2090)) {
            for (measure in c('mean', 'ci05', 'ci95')) {
                subres <- subset(results2[[scn]], year > per - 10 & year <= per + 10)

                if (measure == 'mean')
                    subres2 <- subres %>% group_by(year, ADM0) %>% summarize(fracloss=1 - exp(mean(impact, na.rm=T)))
                else if (measure == 'ci05')
                    subres2 <- subres %>% group_by(year, ADM0) %>% summarize(fracloss=1 - exp(quantile(impact, .05, na.rm=T)))
                else if (measure == 'ci95')
                    subres2 <- subres %>% group_by(year, ADM0) %>% summarize(fracloss=1 - exp(quantile(impact, .95, na.rm=T)))
                else
                    print("Error")

                subres3 <- subres2 %>% group_by(ADM0) %>% summarize(fracloss=mean(fracloss, na.rm=T))

                gdploss <- calc.tradeloss(subres3, -subres3$fracloss[subres3$ADM0 == 'GBR'])

                finals <- rbind(finals, data.frame(scenario=scn, period=per, measure, alone=100 * subres3$fracloss[subres3$ADM0 == 'GBR'], globe=gdploss))
            }
        }
    }

    finals
}

final2tbl <- function(finals) {
    tbl <- data.frame()
    for (per in unique(finals$period)) {
        values <- data.frame(period=paste(per - 9, "-", per + 10))
        for (scn in c('ssp370', 'ssp126')) {
            for (measure in c('ci05', 'mean', 'ci95'))
                values[, paste(scn, measure)] <- finals$globe[finals$scenario == scn & finals$period == per & finals$measure == measure]
        }

        for (measure in c('ci05', 'mean', 'ci95'))
            values[, paste('diff', measure)] <- finals$globe[finals$scenario == 'ssp370' & finals$period == per & finals$measure == measure] - finals$globe[finals$scenario == 'ssp126' & finals$period == per & finals$measure == measure]

        tbl <- rbind(tbl, values)
    }

    library(xtable)
    print(xtable(tbl, digits=2), include.rownames=F)
}
