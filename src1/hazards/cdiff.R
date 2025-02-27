setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

source("lib/project.R")

results <- data.frame()
for (scenario in unique(alldraws$scenario)) {
    for (run_id in 0:max(alldraws$run_id)) {
        print(c(scenario, run_id))

        warming <- get.warming.eoc(scenario, run_id)
        pattern <- common.patterns$pattern[gridpatts$run_id == run_id]

        subres <- data.frame()
        for (period in names(periods)) {
            for (year in periods[[period]][1]:periods[[period]][2]) {
                warming.now <- alldraws$value[alldraws$scenario == scenario & alldraws$run_id == run_id & alldraws$year == year]
                cdiffs <- grid.pattern.cdiff(pattern, scenario, year)
                fullcdiff <- warming.now + cdiffs$variable

                subres <- rbind(subres, data.frame(period, year, x=cdiffs$x, y=cdiffs$y, cdiff=fullcdiff))
            }

            resrow <- subres %>% group_by(period, x, y) %>% summarize(cdiff=mean(cdiff))
            results <- rbind(results, cbind(scenario=scenario, run_id=run_id, pattern=pattern, resrow))
        }
    }
}
