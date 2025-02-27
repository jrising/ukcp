setwd("~/Open Modeling Group Dropbox/UK Economic Risks")

load("channels/energy/rodeetal2-project.RData")

source("lib/disaggregate.R")
polydata.withattr2 <- disaggregate("~/Dropbox/For DJ/world-combo-201710/agglomerated-world-new.shp", 'hierid', function(shp, polydata) {
    shp <- subset(shp, PID %in% polydata$PID[polydata$ISO == 'GBR'])
})

## Make new damages by ADM3

results2.adm3 <- results2 %>% left_join(polydata.withattr2[, c('NAME_3', 'region', 'pop.share')], by='region')
results2.adm3$damage.adm3 <- results2.adm3$damage * results2.adm3$pop.share

save(results2.adm3, file="channels/energy/rodeetal2-project-adm3.RData")
