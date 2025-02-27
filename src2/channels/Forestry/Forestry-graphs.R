#Plotting

channel = "Fishery"

load(file = file.path("/Users/ritikakhurana/Open Modeling Group Dropbox/COACCH/channels", 
                      channel, "results-final.RData"))

source("../../UK Economic Risks/lib/report.R", chdir=T)

dirs <-list.dirs(getwd(), recursive=FALSE)

setwd(dirs[3])

make.maps.loop("graphs", "for", byadm3.match, shp.adm3, function(subres) {
  subres %>% group_by(PID) %>% summarize(mu=mean(damage, na.rm=T), ci5=quantile(damage, .05, na.rm=T), 
                                         ci95=quantile(damage, .95, na.rm=T))
}, "Fishery damages (% GDP)", "95% CI\nfishery\ndamage (£m)", NULL, NULL)