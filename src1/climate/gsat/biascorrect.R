## Careful bias-correction according to Chris Smit

df = read.csv("~/Dropbox/UK Economic Risks/climate/gsat/ssps_26_70_ukproject_allmembers.csv")
df.p1 <- df %>% filter(year >= 1850 & year <= 1900) %>% group_by(scenario, run_id) %>% summarize(mu=mean(value))
df.p2 <- df %>% filter(year >= 1995 & year <= 2014) %>% group_by(scenario, run_id) %>% summarize(mu=mean(value))

df2 <- df.p1 %>% left_join(df.p2, by=c('scenario', 'run_id'), suffix=c('.p1', '.p2'))
df2$comp <- df2$mu.p2 - df2$mu.p1
quantile(df2$comp, c(.05, .95)) # compare to IPCC [0.69 - 0.95]
mean(df2$comp) # compare to IPCC 0.85
