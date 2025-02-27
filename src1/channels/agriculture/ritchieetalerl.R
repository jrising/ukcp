setwd("~/Open Modeling Group Dropbox/UK Economic Risks/channels/agriculture")

df.grid <- read.csv("Ritchie et al ERL/ECO_AG_grid_2km.csv")
df.base <- read.csv("Ritchie et al ERL/ECO_AG_baseline.csv")
df.2100 <- read.csv("Ritchie et al ERL/ECO_AG_climate.csv")

library(dplyr)

df <- df.grid %>% left_join(df.2100)

library(ggplot2)

ggplot(df, aes(Easting, Northing, fill=arable_ha_2100)) +
    geom_raster()
