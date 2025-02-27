setwd("~/Open Modeling Group Dropbox/UK Economic Risks/hazards")

library(readxl)

df <- read_excel("emdat_public_2022_03_23_query_uid-hCceuz.xlsx")
df$Year <- as.numeric(df$Year)

library(ggplot2)

ggplot(df, aes(Year, `Total Damages, Adjusted ('000 US$)`, fill=`Disaster Type`)) +
    geom_col()

ggplot(df, aes(Year, `Total Deaths`, fill=`Disaster Type`)) +
    geom_col()

usd2gbp <- 0.7798 # https://www.exchangerates.org.uk/USD-GBP-spot-exchange-rates-history-2020.html#:~:text=Currency%20Menu&text=This%20is%20the%20US%20Dollar,GBP%20on%2031%20Dec%202020.
vpf.2016 <- 1.83e6 # https://www.bristol.ac.uk/media-library/sites/policybristol/PolicyBristol-Report-April-2018-value-human-life.pdf
gdp.2015.gbp <- 2089276e6 # GBP, from https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2
pop.2015 <- 65.12e6 # from google

df$total <- ifelse(is.na(df$`Total Damages, Adjusted ('000 US$)`), 0, df$`Total Damages, Adjusted ('000 US$)`) * usd2gbp * 1000 +
    vpf.2016  * ifelse(is.na(df$`Total Deaths`), 0, df$`Total Deaths`) +
    (gdp.2015.gbp / pop.2015 / 12) * ifelse(is.na(df$`Total Affected`), 0, df$`Total Affected`)
df$type <- paste0(df$`Disaster Subgroup`, ': ', df$`Disaster Type`)

ggplot(df, aes(Year, total, fill=type)) +
    geom_col()

df2 <- rbind(data.frame(Year=1970:2020, total=0, type=NA),
             df[, c('Year', 'total', 'type')])

library(dplyr)

ggplot(subset(df2, Year >= 1970), aes(Year, total / 1e6)) +
    geom_col(aes(fill=type)) +
    geom_smooth(data=subset(df2, Year >= 1970) %>% group_by(Year) %>% summarize(total=sum(total)), method='lm') +
    scale_fill_manual(NULL, breaks=c("Hydrological: Flood", "Meteorological: Storm", "Meteorological: Extreme temperature"),
                      values=c('#7570b3', '#1b9e77', '#d95f02')) +
    scale_x_continuous(NULL, expand=c(0, 0)) + scale_y_continuous("Total damages (£million)", expand=c(0, 0)) +
    theme_bw() + coord_cartesian(ylim=c(0, 11e3))
ggsave("emdat.pdf", width=6.5, height=3)

1e9 / gdp.2015.gbp
