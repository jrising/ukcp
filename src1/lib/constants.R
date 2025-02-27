## Socioeconomic totals

gdp.2015 <- 2.933e12 # USD
gdp.2015.gbp <- 2089276e6 # GBP, from https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2
gdp.2012.gbp <- 1940087e6 # GBP, from https://www.ons.gov.uk/economy/grossdomesticproductgdp/timeseries/abmi/pn2
gdp.2020.gbp <- 2043373e6

pop.2015 <- 65110000 # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2015

## Conversion rates

convert.usd2gbp.2015 <- 0.6545

## Valuation constants

vpf.2016 <- 1.83e6 # https://www.bristol.ac.uk/media-library/sites/policybristol/PolicyBristol-Report-April-2018-value-human-life.pdf

## Sectoral-specific values

uk.mortrate <- 9 / 1000

## From https://data.worldbank.org/indicator/NY.GDP.DEFL.ZS?locations=GB
deflator <- data.frame(year=2010:2021, wb=c(92.59336445, 94.5602434, 95.95623676, 98.03972192, 99.29247049, 100, 101.8786095, 103.7034227, 105.5056356, 107.7354063, 114.1275061, 114.548927))
