# Get data from OECD and parse them into csv
setwd('../')
source('init.R')
loadPackages()

library(readstata13)
library(data.table)

iso.country <- fread('../data/countries_iso.csv')

# data <- as.data.table(read.dta13('../data/alt_lassen_wehner_transparency_data.dta'))
data <- as.data.table(read.dta13('../data/wdi.dta'))
setnames(data, 'NAMES_STD', 'country')
data <- merge(iso.country, data, by='country', all=TRUE)


data.rest <- as.data.table(read.dta13('../data/rest.dta'))
setnames(data.rest, 'ccode', 'iso')
data <- merge(data, data.rest, by=c('iso', 'year'), all=TRUE)

data.dpi <- as.data.table(read.dta13('../data/dpi.dta', generate.factors=T))
setnames(data.dpi, 'NAMES_STD', 'country')
data <- merge(data, data.dpi, by=c('country', 'year'), all=TRUE)

load('../data/SWIIDv5_0.RData')
dt.swiid <- rbindlist(swiid)
dt.swiid <- merge(iso.country, dt.swiid, by='country') # lucky there are match!

cols <- c("iso", 'year', "ny_gdp_totl_rt_zs", "RevenueIndex",
          "EmploymentIndex", "RegulationIndex", "SubsidisationIndex",
          "auton", "stconst",  "parlsys")

data <- data[, cols, with=F][order(iso)]
data <- data[!is.na(iso) & !is.na(year)]

# data[execrlc < 0 , execrlc:=NA]
data[auton < 0, auton:=NA]
data[stconst < 0, stconst:=NA]
data[parlsys < 0, parlsys:=NA]

setnames(data, c('iso', 'year'), c('location', 'time'))
write.csv(data, '../data/wdi_rest_federalism_cleaned.csv')

## Caution: Have to renamed some column (country and time in the other data set)
