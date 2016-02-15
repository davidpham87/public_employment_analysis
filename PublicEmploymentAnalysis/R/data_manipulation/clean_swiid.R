# Get data from OECD and parse them into csv
setwd('../')
source('init.R')
loadPackages()

load('../data/SWIIDv5_0.RData')
iso.country <- fread('../data/countries_iso.csv')
dt.swiid <- rbindlist(swiid)

dt.swiid <- merge(iso.country, dt.swiid, by='country') # lucky there are match!

dt.swiid <- dt.swiid[, {
  gn <- mean(gini_net)
  gm <- mean(gini_market)
  abs_red <- gm - gn
  list(gini_net=gn, gini_market=gm, rel_red=abs_red/gm, abs_red=abs_red)},
by='iso,year']


setnames(dt.swiid, c('iso', 'year'), c('location', 'time'))
write.csv(dt.swiid, '../data/SWIID_cleaned.csv')
