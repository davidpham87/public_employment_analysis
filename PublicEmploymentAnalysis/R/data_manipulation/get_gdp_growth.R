# Get data from OECD and parse them into csv
setwd('../')
source('init.R')
loadPackages()

x <- fread('../data/gdp_growth_quarterly.csv')
x %>%
  colnames %>%
  { gsub('["[:blank:]]', '', .) } %>%
  strtrim(nchar(.)) -> colnames(x)
colnames(x)[1] <- 'location'

idx <- x[, MEASURE[1]]
x <- x[MEASURE=='GPSA']
idx <- 'gdpv_annpct_quarterly'

parseQuarter <- function(e){
  quarters <- list(Q1='.00', Q2='.25', Q3='.50', Q4='.75')
  as.numeric(paste0(e[1], quarters[[e[2]]], collapse=''))
}

f <- function(e) data.table(YEAR=e[1], QUARTER=e[2], TIME=parseQuarter(e))
x[, TIME_STRING:=TIME]
x[, TIME:=NULL]
y <- cbind(x, strsplit(x$TIME_STRING, '-') %>% lapply(f) %>% rbindlist)
x <- y[, list(location, TIME=as.numeric(TIME), Value)][order(location, TIME)]
colnames(x) <- gsub('Value', idx, colnames(x))
write.csv(x, paste0('../data/', 'gdp_growth_quarterly', '_cleaned.csv'))
