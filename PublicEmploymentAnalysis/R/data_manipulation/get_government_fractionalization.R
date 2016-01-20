library(readstata13)
library(data.table)
data <- as.data.table(read.dta13('../../data/DPI2012.dta'))
data <- data[, list(ifs, year, govfrac)] # 1 right, 2 center, 3 left
data[govfrac < 0 , govfrac:=NA]
setnames(data, c('ifs', 'year'), c('location', 'time'))
write.csv(data, '../../data/dpi_govfrac_cleaned.csv')

## Caution: Have to renamed some column (country and time in the other data set)
