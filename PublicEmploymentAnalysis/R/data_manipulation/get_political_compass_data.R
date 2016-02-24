library(readstata13)
library(data.table)
data <- as.data.table(read.dta13('../../data/alt_lassen_wehner_transparency_data.dta'))
data <- as.data.table(read.dta13('../../data/wdi.dta'))
data <- as.data.table(read.dta13('../../data/rest.dta'))
data <- as.data.table(read.dta13('../../data/dpi.dta'))
data <- data[, list(ifs, year, execrlc)] # 1 right, 2 center, 3 left
data[execrlc < 0 , execrlc:=NA]
setnames(data, c('ifs', 'year'), c('location', 'time'))
write.csv(data, '../../data/dpi_excecrlc_cleaned.csv')

## Caution: Have to renamed some column (country and time in the other data set)
