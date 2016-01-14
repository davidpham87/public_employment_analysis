library(readstata13)
library(data.table)
data <- as.data.table(read.dta13('../../data/DPI2012.dta'))
data <- data[, list(ifs, year, execrlc)] # 1 right, 2 center, 3 left
data[execrlc < 0 , execrlc:=NA]
write.csv(data, '../../data/dpi_excecrlc_cleaned.csv')

## Caution: Have to renamed some column (country and time in the other data set)
