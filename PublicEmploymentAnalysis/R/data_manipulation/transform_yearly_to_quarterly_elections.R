library(data.table)

## Some functions to ease the manipulation
butlast <- function(x) x[-length(x)]

execrlc <- fread('../../data/dpi_excecrlc_cleaned.csv')[, V1:=NULL]
govfrac <- fread('../../data/dpi_govfrac_cleaned.csv')[, V1:=NULL]
yrcurnt <- fread('../../data/year_until_elections_cleaned.csv')[, V1:=NULL]
yrcurnt <- yrcurnt[, list(location, time, yrcurnt_corrected)]

country <- c("BEL", "CAN", "CZE", "DNK", "EST", "FIN", "FRA", "GBR", "HUN",
             "IRL", "JPN", "LTU", "LUX", "NLD", "NOR", "POL", "SWE", "USA")

setkey(yrcurnt, location, time)
setkey(govfrac, location, time)
setkey(execrlc, location, time)

## Join them

x <- merge(merge(govfrac, execrlc, all=TRUE), yrcurnt, all=TRUE)
x <- x[country][time>=1990]
x[, yrcurnt_diff:= c(NA, diff(yrcurnt_corrected)), by='location']
x[, yrcurnt_lag := c(NA, butlast(yrcurnt_corrected)), by='location']
x[, yrcurnt_lead := c(yrcurnt_corrected[-1], NA), by='location']
x[, execrlc_lead := c(execrlc[-1], NA), by='location']
## Wants to see which dates on has to find
## yrcurnt_corrected means election years
y <- x[yrcurnt_corrected==0]
as.data.frame(y[, list(location, time, yrcurnt_corrected, execrlc_lead, govfrac=round(govfrac, 6), yrcurnt_lead+1)])

## Manualy entry of data
