library(data.table)
library(magrittr)
## Some functions to ease the manipulation
butlast <- function(x) x[-length(x)]

completeVoidDT <- function(DT.old){
  DT <- copy(DT.old)
  new.time <- seq(min(DT$TIME)[1], 2012.75, by=0.25)
  DT[, yrcurnt_next:= as.double(yrcurnt_next)]
  DT.fulltime <- data.table(TIME=new.time)
  merge(DT, DT.fulltime, by='TIME', all.y=TRUE)
}


##' Split a numeric value (2008.75) into a list year (numeric) quarter (string)
##' Could clearly be vectorized
splitTime <- function(t){
  li <- as.character(t) %>% {strsplit(., split="\\.")[[1]]}
  quarters <- c('00'='Q1', '25'='Q2', '5'='Q3', '75'='Q4')
  li[[1]] <- as.numeric(li[[1]])
  if (length(li) == 1) li[[2]] <- '00'
  li[[2]] <- quarters[li[[2]]]
  li
}

## Strategy fill forward for election variables and linear decrease for the yurcrnt variable
fillData <- function(DT.old){
  DT <- copy(DT.old)
  cols <- seq(1:ncol(DT))
  names(cols) <- colnames(DT)
  for (i in 1:nrow(DT)){
    year.quarter <- splitTime(DT[i, cols['TIME'], with=F])
    set(DT, i, cols['year'], year.quarter[1])
    set(DT, i, cols['quarter'], year.quarter[2])
    if (DT[i, is.na(yrcurnt_next)]){
      set(DT, i, cols['yrcurnt_next'], DT[i-1, yrcurnt_next - 0.25])
    }
  }
  DT[, execrlc_before:=zoo::na.locf(execrlc_before)]
  DT[, execrlc_elected:=zoo::na.locf(execrlc_elected)]
  DT[, govfrac:=zoo::na.locf(govfrac)]
  DT[is.na(is_election_date), is_election_date:=0]
  DT[is.na(surprise_election), surprise_election:=0]
}


## all(splitTime('2008.75') == list(2008, 'Q4'))


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
x <- x[country][time>=1985] # Need some data before 1990
x[, yrcurnt_diff:= c(NA, diff(yrcurnt_corrected)), by='location']
x[, yrcurnt_lag := c(NA, butlast(yrcurnt_corrected)), by='location']
x[, yrcurnt_lead := c(yrcurnt_corrected[-1], NA), by='location']
x[, execrlc_lead := c(execrlc[-1], NA), by='location']
## Wants to see which dates on has to find
## yrcurnt_corrected means election years
y <- x[yrcurnt_corrected==0]
as.data.frame(y[, list(location, time, yrcurnt_corrected, execrlc, execrlc_lead, govfrac=round(govfrac, 6), yrcurnt_lead+1)])

## Manualy entry of data
## Retake data from the manual entry

x <- fread('../../data/execrlc_govfrac_yrcurnt_quartery.csv')
x[, source:=NULL] # no need to know where the origin of the data
x[, is_election_date:=1] # to track election terms
x[, surprise_election:=as.numeric(gsub("0x0", 1, (gsub("", 0, surprise_election))))]
setkey(x, location)

DT <- copy(x)
DT <- DT[, completeVoidDT(.SD), by=location][, fillData(.SD), by='location']
DT[yrcurnt_next < 0, yrcurnt_next:=0]

library('lattice')

pdf('../plot/dpi_quarterly_statistics.pdf', 16, 9, onefile=TRUE)
xyplot(yrcurnt_next ~ TIME | location, DT, main='Years until next election')
xyplot(execrlc_elected~ TIME | location, DT, main='Political orientation', type='s')
xyplot(govfrac~ TIME | location, DT, main='Government fractionalization', type='s') # measure is not precise
dev.off()

DT[, execrlc_before:=NULL]
setnames(DT, c('execrlc_elected', 'yrcurnt_next'),
         c('execrlc', 'yrcurnt'))
write.csv(DT, '../../data/execrlc_govfrac_yrcurnt_quartery_cleaned.csv')
