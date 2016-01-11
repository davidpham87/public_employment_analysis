setwd('../')
source('init.R')
loadPackages()

## Avoid data type conversion by using the melt data.frame
csvx <-  list.files('../data/') %>% { .[grep('eo-.+melt\\.csv$', .)]}
eos <- lapply(csvx, function(x) {
  res <- fread(file.path('..', 'data', x))
  res[, V1:=NULL]
  res
})

names(eos) <- {gsub('-', '.', csvx)} %>% {gsub('_melt\\.csv$', '', .)}

eos.melt <- lapply(eos, function(eo) eo[, VALUE:=as.numeric(VALUE)][, VARIABLE:=tolower(VARIABLE)])
eos <- lapply(eos.melt, function(eo) {
  dcast(eo[, list(LOCATION, VARIABLE, TIME, VALUE)],
        LOCATION+TIME~VARIABLE, value.var='VALUE')})
eos <- lapply(eos,   function(eo) {
  l <- list(LOCATION='country', TIME='TIME')
  cn <- colnames(eo)
  for (n in names(l)){
    if (n %in% cn) setnames(eo, n, l[[n]])
  }
  eo
})

## change time

## list(list('-Q1', '.00'), ... , list('-Q4', '.75'))
quarter.substitute <-
  lapply(1:4, function(i) list(paste0('-Q', i), paste0('.', 100*(i-1)/4)))
## split in two
quarters.year <- lapply(strsplit(eos$eo.quarter$TIME, '-'),
                        function(x) data.table(YEAR=x[1], QUARTER=x[2])) %>% rbindlist

quarters.time <- Reduce(function(x, l) gsub(l[[1]], l[[2]], x),  quarter.substitute,
                        eos$eo.quarter$TIME) %>% as.numeric
eos$eo.quarter[, TIME:=quarters.time]

eos$eo.quarter <- cbind(eos$eo.quarter, quarters.year)
eos$eo.annual[, TIME:=as.numeric(TIME)]

saveRDS(eos, '../data/eo-data.rds')

eo.cnames <- fread('../data/eo-colnames.csv')
eo.var.names <- unique(eo.cnames[, list(VARIABLE=tolower(VARIABLE), Variable)])
setkey(eo.var.names, 'VARIABLE')
saveRDS(eo.var.names, '../data/eo-colnames-dt.rds')
write.csv(eo.var.names, '../data/eo-colnames-unique.csv')
