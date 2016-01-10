# From the sdmx xml files, one creates the data.table OECD sdmx tables are
# quite robust and the structure almost not vary, so we rely on a simple query
# to look for the keys in the xml and not on the XML packages.

source('init.R')
library(XML)
library(parallel)

pckgs <- loadPackages()

parseSeriesKey <- function(serie.key){
  li <- lapply(serie.key, function(x) {
    l <- list(x[[2]])
    names(l) <- x[[1]]
    l
  })
  as.data.table(do.call(c, li))
}

parseSerie <- function(serie){
  tryCatch({
    meta.data <- parseSeriesKey(serie$SeriesKey)
    attr.data <- parseSeriesKey(serie$Attributes)
    obs.idx <- which(names(serie)=='Obs')
    res <- serie[obs.idx] %>%
      lapply(function(l) {
        tryCatch(data.table(TIME=l$Time, VALUE=l$ObsValue),
                 error=function(e) NULL)}) %>%
      rbindlist
    cbind(meta.data, attr.data, res)
  }, error=function(e) print(e[1]))
}

filenames <- c('../data/eo-quarter.xml', '../data/eo-annual.xml')
for (filename.path in filenames){

  eo <- readSDMX(filename.path, FALSE)
  eo.li <- xmlToList(eo@xmlObj)
  ds <- eo.li$DataSet[-1]

  res <- ds[which(names(ds)=='Series')] %>%
    mclapply(parseSerie, mc.cores=detectCores()) %>%
    rbindlist(fill=TRUE)

  nx <- names(res) %>% {.[grep("Value\\.", .)]}
  new.nx <- nx %>% {gsub("Value\\.", '', .)}

  setnames(res, nx, new.nx)
  res.d <- dcast(res[, list(LOCATION, VARIABLE, TIME, VALUE)], LOCATION+TIME~VARIABLE, value.var='VALUE')
  write.csv(res, gsub('\\.xml$', '_melt.csv', filename.path))
  write.csv(res.d, gsub('\\.xml$', '_dcast.csv', filename.path))
}
