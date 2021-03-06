# Get data from OECD and parse them into csv
setwd('../')
source('init.R')
loadPackages()
data.links <- fread('../data/oecdDataLinks.csv')

downloadOECDData <- function(link, file.name){
  download.file(link, paste0('../data/', file.name, '.csv'))
}

setkey(data.links, data)

data.links[, downloadOECDData(links, data), by=1:nrow(data.links)]


data <- lapply(data.links$data, function(x) fread(paste0('../data/', x, '.csv')))
names(data) <- data.links$data

for (i in 1:length(data)){
  x <- data[[i]]
  x %>%
    colnames %>%
    { gsub('["[:blank:]]', '', .) } %>%
    strtrim(nchar(.)) -> colnames(x)
  idx <- x[, INDICATOR[1]]
  x <- x[, list(LOCATION, TIME=as.numeric(TIME), Value)][order(LOCATION, TIME)]
  colnames(x) <- gsub('Value', idx, colnames(x))
  data[[i]] <- x
}

for (e in names(data)){
  write.csv(data[[e]], paste0('../data/', e, '_cleaned.csv'))
}
