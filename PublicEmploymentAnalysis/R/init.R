PCKGS <- c('data.table',
           'readstata13',
           'magrittr',
           'ggplot2',
           'tikzDevice',
           'stargazer',
           'rsdmx'
)

loadPackages <- function(){
  lapply(PCKGS, require, character.only=TRUE)
}
