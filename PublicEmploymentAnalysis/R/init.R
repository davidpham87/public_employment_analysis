PCKGS <- c('data.table',
           'readstata13',
           'magrittr',
           'ggplot2',
           'tikzDevice',
           'stargazer')

loadPackages <- function(){
  lapply(PCKGS, require, character.only=TRUE)
}
