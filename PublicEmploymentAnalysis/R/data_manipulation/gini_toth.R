setwd('../')
source('init.R')
loadPackages()
gini <- fread('../data/gini_toth.csv', header=TRUE) # melt data

gini[, c('DEU', 'KOR'):=NULL]
gini.melt <- melt(gini, id.vars='TIME', variable.name='location', value.name='gini_toth')
write.csv(gini.melt, '../data/gini_toth_cleaned.csv')
