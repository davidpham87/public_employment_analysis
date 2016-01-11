setwd('../')
source('init.R')
loadPackages()
imf <- fread('../data/imf_gfs_scores.csv', header=TRUE) # melt data
lassen <- fread('../data/lassen_fiscal_scores.csv', header=TRUE)

# setkey(imf, ISO)
# setkey(lassen, ISO)
# lassen[J(imf)][, c('i.Country', 'ISO', 'Fiscal Score', 'Index Score'), with=F]
# merge(imf, lassen, all=T)[, c('Country', 'ISO', 'Fiscal Score', 'Index Score'), with=F]

imf.static <- imf[, list(ISO, `Index Score`)]
imf.melt <- melt(unselect(imf, c("Country", "Index Score")), id.vars='ISO',
                 variable.name='TIME', value.name='fiscal_transparency')
setnames(imf, 'ISO', 'location')
write.csv(imf.melt, '../data/imf_gfs_scores_cleaned.csv')
