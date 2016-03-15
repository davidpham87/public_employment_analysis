## Difference with previous analysis: egr := 100*eg/lf (not et)

## the Government fractionalization measure from the World-Bank database on
## political institutions is used. It measures the probability that two randomly
## chosen deputies from the government parties will be from different parties

## 1) Correct data to have approximatively election date.

## But, as said the current two steps are to be: 1) checking with Bai and Perron
## whether there are structural breaks in eglf, and 2) coming up with a baseline
## model that is well-justified by the literature and makes statistical sense when
## explaining the variable eglf.


source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(parallel)
library(lattice)
library(ggplot2)
library(ecp)
library(changepoint)

MAKE_PLOTS <- TRUE
MAX_YEAR_EXTRAPOLATION <- 2016

cols <- c('egr_diff', # public employement rate
          # 'egr_lagged',
          ## 'YEAR',
          'QUARTER',
          'gdpv_annpct', # gdp growth
          'unr', # 'unemployment rate'
          # 'unr_lagged',
          'ypgtq_interpolated', # Total disburrsements, general government
          ## 'pop1574', # population proxy (aged from 15 to 74)
          'lpop_interpolated', # log population
          # 'country',
          ## 'ydrh' # net money income per capita
          # 'ydrh_to_gdpv', # interpolated value
          'gdp_per_capita'
          )

cols.gls <- c('egr', # public employement rate
              'YEAR', 'QUARTER',
              'gdpv_annpct', # gdp growth
              'unr', # 'unemployment rate'
              'ypgtq_interpolated',
              'lpop_interpolated',
              # 'gdp_per_capita_interpolated',
              'country')



eos <- readRDS('../data/eo-data.rds')
eo.desc <- readRDS('../data/eo-colnames-dt.rds')
setkey(eo.desc, VARIABLE) # enamble eo.desc['bsii'] => Balance of income, value, BOP basis

eos[[1]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.a
eos[[2]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.q
missing.country <- eos[[1]][, setdiff(unique(country), country.a)]

## Splines for interpoalting between years
eo.a <- copy(eos[[1]])
eo.q <- copy(eos[[2]])
eo.q <- Reduce(
  function(x, y) interpolateQuarterColumn(x, eo.a, y, MAX_YEAR_EXTRAPOLATION),
  c('unr', 'ypgtq', 'ydrh', 'pop1574'), init=eo.q)
eo.q[, ydrh_to_gdpv := ydrh_interpolated/gdpv]
eo.q[, lpop_interpolated:=log(pop1574_interpolated)]


################################################################################
## New Data
## Gathering of of additional data in order to make robustness analysis.
## The data are not a part of the oecd economic outlook data set.

new.data.names <- new.data <-
  c('gini', 'population', 'gdp_capita', 'imf_gfs_scores', 'gini_toth')

new.data %<>% {paste0('../data/', ., '_cleaned.csv')} %>% lapply(fread) %>%
  lapply(function(dt) {
    dt[, V1:=NULL]
    setnames(dt, colnames(dt), tolower(colnames(dt)))
    setnames(dt, 'time', 'TIME')
    dt[, TIME:=as.numeric(TIME)]
    setkeyv(dt, c('location', 'TIME'))}) %>% joinDataTable

setnames(new.data, 'location', 'country')
setkeyv(eo.a, c('country', 'TIME'))

# eo.q <- interpolateQuarterColumn(eo.q, new.data, 'gdp_per_capita', MAX_YEAR_EXTRAPOLATION)
# Patch the missing part of gdpv_annpct
# eo.q[is.na(gdpv_annpct), gdpv_annpct:=gdpv_annpct_interpolated]

DT <- fread('../data/execrlc_govfrac_yrcurnt_quartery_cleaned.csv')
DT[, V1:=NULL]
setnames(DT, 'location', 'country')
eo.q <- merge(eo.q, DT, by=c('country', 'TIME'), all=TRUE)

DT <- fread('../data/gdp_per_capita_quarterly_cleaned.csv')
DT[, V1:=NULL]
setnames(DT, 'location', 'country')
eo.q <- merge(eo.q, DT, by=c('country', 'TIME'), all=TRUE)

DT <- fread('../data/gdp_growth_quarterly_cleaned.csv')
DT[, V1:=NULL]
setnames(DT, 'location', 'country')
eo.q <- merge(eo.q, DT, by=c('country', 'TIME'), all=TRUE)

################################################################################
### Transformation of the data to create the data matrix

butlast <- function(x, k=1) x[1:(length(x)-k)]

### x is the data set with annual observation for eg
x <- copy(eo.q)
setkey(x, 'country')
x <- x[country.q]
x <- x[TIME< 2013 & TIME > 1989.75]
x[, country:=as.factor(country)]
time.numeric <- x$TIME
x[, TIME.NUMERIC:=time.numeric]
x[, TIME:=as.factor(TIME)]
x[, QUARTER:=as.factor(QUARTER)]
x[, YEAR:= as.factor(YEAR)]

x[, egr := 100*eg/lf] # et: General Government employment, lf: Total labor force
x[, egr_level_lagged:= c(NA, butlast(egr)), by='country'] # et: General Government employment, et: Total employment
x[, egr_diff:= c(NA, diff(egr)), by='country'] # et: General Government employment, et: Total employment
# x[, egr_diff:= 100*egr/shift(egr, 1), by='country'] # percent change
x[, egr_lagged:= c(NA, butlast(egr_diff)), by='country']
x[, egr_lagged_2:= c(NA, butlast(egr_lagged)), by='country']

x[, ydrh_to_gdpv:=100*ydrh/gdpv]
x[, ydrh_to_gdpv_diff:=c(NA, diff(ydrh_to_gdpv)), by='country']

x[, ypgtq_interpolated_diff:=c(NA, diff(ypgtq_interpolated)), by='country']

x[, gdp_per_capita_diff:=c(NA, diff(gdp_per_capita)), by='country']

x[, unr_lagged:=c(NA, butlast(unr)), by='country']
x[, unr_diff:=c(NA, diff(unr)), by='country']
x[, unr_diff_lagged:=c(NA, butlast(unr_diff)), by='country']

x[, gdpv_annpct_quarterly_lagged:=c(NA, butlast(gdpv_annpct_quarterly)), by='country']
x[, gdpv_annpct_quarterly_lagged_2:=c(NA, NA, butlast(gdpv_annpct_quarterly, 2)), by='country']

x <- x[!is.na(egr_diff)] # Non na observation

if (MAKE_PLOTS){
  dev.new()
  xyplot(egr ~ TIME.NUMERIC | country,
         na.omit(x[, c(cols, 'egr', 'country', 'TIME.NUMERIC'), with=F]),
         type='l')
  dev.new()
  xyplot(egr_diff~ TIME.NUMERIC | country,
         na.omit(x[, c(cols, 'egr', 'country', 'TIME.NUMERIC'), with=F]),
         type='l')
}

################################################################################
## Bai and Perron Filter: Structural break
## using ecp algorithm
if (MAKE_PLOTS){
  pdf('plot/egr_structural_breaks_mean_bai_perron.pdf', 16, 9)
  par(mfrow=c(5, 4))
  x[, {plot(cpt.mean(zoo(egr, order.by=TIME.NUMERIC), method='PELT'), main=.BY[[1]]); 0}, by='country']
  dev.off()

  pdf('plot/egr_structural_breaks_var_bai_perron.pdf', 16, 9)
  par(mfrow=c(5, 4))
  x[, {plot(cpt.var(zoo(egr, order.by=TIME.NUMERIC), method='PELT'), main=.BY[[1]]); 0}, by='country']
  dev.off()
}

x[, egr.cluster.16:=e.divisive(as.matrix(egr_diff, ncol=1), min.size=16)$cluster, by='country']
x[, egr.cluster.20:=e.divisive(as.matrix(egr_diff, ncol=1), min.size=20)$cluster, by='country']
x[, egr.cluster.24:=e.divisive(as.matrix(egr_diff, ncol=1), min.size=24)$cluster, by='country']

x.plot <- copy(x)
x.plot[, egr.cluster.16.chg:= c(0, as.numeric(diff(egr.cluster.16)==1))]
x.plot[, egr.cluster.20.chg:= c(0, as.numeric(diff(egr.cluster.20)==1))]
x.plot[, egr.cluster.24.chg:= c(0, as.numeric(diff(egr.cluster.24)==1))]

### Bad coding but this is tricky with data.table
if (MAKE_PLOTS){
  pdf('plot/egr_structural_breaks.pdf', 16, 9, TRUE)
  ## ggplot(x.plot, aes(TIME.NUMERIC, egr, color=factor(egr.cluster.16))) +
  ##   geom_line() +
  ##   geom_vline(aes(xintercept=vl),
  ##              x.plot[egr.cluster.16.chg==1, list(country, vl=TIME.NUMERIC)],
  ##              linetype='longdash') +
  ##   facet_wrap(~ country) + theme_bw() + scale_color_brewer(palette='Set1') +
  ##   ggtitle("Structural breaks for public employment (minimum cluster size: 4 years)")

  ## ggplot(x.plot, aes(TIME.NUMERIC, egr, color=factor(egr.cluster.20))) +
  ##   geom_line() +
  ##   geom_vline(aes(xintercept=vl),
  ##              x.plot[egr.cluster.20.chg==1, list(country, vl=TIME.NUMERIC)],
  ##              linetype='longdash') +
  ##   facet_wrap(~ country) + theme_bw() + scale_color_brewer(palette='Set1') +
  ##   ggtitle("Structural breaks for public employment (minimum cluster size: 5 years)")

  ggplot(x.plot, aes(TIME.NUMERIC, egr, color=factor(egr.cluster.24))) +
    geom_line() +
    geom_vline(aes(xintercept=vl),
               x.plot[egr.cluster.24.chg==1, list(country, vl=TIME.NUMERIC)],
               linetype='longdash') +
    facet_wrap(~ country) + theme_bw() + scale_color_brewer(palette='Set1') +
    ggtitle("Structural breaks for public employment (minimum cluster size: 6 years)") +
    labs(x='Time', y='Public Employment Rate', color='Breaks')

  ## ggplot(x.plot, aes(TIME.NUMERIC, egr, color=factor(egr.cluster.20))) +
  ##   geom_line() +
  ##   geom_vline(aes(xintercept=vl),
  ##              x.plot[egr.cluster.20.chg==1, list(country, vl=TIME.NUMERIC)],
  ##              linetype='longdash') +
  ##   facet_wrap(~ country, scale='free_y') + theme_bw() +
  ##   scale_color_brewer(palette='Set1') +
  ##   ggtitle("Structural breaks for public employment (minimum cluster size: 5 years)")

  ## ggplot(x.plot, aes(TIME.NUMERIC, egr_diff, color=factor(egr.cluster.20))) +
  ##   geom_line() +
  ##   geom_vline(aes(xintercept=vl),
  ##              x.plot[egr.cluster.20.chg==1, list(country, vl=TIME.NUMERIC)],
  ##              linetype='longdash') +
  ##   facet_wrap(~ country) + theme_bw() + scale_color_brewer(palette='Set1') +
  ##   ggtitle("Structural breaks in ploted with the difference (minimum cluster size: 5 years)")
  dev.off()
}
