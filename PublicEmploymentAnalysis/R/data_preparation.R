source('init.R')
pckgs <- loadPackages()
library(magrittr)
library(lattice)

butlast <- function(x, k=1) x[1:(length(x)-k)]

MAKE_PLOTS <- TRUE
MAX_YEAR_EXTRAPOLATION <- 2014

cols <- c(# 'egr_diff', # public employement rate
  'gdpvd',
  'gdp_per_capita_log',
  'gdpv_yoy_annpct', # gdp growth
  'QUARTER',
  'unr',
  'lpop_interpolated', # log population
  'government_revenue',# yrg over gdpvd
  'nlg_to_gdpv' # net landing in % of gdp
)

cols.to.save <- c(
  cols,
  'egr',
  'country',
  'YEAR',
  'fiscal_transparency_interpolated', # 'imf_gfs_scores'
  'gini_market_interpolated',
  'gini_net_interpolated',
  'gini_red_abs',
  'gini_red_abs',
  'gap_interpolated',
  'gaplfp_interpolated',
  'execrlc',
  'govfrac',
  'yrcurnt', # year until next election
  'is_election_date' # If the quarter is an election quarter
  ) %>% sort %>% {c('TIME', .)}


################################################################################
## Load data

eos <- readRDS('../data/eo-data.rds')
eo.desc <- readRDS('../data/eo-colnames-dt.rds')
setkey(eo.desc, VARIABLE) # enamble eo.desc['bsii'] => Balance of income, value, BOP basis
eos[[2]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.q # get non.missing country


################################################################################
## Splines for interpoalting between years

cols.interpolation.denton.cholette <-
  c('yrg', 'nlg', 'gdpv', 'gdpvd')

cols.interpolation.splines <-
  c('unr', 'gap', 'gaplfp')

eo.a <- copy(eos[[1]])
eo.q <- copy(eos[[2]])

pdf('plot/quarterly_vs_annual_levels', 12, 8)
xyplot(yrg ~ TIME | country, eo.a[country=='USA'], type='l', main='YRG quarterly')
xyplot(yrg ~ TIME | country, eo.q[country=='USA'], type='l', main='YRG annual')
dev.off()

## NOTE: the function add a _interpolated at the end of the variables in
## cols.interpolation

eo.q <- Reduce(
  function(x, y) interpolateQuarterColumn(x, eo.a, y, MAX_YEAR_EXTRAPOLATION, 'denton-cholette'),
  cols.interpolation.denton.cholette, init=eo.q)


eo.q <- Reduce(
  function(x, y) interpolateQuarterColumn(x, eo.a, y, MAX_YEAR_EXTRAPOLATION),
  cols.interpolation.splines, init=eo.q)

################################################################################
## Patching quarterly data

for (col in c(cols.interpolation.denton.cholette, cols.interpolation.splines)){
  try({
    eo.q[is.na(get(col)), (col):= get(paste0(col, '_interpolated'))]
  })
}

eo.q[, gdpv_yoy_annpct:=c(NA, NA, NA, NA,
                      100*gdpv[-(1:4)]/butlast(gdpv, 4)-100), by='country']

################################################################################
## New Data
## Gathering of of additional data in order to make robustness analysis.
## The data are not a part of the oecd economic outlook data set.
## SWIID provides measures of gini

new.data.names <- new.data <-
  c('population', 'imf_gfs_scores', 'SWIID')

# SWIID

new.data %<>% {paste0('../data/', ., '_cleaned.csv')} %>% lapply(fread) %>%
  lapply(function(dt) {
    dt[, V1:=NULL]
    setnames(dt, colnames(dt), tolower(colnames(dt)))
    setnames(dt, 'time', 'TIME')
    dt[, TIME:=as.numeric(TIME)]
    setkeyv(dt, c('location', 'TIME'))}) %>% joinDataTable

setnames(new.data, 'location', 'country')
setkeyv(eo.a, c('country', 'TIME'))

for (col in c('pop', 'gini_net', 'gini_market', 'fiscal_transparency')){
  eo.q <- interpolateQuarterColumn(eo.q, new.data, col, MAX_YEAR_EXTRAPOLATION)
}

eo.q[, lpop_interpolated:=log(pop_interpolated)]
eo.q[, gini_red_abs:=(gini_market_interpolated - gini_net_interpolated)]
eo.q[, gini_red_rel:=100*gini_red_abs/gini_net_interpolated]

DT <- fread('../data/execrlc_govfrac_yrcurnt_quartery_cleaned.csv')
DT[, V1:=NULL]
setnames(DT, 'location', 'country')
eo.q <- merge(eo.q, DT, by=c('country', 'TIME'), all=TRUE)

################################################################################
### Transformation of the data to create the data matrix

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

x[, government_revenue:=100*yrg_interpolated/gdpv_interpolated, by='country']
x[, nlg_to_gdpv:=100*nlg_interpolated/gdpv, by='country']

x[, gdp_per_capita:=gdpvd/pop_interpolated/1e6]
x[, gdp_per_capita_log:=log(gdp_per_capita)]

################################################################################
## LAGs might be useful in the future

## x[, ypgtq_interpolated_diff:=c(NA, diff(ypgtq_interpolated)), by='country']

## x[, gdp_per_capita_diff:=c(NA, diff(gdp_per_capita)), by='country']

## x[, egr_level_lagged:= c(NA, butlast(egr)), by='country'] # et: General Government employment, et: Total employment
## x[, egr_diff:= c(NA, diff(egr)), by='country'] # et: General Government employment, et: Total employment
## x[, egr_diff:= 100*egr/shift(egr, 1), by='country'] # percent change
## x[, egr_lagged:= c(NA, butlast(egr_diff)), by='country']
## x[, egr_lagged_2:= c(NA, butlast(egr_lagged)), by='country']

## x[, ydrh_to_gdpv_diff:=c(NA, diff(ydrh_to_gdpv)), by='country']

## x[, unr_lagged:=c(NA, butlast(unr)), by='country']
## x[, unr_diff:=c(NA, diff(unr)), by='country']
## x[, unr_diff_lagged:=c(NA, butlast(unr_diff)), by='country']

# x[, gdpv_annpct_quarterly_lagged:=c(NA, butlast(gdpv_annpct_quarterly)), by='country']
# x[, gdpv_annpct_quarterly_lagged_2:=c(NA, NA, butlast(gdpv_annpct_quarterly, 2)), by='country']

################################################################################
## Remove NAs
x <- x[!is.na(egr)] # Non na observation

################################################################################
## Diagnostic plots

if (MAKE_PLOTS){

  pdf('plot/variable_validation_check.pdf', 12, 7, onefile=TRUE)

  xyplot(egr ~ TIME.NUMERIC | country,
         na.omit(x[, c(cols, 'egr', 'country', 'TIME.NUMERIC'), with=F]),
         type='l', main='Public Employment in percent of labor force')

  xyplot(gdpv_yoy_annpct ~ TIME.NUMERIC | country,
       x[, c('gdpv_yoy_annpct', 'country', 'TIME.NUMERIC'), with=F],
       type='l', main='GDP Growth, YoY in %')

  xyplot(gap_interpolated ~ TIME.NUMERIC | country,
       x[, c(cols, 'gap_interpolated', 'country', 'TIME.NUMERIC'), with=F],
       type='l', main='GDP Output GAP in %' )

  xyplot(gaplfp_interpolated ~ TIME.NUMERIC | country,
         x[, c(cols, 'gaplfp_interpolated', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='Labor gap')

  xyplot(gdpvd ~ TIME.NUMERIC | country,
         x[, c('gdpvd', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='GDPVD Quartery')

  xyplot(log(gdpvd) ~ TIME.NUMERIC | country,
         x[, c('gdpvd', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='GDPVD Quarterly (log)')

  xyplot(government_revenue ~ TIME.NUMERIC | country,
         x[, c('government_revenue', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='Government Revenue')

  xyplot(gdp_per_capita_log ~ TIME.NUMERIC | country,
         x[, c(cols, 'gdp_per_capita_log', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='GDP per capita (log)')

  xyplot(gdp_per_capita ~ TIME.NUMERIC | country,
         x[, c(cols, 'gdp_per_capita', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='GDP per capita')


  xyplot(government_revenue~ TIME.NUMERIC | country,
         x[, c(cols, 'government_revenue', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='Government revenue')

  ## For Gini: JPN and CAD -> Data stops in 2007. Hence the number afterwards
  ## are not trustable.

  xyplot(gini_net_interpolated ~ TIME.NUMERIC | country,
         x[, c('gini_net_interpolated', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='Gini Net (post-tax and post subsidies)')

  xyplot(gini_market_interpolated ~ TIME.NUMERIC | country,
         x[, c('gini_market_interpolated', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='Gini Market (pre-tax and pre-subsidies)')

  xyplot(gini_red_abs ~ TIME.NUMERIC | country,
         x[, c('gini_red_abs', 'country', 'TIME.NUMERIC'), with=F], type='l',
         main='Gini Reduction, Difference between pre and post tax/subsidies, Absolute')

  xyplot(gini_red_rel ~ TIME.NUMERIC | country,
         x[, c('gini_red_rel', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='Gini Reduction (Relative)')

  xyplot(fiscal_transparency_interpolated ~ TIME.NUMERIC | country,
         x[, c('fiscal_transparency_interpolated', 'country', 'TIME.NUMERIC'), with=F],
         type='l', main='IMF Fiscal Transparency Score')


  dev.off()
}


################################################################################
## Save into CSV

write.csv(x, '../data/public_employment_data_all.csv')
write.csv(x[, cols.to.save, with=F], '../data/public_employment_design_matrix.csv')
