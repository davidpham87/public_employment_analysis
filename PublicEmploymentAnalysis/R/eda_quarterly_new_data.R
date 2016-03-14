source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(parallel)
library(lattice)

butlast <- function(x, k=1) x[1:(length(x)-k)]

MAKE_PLOTS <- TRUE
MAX_YEAR_EXTRAPOLATION <- 2014

cols <- c(# 'egr_diff', # public employement rate
  ## 'gdpvd',
  'egr',
  'egr_lagged', # lagged public employment rate
  'gdp_per_capita_log',
  ## 'gdp_per_capita_log_lagged',
  # 'gap_interpolated',
  ## 'gdpv_yoy_annpct', # gdp growth
  'gdpv_yoy_annpct_lagged',
  ##  'gdpv_yoy_annpct_lagged_2',
  'unr',
  # 'unr_lagged',
  'government_revenue',# yrg over gdpvd
  'nlg_to_gdp', # net landing in % of gdp
  'wage_share',
  'openness',
  'self_employment_rate',
  ## 'lpop_interpolated',
  'TIME',
  'QUARTER',
  'country'
)

cols.to.save <- c(
  cols,
  'egr',
  'country',
  'YEAR',
  'nlg_to_gdpv',
  'fiscal_transparency_interpolated', # 'imf_gfs_scores'
  'gini_market_interpolated',
  'gini_net_interpolated',
  'gini_red_abs',
  'gini_red_rel',
  'gap_interpolated',
  'gaplfp_interpolated',
  'execrlc',
  'govfrac',
  'yrcurnt', # year until next election
  'is_election_date' # If the quarter is an election quarter
  ) %>% sort %>% {c('TIME', .)}


# fread('../data/public_employment_data_all.csv')
x <- fread('../data/public_employment_design_matrix.csv')
head(x)
x[, V1:=NULL]
setkey(x, 'country')

x[, gdp_per_capita_log:= log(gdp_per_capita)]
x[, gdpv_yoy_annpct_lagged:= c(NA, butlast(gdpv_yoy_annpct)), by='country']
x[, gdpv_yoy_annpct_lagged_2:=c(NA, NA, butlast(gdpv_yoy_annpct, 2)), by='country']
x[, lpop_interpolated:=log(population_interpolated)]

time.numeric <- x$TIME
x[, TIME:=as.numeric(TIME)]
x <- x[TIME < 2013 & TIME > 1989.75]
x[, QUARTER:=as.factor(QUARTER)]
x[, YEAR:= as.numeric(YEAR)]
# x[, egr := , by='country'] # et: General Government employment, et: Total employment
x[, egr_lagged:= c(NA, butlast(egr)), by='country']
x[, country:=as.factor(country)]

# x[, egr := c(NA, diff(egr)), by='country'] # Diff only
x[, unr_lagged := c(NA, butlast(unr)), by='country']
x[, gdp_per_capita_log_lagged := c(NA, butlast(gdp_per_capita_log)), by='country']

## Keep data for plotting
lvl2num <- function(x) as.numeric(levels(x)[x])
x.model.lm <- na.omit(x[, c(cols, 'country', 'TIME'), with=FALSE])
# x.model.lm$TIME <- lvl2num(x.model.lm$TIME)

x[, egr_country := scale(egr, center=TRUE, scale=FALSE), by='country']
# histogram(~ egr_country, x[TIME>1990 & TIME < 2012], breaks=seq(-2, 2, length.out=100))

## Simple lm model
x.lm <- lm(egr  ~ ., x[, c(cols), with=FALSE])
summary(x.lm)

y.fit.simple.lm <- fitted(x.lm)

# xyplot(government_revenue ~ TIME | country, x, type='l')

showdiag <- function(lm.obj){
  par(mfrow = c(2, 2))
  plot(lm.obj)
}

showdiag(x.lm)

robustnessAnalysis <- function(data, cols, to.drop, formula=egr ~ .){
  cols.extended <- unselectVector(cols, to.drop)
  x.lm <- lm(formula, data[, cols.extended, with=FALSE])
  print(summary(x.lm))
  x.lm
}


base.cols <- c('government_revenue', 'nlg_to_gdp', 'unr', 'country', 'TIME', 'egr')

gov.lm <- robustnessAnalysis(x, c('government_revenue', 'country', 'TIME', 'egr'), '')
nlg.lm <- robustnessAnalysis(x, c('nlg_to_gdp', 'country', 'TIME', 'egr'), '')
unr.lm <- robustnessAnalysis(x, c('unr', 'country', 'TIME', 'egr'), '')

baseline.lm <- robustnessAnalysis(x, c('government_revenue', 'nlg_to_gdp', 'unr', 'country', 'TIME', 'egr'), '')

gdp.lm <- robustnessAnalysis(x, c('gdpv_yoy_annpct', base.cols), '')
gdp.lm <- robustnessAnalysis(x, c('gdpv_yoy_annpct_lagged', base.cols), '')

gdp_capita.lm <- robustnessAnalysis(x, c('gdp_per_capita_log', base.cols), '')
wage.lm <- robustnessAnalysis(x, c('wage_share', base.cols), '')

open.lm <- robustnessAnalysis(x, c('openness', base.cols), '')


## Fiscal Transparency
ff <- egr ~ .
imf.gfs.lm <- robustnessAnalysis(x, c(cols, 'fiscal_transparency_interpolated'), '', ff)
imf.gfs.wo.lm <- robustnessAnalysis(as.data.table(imf.gfs.lm$model), cols, '', ff)

## Lassen
x.new <- copy(x)
lassen <- fread('../data/lassen_fiscal_scores.csv')
setnames(lassen, 'Index Score', 'lassen_score')
x.lassen <- merge(x.new, lassen, by.x='country', by.y='ISO')
lassen.lm <- robustnessAnalysis(x.lassen, c(cols, 'lassen_score', 'left'), '',
                                egr ~ . + left*lassen_score + left - lassen_score)

## Left or right goverment
govrlc.lm <- robustnessAnalysis(x, c(cols, 'left'), '', ff)
govrlc.base.lm <- robustnessAnalysis(x, c('left', base.cols), '', ff)
govrlc.wo.lm <- robustnessAnalysis(as.data.table(govrlc.lm$model), cols, '', ff)

is_election.base.lm <- robustnessAnalysis(x, c('is_election_date', base.cols), '', ff)

## Years until election
nrr.lm <- robustnessAnalysis(x, c('natural_ressource_rent', 'is_election_date', base.cols), '', ff)
nrr.mix.lm <- robustnessAnalysis(x, c('natural_ressource_rent', 'is_election_date', base.cols), '',
                                 egr ~ . + natural_ressource_rent*is_election_date)

lassen.lm <- robustnessAnalysis(x.lassen, c(base.cols, 'fiscal_transparency_interpolated', 'is_election_date'), '',
                                egr ~ . + is_election_date*fiscal_transparency_interpolated)

stconst.lm <- robustnessAnalysis(x, c('state_interpolated', 'is_election_date', base.cols), '', ff)


yrcurnt.lm <- robustnessAnalysis(x, c(cols, 'yrcurnt'), '', ff)
yrcurnt.wo.lm <- robustnessAnalysis(as.data.table(yrcurnt.lm$model), cols, '', ff)

pop.lm <- robustnessAnalysis(x, c(cols, 'lpop_interpolated'), '', ff) # log population
pop.wo.lm <- robustnessAnalysis(as.data.table(pop.lm$model), cols, '', ff)

## Inequality
gini.red.lm <- robustnessAnalysis(x[TIME < 2010], c(cols, 'gini_red_abs'), '', ff)
gini.lm <- robustnessAnalysis(x[TIME < 2010], c(cols, 'gini_market_interpolated', 'gini_net_interpolated'), '', ff)
gini.lm <- robustnessAnalysis(x[TIME < 2010], c(cols, 'gini_market_interpolated', 'gini_net_interpolated', 'gini_red_rel'), '', ff)
gini.wo.lm <- robustnessAnalysis(as.data.table(gini.lm$model), cols, '', ff)

## Net lending
nlg.lm <- robustnessAnalysis(x, c(cols, 'nlg_to_gdp'), '', ff)
nlg.wo.lm <- robustnessAnalysis(as.data.table(nlg.lm$model), cols, '', ff)

## Government fractionalization
govfrac.lm <- robustnessAnalysis(x, c(cols, 'govfrac'), '', ff)
govfrac.base.lm <- robustnessAnalysis(x, c('govfrac', base.cols), '', ff)
govfrac.wo.lm <- robustnessAnalysis(as.data.table(govfrac.lm$model), cols, '', ff)

## Federalism
x[, muni_interpolated:=gsub("-999", NA, muni_interpolated)]
x[, state_interpolated:=gsub("-999", NA, state_interpolated)]
x[, state_interpolated:=gsub("No local elections", 0, state_interpolated)]
x[, state_interpolated:=gsub("Legislature locally elected", 1, state_interpolated)]
x[, state_interpolated:=gsub("Legislature and executive locally elected", 1, state_interpolated)]

fed.lm <- robustnessAnalysis(x, c(cols, 'muni_interpolated', 'auton_interpolated', 'state_interpolated'), '',
                             egr ~ . + muni_interpolated*gdpv_yoy_annpct_lagged + auton_interpolated*gdpv_yoy_annpct_lagged +
                               state_interpolated*gdpv_yoy_annpct_lagged - state_interpolated)

## GDP GAP (gap_interpolated)
## gap.lm <- robustnessAnalysis(x, c(cols, 'gap_interpolated'), '', ff)
## gap.wo.lm <- robustnessAnalysis(as.data.table(gap.lm$model), cols, '', ff)

## Labor Force Trend Gap (gaplfp_interpolated)
gaplf.lm <- robustnessAnalysis(x, c(cols, 'gaplfp_interpolated'), '', ff)
gaplf.wo.lm <- robustnessAnalysis(as.data.table(gaplf.lm$model), cols, '', ff)

all.lm <- robustnessAnalysis(x, c(cols, 'gaplfp_interpolated', 'nlg_to_gdp', 'left', 'yrcurnt'), '', ff)

natural.lm <- robustnessAnalysis(x, c(cols, 'natural_ressource_rent'), '', ff)
natural.wo.lm <- robustnessAnalysis(as.data.table(natural.lm$model), cols, '', ff)

## REST



################################################################################
### Residuals are really bad when using no difference
### New methods: diff all variable and study the difference

################################################################################
## Plots

descriptions <-
  list(`gdpv\\_yoy\\_annpct`='GDP growth, YoY in percent',
       unr='Unemployment rate',
       ypgtq='Total disbursements, general government, in percent of GDP',
       egr='Public employment rate',
       lpop='Log of population in million',
       `lpop\\_interpolated` ='Log of population in million',
       `ydrh\\_to\\_gdpv`='Household net income, in percent of GDP',
       `gdp\\_per\\_capita`='GDP per capita in USD Millions',
       `fiscal\\_transparency`='IMF GFS Index',
       incomeineq='Gini coefficient',
       lpoptot='Log of total population in million',
       'TIME'='Time',
       egr_diff='Change in Public Employment Rate (CPER)',
       `egr\\_lagged`='Lagged public employment rate',
       ## egr_lagged='Lagged change in Public Employment Rate',
       unr_lagged='Lagged unemployment rate',
       `government\\_revenue`='Government Revenue',
       `gdp\\_per\\_capita\\_log`='GDP per capita, 2010 PPP, in USD/person',
       'nlg\\_to\\_gdpv'='Net Lending, in percent of GDP',
       'gap_interpolated'='Output Gap in percent',
       'gdpv_yoy_annpct'='GDP growth, YoY in percent',
       'egr\\_lagged'='Public employment rate (1 Quarter Lag)'
       )


description <-
  c(list(gdpv_annpct='GDP growth',
         ydrh_to_gdpv='Household net income, in \\% of GDP',
         `gdp_per_capita_log`='Log of GDP per capita, in USD Millions',
         fiscal_transparency_interpolated='IMF GFS Index',
         `ypgtq_interpolated`='Government expenditure in \\% of GDP (interpolated)',
         country='Country',
         `gdpv_annpct:fiscal_transparency_score`='Effect of fiscal transparency on GDP growth',
         fiscal_transparency_score='Fiscal Transparency',
         'gini_toth'='Gini coefficient (Toth 2015)',
         egr_diff='Difference with previous public employment rate (CPER)',
         egr_lagged='Lagged of difference in public employment rate',
         lpop_interpolated='Log of population in million',
         QUARTER='Quarter',
         YEAR='Year',
         TIME='Time',
         government_revenue='Government Revenue',
         execrlc='Left Orientated Government',
         yrcurnt='Years until next election',
         gini_market_interpolated='Gini Coefficient, Market Income',
         gini_net_interpolated='Gini Coefficient, Net Income',
         govfrac='Government Fractionalization',
         nlg_to_gdpv='Net Lending in percent of GDP',
         gini_red_abs='Diff. of Gini Market and Net Income'),
    descriptions)



queryList <- function(l, kx){
  kx %>%
    lapply(function(s) if(is.null(d <- l[[s]])) NA else d) %>%
    unlist
}


x.lm$model$TIME <- lvl2num(x.lm$model$TIME)
queryList(description, colnames(x.lm$model)) %>% {
  stargazer(x.lm$model, out='model_output/simple_statistic_quarterly.tex',
            covariate.labels=.,
            font.size='footnotesize', title='Data statistics')
}

description[['egr_lagged']] <- NA
description[['YEAR']] <- NA

toTexModel <- function(li.lm, title, out, dep.name='Difference in public employment rate'){
  cov.labs <- na.omit(queryList(description, names(coef(li.lm[[1]]))[-1]))

  argx <- c(li.lm, list(title=title, out=out, covariate.labels=cov.labs,
                        dep.var.labels=dep.name, omit=c('egr_lagged', 'QUARTER', 'country'),
                        omit.labels = c('Auto-correlation effect', 'Seasonal effect', 'Country effect')))
  do.call(stargazer, argx)
}

dep.name <- 'Public employment rate'
toTexModel(list(x.lm),
           'Main variable result',
           'model_output/simple_lm_quarterly.tex')
toTexModel(list(imf.gfs.lm, imf.gfs.wo.lm),
           'Effect of IMF GFS Score',
           'model_output/simple_lm_imf_quarterly.tex')
toTexModel(list(govrlc.lm, govrlc.wo.lm),
           'Effect of Government Political Orientation',
           'model_output/simple_lm_govrlc_quarterly.tex')
toTexModel(list(govfrac.lm, govfrac.wo.lm),
           'Effect of Government Fractionalization',
           'model_output/simple_lm_govfrac_quarterly.tex')
toTexModel(list(yrcurnt.lm, yrcurnt.wo.lm),
           'Effect of Years until next Election',
           'model_output/simple_lm_yrcurnt_quarterly.tex')
toTexModel(list(gini.lm, gini.wo.lm),
           'Effect of Gini coefficient, data up to 2010 (included)',
           'model_output/simple_lm_gini_quarterly.tex')
toTexModel(list(gini.red.lm, gini.wo.lm),
           'Effect of Difference of Gini coefficient (Market and Net), data up to 2010 (included)',
           'model_output/simple_lm_gini_red__quarterly.tex')
toTexModel(list(nlg.lm, nlg.wo.lm),
           'Effect of Net Landing',
           'model_output/simple_lm_nlg_quarterly.tex')


## Years until election

library(readstata13)
library(data.table)
data_used <- as.data.table(read.dta13('../data/data_used.dta'))
## summary(lm(eglf ~ ., data_used[date > list(eglf, date, nlgq, yrgq, unlf, NAMES_STD, elec)]))
x.lm <- lm(eglf ~ ., data_used[date > 140, list(eglf, nlgq, yrgq, unlf, gdpv_yoy_lagged, date, NAMES_STD)])
summary(x.lm)
# p.adjust(summary(x.lm)$coefficient[, 4])

# summary(lm(eglf ~ . + elec*parlsys, data_used[date > 140, list(eglf, date, nlgq, unlf, yrgq, NAMES_STD, elec, parlsys)]))
# summary(lm(eglf ~ . , data_used[date > 140, list(eglf, date, nlgq, unlf, yrgq, NAMES_STD, elec)]))
