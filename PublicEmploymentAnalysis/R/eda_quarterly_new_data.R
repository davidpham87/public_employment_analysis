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
  'gdp_per_capita_log',
  'gap_interpolated',
  'gdpv_yoy_annpct', # gdp growth
  'TIME',
  'QUARTER',
  'unr',
  'lpop_interpolated', # log population
  'government_revenue',# yrg over gdpvd
  # 'nlg_to_gdpv', # net landing in % of gdp
  'egr_lagged', # lagged public employment rate
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

time.numeric <- x$TIME
x[, TIME:=as.numeric(TIME)]
x[, QUARTER:=as.factor(QUARTER)]
x[, YEAR:= as.numeric(YEAR)]
# x[, egr := , by='country'] # et: General Government employment, et: Total employment
x[, egr_lagged:= c(NA, butlast(egr)), by='country']
x[, country:=as.factor(country)]

## Keep data for plotting
lvl2num <- function(x) as.numeric(levels(x)[x])
x.model.lm <- na.omit(x[, c(cols, 'country', 'TIME'), with=FALSE])
# x.model.lm$TIME <- lvl2num(x.model.lm$TIME)

## Simple lm model
x.lm <- lm(egr  ~ ., x[, c(cols), with=FALSE])
summary(x.lm)

y.fit.simple.lm <- fitted(x.lm)

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


## Fiscal Transparency
ff <- egr ~ .
imf.gfs.lm <- robustnessAnalysis(x, c(cols, 'fiscal_transparency_interpolated'), '', ff)
imf.gfs.wo.lm <- robustnessAnalysis(as.data.table(imf.gfs.lm$model), cols, '', ff)

## Left or right goverment
govrlc.lm <- robustnessAnalysis(x, c(cols, 'execrlc'), '', ff)
govrlc.wo.lm <- robustnessAnalysis(as.data.table(govrlc.lm$model), cols, '', ff)

## Years until election
yrcurnt.lm <- robustnessAnalysis(x, c(cols, 'yrcurnt'), '', ff)
yrcurnt.wo.lm <- robustnessAnalysis(as.data.table(yrcurnt.lm$model), cols, '', ff)

## Inequality
gini.red.lm <- robustnessAnalysis(x[TIME < 2010], c(cols, 'gini_red_abs'), '', ff)
gini.lm <- robustnessAnalysis(x[TIME < 2010], c(cols, 'gini_market_interpolated', 'gini_net_interpolated'), '', ff)
gini.wo.lm <- robustnessAnalysis(as.data.table(gini.lm$model), cols, '', ff)

## Net lending
nlg.lm <- robustnessAnalysis(x, c(cols, 'nlg_to_gdpv'), '', ff)
nlg.wo.lm <- robustnessAnalysis(as.data.table(nlg.lm$model), cols, '', ff)

## Government fractionalization
govfrac.lm <- robustnessAnalysis(x, c(cols, 'govfrac'), '', ff)
govfrac.wo.lm <- robustnessAnalysis(as.data.table(govfrac.lm$model), cols, '', ff)

## GDP GAP (gap_interpolated)
## gap.lm <- robustnessAnalysis(x, c(cols, 'gap_interpolated'), '', ff)
## gap.wo.lm <- robustnessAnalysis(as.data.table(gap.lm$model), cols, '', ff)

## Labor Force Trend Gap (gaplfp_interpolated)
gaplf.lm <- robustnessAnalysis(x, c(cols, 'gaplfp_interpolated'), '', ff)
gaplf.wo.lm <- robustnessAnalysis(as.data.table(gaplf.lm$model), cols, '', ff)

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
