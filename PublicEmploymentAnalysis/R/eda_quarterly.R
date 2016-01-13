source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(parallel)

MAKE_PLOTS <- TRUE
MAX_YEAR_EXTRAPOLATION <- 2016

cols <- c('egr_diff', # public employement rate
          'egr_lagged',
          'YEAR',
          'QUARTER',
          'gdpv_annpct', # gdp growth
          'unr', # 'unemployment rate'
          'unr_lagged',
          'ypgtq_interpolated', # Total disburrsements, general government
          ## 'pop1574', # population proxy (aged from 15 to 74)
          'lpop_interpolated', # log population
          # 'country',
          ## 'ydrh' # net money income per capita
          # 'ydrh_to_gdpv', # interpolated value
          'gdp_per_capita_interpolated'
          )

eos <- readRDS('../data/eo-data.rds')
eo.desc <- readRDS('../data/eo-colnames-dt.rds')
setkey(eo.desc, VARIABLE) # enamble eo.desc['bsii'] => Balance of income, value, BOP basis

eos[[1]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.a
eos[[2]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.q
missing.country <- eos[[1]][, setdiff(unique(country), country.a)]

## Splines for interpoalting between years
eo.a <- copy(eos[[1]])
eo.q <- copy(eos[[2]])
eo.q <- Reduce(function(x, y) interpolateQuarterColumn(x, eo.a, y, MAX_YEAR_EXTRAPOLATION), c('unr', 'ypgtq', 'gdpv_annpct', 'ydrh', 'pop1574'), init=eo.q)
eo.q[, ydrh_to_gdpv := ydrh_interpolated/gdpv]
eo.q[, lpop_interpolated:=log(pop1574_interpolated)]


################################################################################
## New Data

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

eo.q <- interpolateQuarterColumn(eo.q, new.data, 'gdp_per_capita', MAX_YEAR_EXTRAPOLATION)
# Patch the missing part of gdpv_annpct
eo.q[is.na(gdpv_annpct), gdpv_annpct:=gdpv_annpct_interpolated]

################################################################################

# x is the data set with annual observation for eg
x <- eo.q
setkey(x, 'country')
x <- x[country.q]
x <- x[TIME< 2016]
time.numeric <- x$TIME
x[, TIME:=as.factor(TIME)]
x[, QUARTER:=as.factor(QUARTER)]
x[, YEAR:= as.numeric(YEAR)]
x[, egr := eg/et] # et: General Government employment, et: Total employment
x[, egr_diff:= c(NA, diff(egr)), by='country'] # et: General Government employment, et: Total employment
x[, egr_lagged:= c(NA, egr_diff[1:length(egr_diff)-1]), by='country'] # et: General Government employment, et: Total employment
x[, country:=as.factor(country)]
x[, ydrh_to_gdpv:=100*ydrh/gdpv]
x[, unr_lagged:=c(NA, unr[1:length(unr)-1])]
x <- x[!is.na(egr_diff)] # Non na observation


## Keep data for plotting
lvl2num <- function(x) as.numeric(levels(x)[x])
x.model.lm <- na.omit(x[, c(cols, 'country', 'TIME'), with=FALSE])
x.model.lm$TIME <- lvl2num(x.model.lm$TIME)

## Simple lm model
x.lm <- lm(egr_diff ~ ., x[, cols, with=FALSE])
summary(x.lm)
y.fit.simple.lm <- fitted(x.lm)

showdiag <- function(lm.obj){
  par(mfrow = c(2, 2))
  plot(lm.obj)
}
showdiag(x.lm)

## library(dynlm)
## cols.dynlm <- c('egr', # public employement rate
##                 'YEAR',
##                 'QUARTER',
##                 'gdpv_annpct', # gdp growth
##                 'unr', # 'unemployment rate'
##                 'ypgtq_interpolated', # Total disburrsements, general government
##                 ## 'pop1574', # population proxy (aged from 15 to 74)
##                 'lpop_interpolated', # log population
##                                         # 'country',
##                 ## 'ydrh' # net money income per capita
##                                         # 'ydrh_to_gdpv', # interpolated value
##                 'gdp_per_capita_interpolated'
##                 )
## DT <- copy(x[, cols.dynlm, with=FALSE])
## data.plm <- na.omit(as.data.frame(DT))
## form <- formula(terms(d(egr, 1) ~ ., data = data.plm))
## dform <- update(form, . ~ . + L(egr, 1) + L(unr, 1))

## data.plm <- do.call(cbind, lapply(data.plm, zoo))
## summary(x.dynlm <- dynlm(dform, data.plm))




robustnessAnalysis <- function(data, cols, to.drop, formula=egr ~ .){
  cols.extended <- unselectVector(cols, to.drop)
  x.lm <- lm(formula, data[, cols.extended, with=FALSE])
  print(summary(x.lm))
  x.lm
}


## Using the trick fiscal_transparencyscore*gdp_growth
x.new <- copy(x)
lassen <- fread('../data/lassen_fiscal_scores.csv')
x.lassen <- merge(x.new, lassen, by.x='country', by.y='ISO')

imf.gfs <- fread('../data/imf_gfs_scores.csv', header=TRUE) %>%
  {.[, list(ISO, `Index Score`)]}
setnames(imf.gfs, c('ISO', 'Index Score'), c('country', 'imf_gfs'))
x.imf.gfs <- merge(x.new, imf.gfs, by='country')

setnames(x.lassen, 'Index Score', 'fiscal_transparency_score')
setnames(x.imf.gfs, 'imf_gfs', 'fiscal_transparency_score')

ff <- egr_diff~ .
ff.fiscal <- egr_diff~ . - fiscal_transparency_score + fiscal_transparency_score*gdpv_annpct
lassen.lm <- robustnessAnalysis(x.lassen, c(cols, 'fiscal_transparency_score'), '',
                                ff.fiscal)
lassen.wo.lm <- robustnessAnalysis(as.data.table(lassen.lm$model), cols, '', ff)

imf.gfs.lm <- robustnessAnalysis(x.imf.gfs, c(cols, 'fiscal_transparency_score'), '',
                                ff.fiscal)
imf.gfs.wo.lm <- robustnessAnalysis(as.data.table(imf.gfs.lm$model), cols, '', ff)

par(mfrow = c(2,2))
plot(imf.gfs.lm)

################################################################################
### Residuals are really bad when using no difference
### New methods: diff all variable and study the difference



################################################################################
## Plots



descriptions <- list(`gdpv\\_annpct`='GDP growth',
                     unr='Unemployment rate',
                     ypgtq='Total disbursements, general government, in percent of GDP',
                     egr='Public employment rate',
                     lpop='Log of population in million',
                     `ydrh\\_to\\_gdpv`='Household net income, in percent of GDP',
                     `gdp\\_per\\_capita`='GDP per capita in USD Millions',
                     `fiscal\\_transparency`='IMF GFS Index',
                     incomeineq='Gini coefficient',
                     lpoptot='Log of total population in million',
                     'TIME'='Time',
                     egr_diff='Change in Public Employment Rate (CPER)',
                     egr_lagged='Lagged change in Public Employment Rate',
                     unr_lagged='Lagged Unemployment rate'
                     )

## Data plots
if (MAKE_PLOTS){
  data.plot <- melt(data.table(x.lm$model), id.vars=c('country', 'YEAR', 'QUARTER'))

  quarter.substitute <-
    lapply(1:4, function(i) list(paste0('-Q', i), paste0('.', 100*(i-1)/4)))

  quarters.time <- Reduce(function(x, l) gsub(l[[1]], l[[2]], x),  quarter.substitute,
                          data.plot[, paste0(YEAR, '-', QUARTER)]) %>% as.numeric
  data.plot[, TIME:=quarters.time]


  data.plot[, variable:=gsub('_', '\\\\_', variable)]

  data.plot[, {
    options(tikzDefaultEngine = 'pdftex')
    s <- paste0('plot/simple_model_quarterly_', .BY[[1]], '.tex')
    s <- gsub('\\', '', s, fixed=TRUE)
    gg2 <- ggplot(.SD, aes(TIME, value)) + geom_line() + facet_wrap(~ country) +
      ggtitle(paste0(descriptions[[.BY[[1]]]], ' by country'))
    tikz(s, height=6, width=9)
    print(gg2)
    dev.off()
  }, by='variable']

  tikz('plot/model_diagnostic_quarterly.tex', width=6, height=6)
  par(mfrow=c(2,2))
  plot(x.lm)
  dev.off()

  pdf('plot/model_diagnostic_quarterly.pdf', width=9, height=9)
  par(mfrow=c(2,2))
  plot(x.lm)
  dev.off()

}





################################################################################
### Plot
### Check quality of the interpolation
if (FALSE) {
  summary(x)
  par(mar=c(2.5, 10, 2.5, 2.5))
  barplot(sort(table(x$country)), horiz=T, las=2)

  compareValue(eo.q[country.q], int='gdpv_interpolated', quarter='gdpv')
  compareValue(eo.q[country.q], int='ypgtq_interpolated', quarter='ypgtq')
  compareValue(eo.q[country.q], int='unr_interpolated', quarter='unr')
  compareValue(eo.q[country.q], int='ydrh_interpolated', quarter='ydrh')


  colnames(x.model.lm) <- gsub('\\_', '\\\\_', colnames(x.model.lm))
  y.fit <- y.fit.simple.lm

  tikz('plot/model_fit_quality_quarterly.tex', width=6, height=6)
  gg <- compareValue(as.data.table(cbind(x.model.lm, y.fit)), y.fit='y.fit', egr='egr\\_diff')
  print(gg)
  dev.off()

}


################################################################################
### Output of model

queryList <- function(l, kx){
  kx %>%
    lapply(function(s) if(is.null(d <- l[[s]])) NA else d) %>%
    unlist
}

description <-
  c(list(gdpv_annpct='GDP growth',
         ydrh_to_gdpv='Household net income, in \\% of GDP',
         `gdp_per_capita_interpolated`='GDP per capita, in USD Millions (interpolated)',
         fiscal_transparency='IMF GFS Index',
         `ypgtq_interpolated`='Government expenditure in \\% of GDP (interpolated)',
         country='Country',
         `gdpv_annpct:fiscal_transparency_score`='Effect of fiscal transparency on GDP growth',
         fiscal_transparency_score='Fiscal Transparency',
         'gini_toth'='Gini coefficient (Toth 2015)',
         egr_diff='Difference with previous public employment rate (CPER)',
         egr_lagged='Lagged of difference in public employment rate',
         lpop_interpolated='Log of working population (interpolated)',
         QUARTER='Quarter',
         YEAR='Year'),
    descriptions)

x.lm$model$TIME <- lvl2num(x.lm$model$TIME)
queryList(description, colnames(x.lm$model)) %>% {
  stargazer(x.lm$model, out='model_output/simple_statistic_quarterly.tex',
            covariate.labels=.,
            font.size='footnotesize')
}

description[['egr_lagged']] <- NA
description[['YEAR']] <- NA

toTexModel <- function(li.lm, title, out, dep.names='Difference in Public employment rate'){
  cov.labs <- na.omit(queryList(description, names(coef(li.lm[[1]]))[-1]))

  argx <- c(li.lm, list(title=title, out=out, covariate.labels=cov.labs,
                        dep.var.labels=dep.name, omit=c('YEAR', 'country', 'egr_lagged', 'QUARTER'),
                        omit.labels = c('Year fixed-effect',
                                        'Country fixed-effect', 'Auto-correlation effect', 'Quarter effect')))
  do.call(stargazer, argx)
}

dep.name <- 'Public employment rate'
toTexModel(list(x.lm),
           'Main variable result',
           'model_output/simple_lm_quarterly.tex')
toTexModel(list(lassen.lm, lassen.wo.lm),
           'Effect of Lassen Fiscal Score',
           'model_output/simple_lm_lassen_quarterly.tex')
toTexModel(list(imf.gfs.lm, imf.gfs.wo.lm),
           'Effect of IMF GFS Score',
           'model_output/simple_lm_imf_quarterly.tex')
