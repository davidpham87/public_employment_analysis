source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(parallel)
library(lattice)

MAKE_PLOTS <- TRUE
MAX_YEAR_EXTRAPOLATION <- 2014

cols <- c(
  'egr',
  'egr_lagged', # lagged public employment rate
  'unr',
  'government_revenue',# yrg over gdpvd
  'nlg_to_gdp', # net landing in % of gdp
  'TIME',
  ## 'QUARTER',
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

x[, TIME:=as.factor(TIME)]
x[, QUARTER:=as.factor(QUARTER)]
x[, YEAR:= as.numeric(YEAR)]

x[, egr_lagged:= c(NA, butlast(egr)), by='country']
x[, country:=as.factor(country)]


x[, unr_lagged := c(NA, butlast(unr)), by='country']
x[, gdp_per_capita_log_lagged := c(NA, butlast(gdp_per_capita_log)), by='country']

## Keep data for plotting
lvl2num <- function(x) as.numeric(levels(x)[x])
x.model.lm <- na.omit(x[, c(cols, 'country', 'TIME'), with=FALSE])


x[, egr_country := scale(egr, center=TRUE, scale=FALSE), by='country']



## Simple lm model
x.lm <- lm(egr  ~ ., x[, c(cols), with=FALSE])
summary(x.lm)
showdiag(x.lm)

y.fit.simple.lm <- fitted(x.lm)

base.cols <- c('egr_lagged', 'government_revenue', 'nlg_to_gdp',
               'unr', 'country', 'TIME', 'egr')

gov.lm <- robustnessAnalysis(
  x, c('egr_lagged', 'government_revenue', 'country', 'TIME', 'egr'), '')
nlg.lm <- robustnessAnalysis(
  x, c('egr_lagged', 'nlg_to_gdp', 'country', 'TIME', 'egr'), '')
unr.lm <- robustnessAnalysis(
  x, c('egr_lagged', 'unr', 'country', 'TIME', 'egr'), '')

baseline.lm <- robustnessAnalysis(
  x, c('egr_lagged', 'government_revenue', 'nlg_to_gdp',
       'unr', 'country', 'TIME', 'egr'), '')

gdp.lm <- robustnessAnalysis(x, c(base.cols, 'gdpv_yoy_annpct'), '')
gdp.wo.lm <- robustnessAnalysis(as.data.table(gdp.lm$model), base.cols, '', ff)
# gdp.lm <- robustnessAnalysis(x, c('gdpv_yoy_annpct_lagged', base.cols), '')

gdp.capita.lm <- robustnessAnalysis(x, c(base.cols, 'gdp_per_capita_log'), '')
gdp.capita.wo.lm <- robustnessAnalysis(
  as.data.table(gdp.capita.lm$model), base.cols, '', ff)

open.lm <- robustnessAnalysis(x, c('openness', base.cols), '')
self.lm <- robustnessAnalysis(x, c('self_employment_rate', base.cols), '')

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
govrlc.base.lm <- robustnessAnalysis(x, c(base.cols, 'left'), '', ff)
govrlc.wo.lm <- robustnessAnalysis(as.data.table(govrlc.lm$model), cols, '', ff)

is_election.base.lm <- robustnessAnalysis(x, c('is_election_date', base.cols), '', ff)

## Years until election
nrr.lm <- robustnessAnalysis(
  x, c('natural_ressource_rent', 'is_election_date', base.cols), '', ff)

nrr.mix.lm <- robustnessAnalysis(
  x, c('natural_ressource_rent', 'is_election_date', base.cols), '',
  egr ~ . + natural_ressource_rent*is_election_date)

lassen.lm <- robustnessAnalysis(
  x.lassen, c(base.cols, 'fiscal_transparency_interpolated', 'is_election_date'), '',
  egr ~ . + is_election_date*fiscal_transparency_interpolated)

stconst.lm <- robustnessAnalysis(
  x, c('state_interpolated', 'is_election_date', base.cols), '', ff)

yrcurnt.lm <- robustnessAnalysis(x, c(cols, 'yrcurnt'), '', ff)
yrcurnt.wo.lm <- robustnessAnalysis(as.data.table(yrcurnt.lm$model), cols, '', ff)

pop.lm <- robustnessAnalysis(x, c(cols, 'lpop_interpolated'), '', ff) # log population
pop.wo.lm <- robustnessAnalysis(as.data.table(pop.lm$model), cols, '', ff)

## Inequality
y <- copy(x)
y[, TIME:=fctr2num(TIME)]
y <- y[TIME < 2010] # restrict time because of interpolation
y[, TIME:=as.factor(TIME)]

gini.red.lm <- robustnessAnalysis(y, c(cols, 'gini_red_abs'), '', ff)
gini.lm <- robustnessAnalysis(
  y, c(cols, 'gini_market_interpolated', 'gini_net_interpolated'), '', ff)
## gini.lm <- robustnessAnalysis(
##   y, c(cols, 'gini_red_abs', 'gini_red_rel'), '', ff)
gini.wo.lm <- robustnessAnalysis(
  as.data.table(gini.lm$model), cols, '', ff)

## Net lending
nlg.lm <- robustnessAnalysis(x, c(cols, 'nlg_to_gdp'), '', ff)
nlg.wo.lm <- robustnessAnalysis(as.data.table(nlg.lm$model), cols, '', ff)

## Government fractionalization
govfrac.lm <- robustnessAnalysis(x, c(cols, 'govfrac'), '', ff)
govfrac.base.lm <- robustnessAnalysis(x, c('govfrac', base.cols), '', ff)
govfrac.wo.lm <- robustnessAnalysis(
  as.data.table(govfrac.lm$model), cols, '', ff)

## Federalism
x[, muni_interpolated:=gsub("-999", NA, muni_interpolated)]
x[, state_interpolated:=gsub("-999", NA, state_interpolated)]
x[, state_interpolated:=gsub("No local elections",
                             0, state_interpolated)]
x[, state_interpolated:=gsub("Legislature locally elected",
                             1, state_interpolated)]
x[, state_interpolated:=gsub("Legislature and executive locally elected",
                             1, state_interpolated)]

fed.lm <- robustnessAnalysis(
  x, c(cols, 'muni_interpolated', 'auton_interpolated',
       'state_interpolated'), '',
  egr ~ . + muni_interpolated*gdpv_yoy_annpct_lagged +
    auton_interpolated*gdpv_yoy_annpct_lagged +
    state_interpolated*gdpv_yoy_annpct_lagged - state_interpolated)

##############################################################################

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
         nlg_to_gdp='Net Lending in percent of GDP',
         gini_red_abs='Diff. of Gini Market and Net Income',
         left='Government with social orientation'),
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

toTexModel <- function(li.lm, title, out, dep.name='Public employment rate'){
  cov.labs <- na.omit(queryList(description, names(coef(li.lm[[1]]))[-1]))

  argx <- c(li.lm, list(title=title, out=out, covariate.labels=cov.labs,
                        dep.var.labels=dep.name, omit=c('egr_lagged', 'TIME', 'country'),
                        omit.labels = c('Auto-correlation effect', 'Time effect', 'Country effect')))
  do.call(stargazer, argx)
}

dep.name <- 'Public employment rate'


toTexModel(list(x.lm, unr.lm, gov.lm, nlg.lm),
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
           'model_output/simple_lm_gini_red_quarterly.tex')
toTexModel(list(gdp.lm, gdp.wo.lm),
           'Effect of GDP',
           'model_output/simple_lm_gdp_quarterly.tex')
toTexModel(list(gdp.capita.lm, gdp.capita.wo.lm),
           'Effect of GDP per Capita',
           'model_output/simple_lm_gdp_capita_quarterly.tex')



## Plots
if (FALSE) {
  cols.plot <- cols[cols!='TIME']
  data.plot <- melt(x[, c(cols.plot, 'YEAR', 'QUARTER'), with=FALSE],
                    id.vars=c('country', 'YEAR', 'QUARTER'))

  quarter.substitute <-
    lapply(1:4, function(i) list(paste0('-Q', i), paste0('.', 100*(i-1)/4)))

  quarters.time <- Reduce(function(x, l) gsub(l[[1]], l[[2]], x),  quarter.substitute,
                          data.plot[, paste0(YEAR, '-', QUARTER)]) %>% as.numeric
  data.plot[, Time:=quarters.time]


  data.plot[, variable:=gsub('_', '\\\\_', variable)]

  data.plot[, {
    options(tikzDefaultEngine = 'pdftex')
    s <- paste0('plot/simple_model_quarterly_', .BY[[1]], '.tex')
    s <- gsub('\\', '', s, fixed=TRUE)
    gg2 <- ggplot(.SD, aes(Time, value)) + geom_line() + facet_wrap(~ country) +
      ggtitle(paste0(descriptions[[.BY[[1]]]], ' by country'))
    tikz(s, height=6, width=9)
    print(gg2)
    dev.off()
  }, by='variable']

  pdf('plot/model_diagnostic_quarterly.pdf', width=9, height=9)
  par(mfrow=c(2,2))
  plot(x.lm)
  dev.off()

}
