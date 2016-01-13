source('init.R')
pckgs <- loadPackages()

MAKE_PLOT <- FALSE

cols <- c('egr', # public employement rate
          'TIME', # year
          'gdpv_annpct', # gdp growth
          'unr', # 'unemployment rate'
          'ypgtq', # Total disburrsements, general government percent of GDP
          ## 'pop1574', # population proxy (aged from 15 to 74) # absolute number
          'lpop', # log population -> take approximation
          'country',
          'ydrh_to_gdpv' # Ratio of net money income per capita to gdp (volume)
          )

eos <- readRDS('../data/eo-data.rds')
eo.desc <- readRDS('../data/eo-colnames-dt.rds')
setkey(eo.desc, VARIABLE) # enable eo.desc['bsii'] => Balance of income, value, BOP basis

## change time
setkey(eos[[1]], 'country')
eos[[1]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.a
missing.country <- eos[[1]][, setdiff(unique(country), country.a)]


x <- eos[[1]][country.a]
x[, egr := 100*eg/et] # et: General Government employment, et: Total employment
time.numeric <- x$TIME
x[, TIME:=as.factor(TIME)]
x[, country:=as.factor(country)]
x[, lpop := log(pop1574/1e6)] # log pop of millions
x[, ydrh_to_gdpv:=100*ydrh/gdpv]
x <- x[!is.na(egr)] # Non na observation

library(mgcv)
library(sfsmisc)
library(nlme)

## Expand the formula

x.lm <- lm(egr ~ ., data=x[, cols, with=FALSE])
x.lm.s <- summary(x.lm)



## x.gam <- gam(ff, data=x[, cols, with=FALSE])
## plot(x.gam)
## gam.check(x.gam)

## dev.new()
## par(mfrow=c(5,5))
## x[, {pacf(egr, main=.BY[[1]]);0}, by='country']
x[, TIME:=as.numeric(TIME)][, TIME:=time.numeric]
form <- formula(terms(egr ~ ., data = x[, cols, with=FALSE]))
x.gls <- gls(form, data=na.omit(x[, cols, with=FALSE]),
             correlation=corAR1(0.9, ~ 1 | country, TRUE))
anova(x.gls)
eo.desc[cols]
x.simple.model <- as.data.table(x.lm$model)

################################################################################
## Additional Data

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
setkeyv(x, c('country', 'TIME'))

imf.gfs <- fread('../data/imf_gfs_scores.csv', header=TRUE) %>%
  {.[, list(ISO, `Index Score`)]}
setnames(imf.gfs, c('ISO', 'Index Score'), c('country', 'imf_gfs'))

x.new <- new.data[J(x)]
x.new[, lpoptot:=log(pop)]
x.new[, pop:=NULL]
x.new <- merge(x.new, imf.gfs, by='country') # add img_gfs fiscal_scores

new.var.names <- list(incomeineq='gini', pop='population',
                      gdp_per_capita='gdp_per_capita',
                      fiscal_transparency='fiscal_transparency')

################################################################################
### Robustness Analysis

new.cols <- c('imf_gfs', 'incomeineq', 'lpoptot', 'gdp_per_capita')
cols.extended <- c(cols, new.cols)
# x.new[, fiscal_transparency:=scale(imf_gfs, scale=FALSE)]
x.new[, imf_gfs:=scale(imf_gfs, scale=TRUE)]

robustnessAnalysis <- function(data, cols, to.drop, formula=egr ~ .){
  cols.extended <- unselectVector(cols, to.drop)
  ## x.lm <- gls(formula, data=na.omit(data[, cols, with=FALSE]),
  ##             correlation=corAR1(0.9, ~ 1 | country, TRUE))
  ## print(anova(x.lm))
  x.lm <- lm(formula, data[, cols.extended, with=FALSE])
  print(summary(x.lm))
  x.lm
}

# All variables
ff <- egr ~ .  #+ imf_gfs*gdpv_annpct
x.new[, TIME:=as.factor(TIME)]

## W/o countries
print(cols)
country.wo.lm <- robustnessAnalysis(x.new, cols, 'country', ff)

## Not so much difference, but loss of 150 data points, so keep lpop.
lpoptot.lm <- robustnessAnalysis(x.new, c(cols, 'lpoptot'), 'lpop', ff)
lpop.lm <- completeLmData(lpoptot.lm, x.new, 'lpop') %>%
  robustnessAnalysis(c(unselectVector(cols, 'lpop'), 'lpop'), 'lpoptot', ff)

## Income inquality is not significant and also limit the size of the dataset
## gdp_per_capita and lpop are represented by ydrh and lpoptot, hence we can drop them
incomeineq.lm <- robustnessAnalysis(x.new, c(cols, 'incomeineq'), '', ff)
incomeineq.wo.lm <- as.data.table(incomeineq.lm$model) %>%
  robustnessAnalysis(cols, '', ff)

gini.lm <- robustnessAnalysis(x.new, c(cols, 'gini_toth'), '', ff)
gini.wo.lm <- robustnessAnalysis(as.data.table(gini.lm$model), cols, '', ff)


## gdp_per_capita seems to be significant # Loss of 500 observations though
gdp.per.capita.lm <- robustnessAnalysis(x.new, c(cols, 'gdp_per_capita'), '', ff)
gdp.per.capita.wo.lm <- as.data.table(gdp.per.capita.lm$model) %>%
  robustnessAnalysis(cols, '', ff)

## Fiscal Transparency
fiscal.lm <- robustnessAnalysis(x.new, c(cols, 'fiscal_transparency'), '', ff)
fiscal.wo.lm <- as.data.table(fiscal.lm$model) %>%
  robustnessAnalysis(c(cols), '', ff)

## Using the trick fiscal_transparencyscore*gdp_growth
lassen <- fread('../data/lassen_fiscal_scores.csv')
x.lassen <- merge(x.new, lassen, by.x='country', by.y='ISO')
x.imf.gfs <- merge(x.new, imf.gfs, by='country')
setnames(x.lassen, 'Index Score', 'fiscal_transparency_score')
setnames(x.imf.gfs, 'imf_gfs.y', 'fiscal_transparency_score')

ff.fiscal <- egr ~ . - fiscal_transparency_score + fiscal_transparency_score*gdpv_annpct
lassen.lm <- robustnessAnalysis(x.lassen, c(cols, 'fiscal_transparency_score'), '',
                                ff.fiscal)
lassen.wo.lm <- robustnessAnalysis(as.data.table(lassen.lm$model), cols, '', ff)

imf.gfs.lm <- robustnessAnalysis(x.imf.gfs, c(cols, 'fiscal_transparency_score'), '',
                                ff.fiscal)
imf.gfs.wo.lm <- robustnessAnalysis(as.data.table(imf.gfs.lm$model), cols, '', ff)

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
                     'TIME'='Time')

## Data plots
if (MAKE_PLOT){

  data.plot <-   x.new[, c(cols.extended, 'fiscal_transparency'), with=FALSE] %>%
    melt(id.vars=c('country', 'TIME'))
  data.plot[, variable:=gsub('_', '\\\\_', variable)] # LaTeX Escaping

  ## List of variable names and their corresponding title

  ## Plot
  data.plot[, {
    options(tikzDefaultEngine = 'pdftex')
    s <- paste0('plot/simple_model_', .BY[[1]], '.tex')
    s <- gsub('\\', '', s, fixed=TRUE)
    gg2 <- ggplot(.SD, aes(TIME, value)) + geom_line() + facet_wrap(~ country) +
      ggtitle(paste0(descriptions[[.BY[[1]]]], ' by country'))
    tikz(s, height=6, width=9)
    print(gg2)
    dev.off()
  }, by='variable']

  ## Diagnostic plot
  tikz('plot/model_diagnostic.tex', width=6, height=6)
  par(mfrow=c(2,2))
  plot(x.lm)
  dev.off()
}


## If you want to compare visually the variables
if (MAKE_PLOT){
  ## compareValue(x.new, total_disburrsements='ypgtq')
  ## compareValue(x.new, ydrh='ydrh', gdp_cap='gdp_per_capita')
  ## compareValue(x.new, gdp_cap='gdp_per_capita')
  plot.data <- melt(x.new[, list(TIME, country, popwork=exp(lpop), poptot=exp(lpoptot)), ],
                    id.vars=c('TIME', 'country'))
  gg <- ggplot(plot.data, aes(TIME, value)) + geom_line(aes(color=variable)) +
    facet_wrap(~country)
  print(ggplotly(gg))
}


################################################################################
### Generate lm output for latex

queryList <- function(l, kx){
  kx %>%
    lapply(function(s) if(is.null(d <- l[[s]])) NA else d) %>%
    unlist
}

description <-
  c(list(gdpv_annpct='GDP growth',
         ydrh_to_gdpv='Household net income, in \\% of GDP',
         gdp_per_capita='GDP per capita, in USD Millions',
         fiscal_transparency='IMF GFS Index',
         ypgtq='Government expenditure in \\% of GDP',
         country='Country',
         `gdpv_annpct:fiscal_transparency_score`='Effect of Fiscal Transparency on GDP Growth',
         fiscal_transparency_score='Fiscal Transparency',
         'gini_toth'='Gini coefficient (Toth 2015)'),
    descriptions)

queryList(description, colnames(x.lm$model)) %>% {
  stargazer(x.lm$model, out='model_output/simple_statistic.tex',
            covariate.labels=.,
            font.size='footnotesize')
}

toTexModel <- function(li.lm, title, out, dep.names='Public employment rate'){
  cov.labs <- na.omit(queryList(description, names(coef(li.lm[[1]]))[-1]))
  argx <- c(li.lm, list(title=title, out=out, covariate.labels=cov.labs,
                        dep.var.labels=dep.name, omit=c('TIME', 'country'),
                        omit.labels = c('Year fixed-effect',
                                        'Country fixed-effect')))
  do.call(stargazer, argx)
}

dep.name <- 'Public employment rate'
toTexModel(list(x.lm),
           'Main variable result',
           'model_output/simple_lm.tex')
toTexModel(list(lpoptot.lm, lpop.lm),
           'Robustness of log of working population',
           'model_output/simple_lm_lpop.tex')
toTexModel(list(incomeineq.lm, incomeineq.wo.lm),
           'Effect of income inequality',
           'model_output/simple_lm_incomeineq.tex')
toTexModel(list(gini.lm, gini.wo.lm),
           'Effect of the gini coefficient (Toth 2015)',
           'model_output/simple_lm_gini.tex')
toTexModel(list(gdp.per.capita.lm, gdp.per.capita.wo.lm),
           'Effect of the GDP per capita',
           'model_output/simple_lm_gdp_per_capita.tex')
toTexModel(list(fiscal.lm, fiscal.wo.lm),
           'Effect of IMF fiscal transparency index',
           'model_output/simple_lm_fiscal_transparency.tex')
toTexModel(list(lassen.lm, lassen.wo.lm),
           'Effect of Lassen Fiscal Transparency index',
           'model_output/simple_lm_lassen_transparency.tex')
toTexModel(list(imf.gfs.lm, imf.gfs.wo.lm),
           'Effect of Lassen Fiscal Transparency index',
           'model_output/simple_lm_imf_transparency.tex')

################################################################################
