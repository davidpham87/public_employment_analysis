source('init.R')
pckgs <- loadPackages()

lvl2num <- function(x) as.numeric(levels(x)[x])
showdiag <- function(lm.obj){
  par(mfrow = c(2, 2))
  plot(lm.obj)
}


MAKE_PLOT <- FALSE

cols <- c('egr_diff', # public employement rate
          'egr_lagged',
          'TIME', # year
          'gdpv_annpct', # gdp growth
          'unr', # 'unemployment rate'
          'ypgtq', # Total disburrsements, general government percent of GDP
          ## 'pop1574', # population proxy (aged from 15 to 74) # absolute number
          'lpop', # log population -> take approximation
          #'country'
          'ydrh_to_gdpv' # Ratio of net money income per capita to gdp (volume)
          )

# For dynamic model and plm
## cols.dyn <- c('egr', 'TIME', 'gdpv_annpct', 'unr', 'ypgtq', 'lpop', 'country',
##               'ydrh_to_gdpv')


eos <- readRDS('../data/eo-data.rds')
eo.desc <- readRDS('../data/eo-colnames-dt.rds')
setkey(eo.desc, VARIABLE) # enable eo.desc['bsii'] => Balance of income, value, BOP basis

## change time
setkey(eos[[1]], 'country')
eos[[1]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.a
missing.country <- eos[[1]][, setdiff(unique(country), country.a)]


x <- eos[[1]][country.a]
x[, egr := 100*eg/et, by='country'] # et: General Government employment, et: Total employment
x[, egr_diff:= c(NA, diff(egr)), by='country'] # et: General Government employment, et: Total employment
## x <- x[TIME < 2014] # make sure there are no projections
x <- x[TIME < 2012 & TIME >= 1960] # make sure there are no projections
x[, egr_lagged:= c(NA, egr_diff[1:length(egr_diff)-1]), by='country'] # et: General Government employment, et: Total employment
x[, TIME:=as.factor(TIME)]
x[, country:=as.factor(country)]
x[, lpop := log(pop1574/1e6)] # log pop of millions
x[, ydrh_to_gdpv:=100*ydrh/gdpv]
x <- x[!is.na(egr)] # Non na observation

if (MAKE_PLOT){
  par(mfrow=c(5,5))
  x[, { pacf(egr, main=.BY[[1]]); 0 }, by='country']
}

## Expand the formula
x.lm <- lm(egr_diff~ ., data=x[, cols, with=FALSE])
x.lm.s <- summary(x.lm)
x.lm.s

x.model.lm <- na.omit(x[, c(cols, 'country'), with=FALSE])

x.model.lm$TIME <- x.model.lm[, lvl2num(TIME)]
y.fit.simple.lm <- fitted(x.lm)
x.simple.model <- as.data.table(x.lm$model)

################################################################################
## Additional Data

## execrlc
## 1 is right 2 center 3 left

x[, TIME:=lvl2num(TIME)] # Revert back because of joining data

new.data.names <- new.data <-
  c('gini', 'population', 'gdp_capita', 'imf_gfs_scores', 'gini_toth', 'dpi_excecrlc', 'year_until_elections')

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
x.new[execrlc %in% c(0, NA, 2), execrlc:=NA]
x.new[, execrlc:=factor(execrlc, labels=c('right', 'left'))]
x.new <- merge(x.new, imf.gfs, by='country') # add img_gfs fiscal_scores

new.var.names <- list(incomeineq='gini', pop='population',
                      gdp_per_capita='gdp_per_capita',
                      fiscal_transparency='fiscal_transparency')

################################################################################
### Robustness Analysis

new.cols <- c('imf_gfs', 'incomeineq', 'lpoptot', 'gdp_per_capita', 'execrlc',
              'yrcurnt_corrected')

cols.extended <- c(cols, new.cols)
x.new[, imf_gfs:=scale(imf_gfs, scale=TRUE)]

robustnessAnalysis <- function(data, cols, to.drop, formula=egr ~ .){
  cols.extended <- unselectVector(cols, to.drop)
  x.lm <- lm(formula, data[, cols.extended, with=FALSE])
  print(summary(x.lm))
  x.lm
}

# All variables
ff <- egr_diff~ .  #+ imf_gfs*gdpv_annpct
x.new[, TIME:=as.factor(TIME)]

## W/o countries
print(cols)
country.wo.lm <- robustnessAnalysis(x.new, cols, 'country', ff)

## Not so much difference, but loss of 150 data points, so keep lpop.
lpoptot.lm <- robustnessAnalysis(x.new, c(cols, 'lpoptot'), 'lpop', ff)
lpop.lm <- robustnessAnalysis(x.new[!is.na(lpoptot)], c(unselectVector(cols, 'lpop'), 'lpop'), '', ff)

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

ff.fiscal <-
  egr_diff ~ . - fiscal_transparency_score + fiscal_transparency_score*gdpv_annpct

lassen.lm <- robustnessAnalysis(x.lassen, c(cols, 'fiscal_transparency_score'),
                                '', ff.fiscal)
lassen.wo.lm <- robustnessAnalysis(as.data.table(lassen.lm$model), cols, '', ff)

imf.gfs.lm <-
  robustnessAnalysis(x.imf.gfs, c(cols, 'fiscal_transparency_score'),
                     '',  ff.fiscal)
imf.gfs.wo.lm <- robustnessAnalysis(as.data.table(imf.gfs.lm$model), cols, '', ff)

## Left or right goverment
govrlc.lm <- robustnessAnalysis(x.new, c(cols, 'execrlc'), '', ff)
govrlc.wo.lm <- robustnessAnalysis(as.data.table(govrlc.lm$model), cols, '', ff)

## Years until election
yrcurnt.lm <- robustnessAnalysis(x.new, c(cols, 'yrcurnt_corrected'), '', ff)
yrcurnt.wo.lm <- robustnessAnalysis(as.data.table(yrcurnt.lm$model), cols, '', ff)


################################################################################
## Plots

descriptions <- list(`gdpv\\_annpct`='GDP growth',
                     unr='Unemployment rate',
                     ypgtq='Total disbursements, general government, in percent of GDP',
                     egr='Public employment rate',
                     lpop='Log of adult population in million',
                     `ydrh\\_to\\_gdpv`='Household net income, in percent of GDP',
                     `gdp\\_per\\_capita`='GDP per capita in USD Millions',
                     `fiscal\\_transparency`='IMF GFS Index',
                     incomeineq='Gini coefficient',
                     lpoptot='Log of total population in million',
                     'TIME'='Time',
                     egr_diff='Change in Public Employment Rate (CPER)',
                     egr_lagged='Lagged change in Public Employment Rate',
                     execrlc='Left or right government',
                     yrcurnt_corrected='Years until election')

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

  ## Plots the pacf for the diff of egr
  tikz('plot/model_pacf.tex', width=6, height=6)
  par(mfrow=c(4, 4))
  x[, {pacf(egr, main=.BY[[1]]);0}, by='country']
  dev.off()

  colnames(x.model.lm) <- gsub('\\_', '\\\\_', colnames(x.model.lm))
  y.fit <- y.fit.simple.lm
  tikz('plot/model_fit_quality.tex', width=6, height=6)
  gg <- compareValue(as.data.table(cbind(x.model.lm, y.fit)), y.fit='y.fit', egr='egr\\_diff')
  print(gg)
  dev.off()
}


## If you want to compare visually the variables
if (FALSE){
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
         'gini_toth'='Gini coefficient (Toth 2015)',
         egr_diff='Change in Public Employment Rate (CPER)',
         egr_lagged='Lagged change in Public Employment Rate',
         yrcurnt_corrected='Years left until election',
         execrlcleft='Left government effect'),
    descriptions)

x.lm$model$TIME <- lvl2num(x.lm$model$TIME)
queryList(description, colnames(x.lm$model)) %>% {
  stargazer(x.lm$model, out='model_output/simple_statistic.tex',
            covariate.labels=.,
            font.size='footnotesize', title='Data statistics')
}

description[['egr_lagged']] <- NA
toTexModel <- function(li.lm, title, out, dep.name='Difference in public employment rate'){
  cov.labs <- na.omit(queryList(description, names(coef(li.lm[[1]]))[-1]))
  argx <- c(li.lm, list(title=title, out=out, covariate.labels=cov.labs,
                        dep.var.labels=dep.name, omit=c('TIME', 'egr_lagged'),
                        omit.labels = c('Year fixed-effect',
                                        'Auto-correlation effect')))
  do.call(stargazer, argx)
}

dep.name <- 'Difference in public employment rate'
toTexModel(list(x.lm),
           'Main variable result',
           'model_output/simple_lm.tex')
toTexModel(list(lpoptot.lm, lpop.lm),
           'Robustness of log of adult population',
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
toTexModel(list(govrlc.lm, govrlc.wo.lm),
           'Effect of Government Political Side',
           'model_output/simple_lm_govrlc.tex')
toTexModel(list(yrcurnt.lm, yrcurnt.wo.lm),
           'Effect of years left until election',
           'model_output/simple_lm_yrcurnt.tex')
################################################################################
