source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(plotly)

MAKE_PLOTS <- FALSE

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

# x is the data set with annual observation for eg
x <- eos[[1]][country.a]
x[, egr := 100*eg/et] # et: General Government employment, et: Total employment
x[, country:=as.factor(country)]
x[, lpop := log(pop1574/1e6)] # log pop of millions
x[, ydrh_to_gdpv:=100*ydrh/gdpv]
x <- x[!is.na(egr)] # Non na observation

x.lm <- lm(egr ~ ., x[, cols, with=FALSE])
x.lm.s <- summary(x.lm)
x.lm.s
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
  x.lm <- lm(formula, data[, cols.extended, with=FALSE])
  print(summary(x.lm))
  x.lm
}

# All variables
ff <- egr ~ .  #+ imf_gfs*gdpv_annpct

## W/o countries
print(cols)
country.wo.lm <- robustnessAnalysis(x.new, cols, 'country', ff)

## Not so much difference, but loss of 150 data points, so keep lpop.
lpoptot.lm <- robustnessAnalysis(x.new, c(cols, 'lpoptot'), 'lpop', ff)
lpop.lm <- completeLmData(lpoptot.lm, x.new, 'lpop') %>%
  robustnessAnalysis(cols, 'lpoptot', ff)

## Income inquality is not significant and also limit the size of the dataset
## gdp_per_capita and lpop are represented by ydrh and lpoptot, hence we can drop them
incomeineq.lm <- robustnessAnalysis(x.new, c(cols, 'incomeineq'), '', ff)
incomeineq.wo.lm <- as.data.table(incomeineq.lm$model) %>%
  robustnessAnalysis(, cols, '', ff)

gini.lm <- robustnessAnalysis(x.new, c(cols, 'gini_toth'), '', ff)
gini.wo.lm <- robustnessAnalysis(as.data.table(gini.lm$model), cols, '', ff)

## gdp_per_capita seems to be significant # Loss of 500 observations though
gdp.per.capita.lm <- robustnessAnalysis(x.new, c(cols, 'gdp_per_capita'), '', ff)
gdp.per.capita.wo.lm <- as.data.table(gdp.per.capita.lm$model) %>%
  robustnessAnalysis(cols, '', ff)

## Fiscal Transparency
## Note: should try with the constant fiscal score * gdp_growth
fiscal.lm <- robustnessAnalysis(x.new, c(cols, 'fiscal_transparency'), '', ff)
fiscal.wo.lm <- as.data.table(fiscal.lm$model) %>%
  robustnessAnalysis(c(cols), '', ff)



################################################################################
## Plots

## Data plots
if (MAKE_PLOTS){
  data.plot <- melt(data.table(x.lm$model), id.vars=c('country', 'TIME'))
  data.plot[, variable:=gsub('_', '\\\\_', variable)]
  descriptions <- list(`gdpv\\_annpct`='GDP growth',
                       unr='Unemployment rate',
                       ypgtq='Total disbursements, general government, in percent of GDP',
                       egr='Public employment rate',
                       lpop='Log of population',
                       `ydrh\\_to\\_gdpv`='Household net income, in percent of GDP')

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

  tikz('plot/model_diagnostic.tex', width=6, height=6)
  par(mfrow=c(2,2))
  plot(x.lm)
  dev.off()
}

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
