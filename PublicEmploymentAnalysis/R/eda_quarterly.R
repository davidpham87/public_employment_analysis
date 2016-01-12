source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(parallel)

MAKE_PLOTS <- TRUE
MAX_YEAR_EXTRAPOLATION <- 2016

cols <- c('egr', # public employement rate
          'YEAR',
          'QUARTER',
          'gdpv_annpct', # gdp growth
          'unr', # 'unemployment rate'
          'ypgtq_interpolated', # Total disburrsements, general government
          ## 'pop1574', # population proxy (aged from 15 to 74)
          'lpop_interpolated', # log population
          'country',
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
x[, YEAR:= as.numeric(YEAR)]
x[, egr := eg/et] # et: General Government employment, et: Total employment
x[, country:=as.factor(country)]
x[, ydrh_to_gdpv:=100*ydrh/gdpv]
x <- x[!is.na(egr)] # Non na observation


x.lm <- lm(egr ~ ., x[, cols, with=FALSE])
summary(x.lm)

################################################################################
### Residuals are really bad when using no difference
### New methods: diff all variable and study the difference



################################################################################
## Plots

## Data plots
if (MAKE_PLOTS){
  data.plot <- melt(data.table(x.lm$model), id.vars=c('country', 'YEAR', 'QUARTER'))

  quarter.substitute <-
    lapply(1:4, function(i) list(paste0('-Q', i), paste0('.', 100*(i-1)/4)))

  quarters.time <- Reduce(function(x, l) gsub(l[[1]], l[[2]], x),  quarter.substitute,
                          data.plot[, paste0(YEAR, '-', QUARTER)]) %>% as.numeric
  data.plot[, TIME:=quarters.time]


  data.plot[, variable:=gsub('_', '\\\\_', variable)]
  descriptions <- list(`gdpv\\_annpct`='GDP growth',
                       unr='Unemployment rate',
                       ypgtq='Total disbursements, general government, in percent of GDP',
                       egr='Public employment rate',
                       lpop='Log of population',
                       `ydrh\\_to\\_gdpv`='Household net income, in percent of GDP')

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
}
