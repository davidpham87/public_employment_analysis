source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(plotly)

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
setkey(eo.desc, VARIABLE) # enamble eo.desc['bsii'] => Balance of income, value, BOP basis

## change time

setkey(eos[[1]], 'country')

eos[[1]]['CHE'][, list(TIME, ypgtq)]
eos[[1]]['JPN'][, list(TIME, ypgtq)]
eos[[1]]['USA'][, list(TIME, 100*eg/et, eg, et)]
eos[[1]]['NOR', list(TIME, 100*eg/et, eg, et)]

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


# Data plots
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

par(mar=c(4, 10, 4, 4))
x.lm.lower <- lm(egr ~ 1, x[, cols, with=FALSE])
x.step.low <- step(x.lm.lower, scope=list(lower=x.lm.lower, upper=x.lm), direction="both")
x.step.up <- step(x.lm, scope=list(lower=x.lm.lower, upper=x.lm), direction="both")

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

imf.gfs <- fread('../data/imf_gfs_scores.csv', header=TRUE)[, list(ISO, `Index Score`)]
setnames(imf.gfs, c('ISO', 'Index Score'), c('country', 'imf_gfs'))

x.new <- new.data[J(x)]
x.new[, lpoptot:=log(pop)]
x.new[, pop:=NULL]
x.new <- merge(x.new, imf.gfs, by='country') # add img_gfs fiscal_scores

new.var.names <- list(incomeineq='gini', pop='population',
                      gdp_per_capita='gdp_per_capita',
                      fiscal_transparency='fiscal_transparency')

## Robustness Analysis
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

## Not so much difference, but loss of 150 data points, so keep lpop.
lpoptot.lm <- robustnessAnalysis(x.new, c(cols, 'lpoptot'), 'lpop', ff)
lpop.lm <- completeLmData(lpoptot.lm, x.new, 'lpop') %>%
  robustnessAnalysis(cols, 'lpoptot', ff)

## Income inquality is not significant and also limit the size of the dataset
## gdp_per_capita and lpop are represented by ydrh and lpoptot, hence we can drop them
incomeineq.lm <- robustnessAnalysis(x.new, c(cols, 'incomeineq'), '', ff)
incomeineq.wo.lm <- robustnessAnalysis(as.data.table(incomeineq.lm$model), cols, '', ff)


## gdp_per_capita seems to be significant # Loss of 500 observations though
gdp.per.capita.lm <- robustnessAnalysis(x.new, c(cols, 'gdp_per_capita'), '', ff)
gdp.per.capita.wo.lm <- as.data.table(gdp.per.capita.lm$model) %>%
  robustnessAnalysis(cols, '', ff)

fiscal.lm <- robustnessAnalysis(x.new, c(cols, 'fiscal_transparency'), '', ff)
fiscal.wo.lm <- as.data.table(fiscal.lm$model) %>%
  robustnessAnalysis(c(cols), '', ff)



y <- x.new[, cols.extended, with=FALSE]



compareValue(x.new, ggexp='ggexp', total_disburrsements='ypgtq')
compareValue(x.new, ydrh='ydrh', gdp_cap='gdp_per_capita')
compareValue(x.new, gdp_cap='gdp_per_capita')

plot.data <- melt(x.new[, list(TIME, country, popwork=exp(lpop), poptot=exp(lpoptot)), ],
                    id.vars=c('TIME', 'country'))
gg <- ggplot(plot.data, aes(TIME, value)) + geom_line(aes(color=variable)) +
  facet_wrap(~country)
print(ggplotly(gg))

par(mar=c(4, 10, 4, 4))
x.step <- step(x.lm, egr ~ .)

## Many missing data

y <- x[, cols, with=FALSE][, lapply(.SD, as.numeric)]
y <- x[, lapply(.SD, as.numeric)]
imputations <- impute::impute.knn(as.matrix(y), colmax = 0.99999)
n <- 5
data.mice <- lapply(1:n, function(i) mice::complete(mice::mice(x, n), i))
## Drop data here

## Look for column with more than 80% data
p <- 0.5
missing.rate <- vapply(x, function(y, p) mean(is.na(y)), 0.0) %>%
  sort(TRUE) %>% round(4) %>% {Filter(function(x) x >= p, .)}

sort(names(Filter(function(x) x > 0.9, missing.rate)))
par(mar=c(2.5, 10, 2.5, 2.5))
barplot(missing.rate, horiz=T, las=2)

## library(mi)
## mdf <- missing_data.frame(y)
## options(mc.cores=8)
## mi.imputations <- mi(mdf, n.iter = 50, n.chains = 4, max.minutes = 20)
## pool(egr ~. , mi.imputations)

# GDP is local currency
ggplot(x[c('USA', 'JPN')], aes(TIME, gdp)) + geom_line() + facet_wrap( ~ country)

# gdp per capita -> USD
# total_spending_per_capita? USD?
#
