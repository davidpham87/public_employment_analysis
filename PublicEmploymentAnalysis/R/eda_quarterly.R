source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(parallel)

cols <- c('egr', # public employement rate
          ## 'TIME', # year
          'YEAR',
          'QUARTER',
          'gdpv_annpct', # gdp growth
          'unr', # 'unemployment rate'
          'ypgtq', # Total disburrsements, general government
          ## 'pop1574', # population proxy (aged from 15 to 74)
          # 'lpop', # log population
          'country',
          'ydrh' # net money income per capita
          )

eos <- readRDS('../data/eo-data.rds')
eo.desc <- readRDS('../data/eo-colnames-dt.rds')
setkey(eo.desc, VARIABLE) # enamble eo.desc['bsii'] => Balance of income, value, BOP basis

eos[[1]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.a
eos[[2]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.q
missing.country <- eos[[1]][, setdiff(unique(country), country.a)]

# x is the data set with annual observation for eg
x <- eos[[2]]
setkey(x, 'country')
x <- x[country.q]
x[, YEAR:= as.numeric(YEAR)]
x[, egr := eg/et] # et: General Government employment, et: Total employment
x[, country:=as.factor(country)]
# x[, lpop := log(pop1574)]
x <- x[!is.na(egr)] # Non na observation

summary(x)
par(mar=c(2.5, 10, 2.5, 2.5))
barplot(sort(table(x$country)), horiz=T, las=2)

x.lm <- lm(egr ~ ., x[, cols, with=FALSE])
summary(x.lm)

par(mar=c(4, 10, 4, 4))
x.lm.lower <- lm(egr ~ 1, x[, cols, with=FALSE])
x.step.low <- step(x.lm.lower, scope=list(lower=x.lm.lower, upper=x.lm), direction="both")
x.step.up <- step(x.lm, scope=list(lower=x.lm.lower, upper=x.lm), direction="both")

new.data <- c('gini', 'government_spending', 'population', 'gdp_capita')
new.data %<>% {paste0('../data/', ., '_cleaned.csv')} %>% lapply(fread)

par(mar=c(4, 10, 4, 4))
x.step <- step(x.lm, egr ~.)

## Look for column with more than 70% data
p <- 0.0
missing.rate <- MissingRatePerColumn(x)
par(mar=c(2.5, 10, 2.5, 2.5))
barplot(missing.rate, horiz=T, las=2)
sort(names(Filter(function(x) x > 0.75, missing.rate)))
cols <- names(Filter(function(x) x < 0.5, missing.rate))

y <- x[, cols, with=F]
missing.rate <- MissingRatePerColumn(y)
par(mar=c(2.5, 10, 2.5, 2.5))
barplot(missing.rate, horiz=T, las=2)

y[, QUARTER:=as.factor(QUARTER)]
y[, country:=as.factor(country)]

## log population?
## GDP per capita
VIF <- function(data){
  vif.unique <- . %>% {reformulate('.', ., FALSE)} %>% lm(data) %>% summary %$%
  adj.r.squared %>% {1/(1-.)}
  cn <- colnames(data)
  f <- function(s) tryCatch(vif.unique(s), error=function(error) NaN)
  res <- mclapply(cn, f, mc.cores=8)
  names(res) <- cn
  res
}

## y.impute <- imputeDataSoftImpute(as.data.frame(y), ncol(y)-1)[[1]] %>% as.data.table
vifs <- VIF(unselect(y, 'egr'))
vifs.dt <- data.table(vifs=unlist(vifs), names=names(vifs),
                      desc=eo.desc[names(vifs), Variable])

n <- 5
imputations <- mice::mice(y, n)
data.mice <- lapply(1:n, function(i) mice::complete(imputations, i))

y.lm <- lm(egr ~ ., y.impute)
summary(y.lm)

par(mar=c(4, 10, 4, 4))
y.lm.lower <- lm(egr ~ 1, y.impute)
# y.step.low <- step(y.lm.lower, scope=list(lower=y.lm.lower, upper=y.lm), direction="both")
# y.step.up <- step(y.lm, scope=list(lower=y.lm.lower, upper=y.lm), direction="both")
