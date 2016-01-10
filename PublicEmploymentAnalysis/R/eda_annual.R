source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)

cols <- c('egr', # public employement rate
          'TIME', # year
          'gdpv_annpct', # gdp growth
          'unr', # 'unemployment rate'
          'ypgtq', # Total disburrsements, general government
          ## 'pop1574', # population proxy (aged from 15 to 74)
          'lpop', # log population -> take approximation
          'country',
          'ydrh' # net money income per capita
          )

eos <- readRDS('../data/eo-data.rds')
eo.desc <- readRDS('../data/eo-colnames-dt.rds')
setkey(eo.desc, VARIABLE) # enamble eo.desc['bsii'] => Balance of income, value, BOP basis

## change time

setkey(eos[[1]], 'country')
setkey(eos[[2]], 'country')

eos[[1]]['CHE'][, list(TIME, ypgtq)]
eos[[1]]['JPN'][, list(TIME, ypgtq)]
eos[[1]]['USA'][, list(TIME, 100*eg/et, eg, et)]
eos[[1]]['NOR', list(TIME, 100*eg/et, eg, et)]

eos[[1]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.a
eos[[2]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.q

missing.country <- eos[[1]][, setdiff(unique(country), country.a)]

# x is the data set with annual observation for eg
x <- eos[[1]][country.a]
x[, egr := eg/et] # et: General Government employment, et: Total employment
x[, country:=as.factor(country)]
x[, lpop := log(pop1574)]
x <- x[!is.na(egr)] # Non na observation

summary(x)
par(mar=c(2.5, 10, 2.5, 2.5))
barplot(sort(table(x$country)), horiz=T, las=2)

options(tikzDefaultEngine = 'pdftex')
gg <- ggplot(x, aes(TIME, 100*egr)) +
  geom_line() + facet_wrap(~country) + theme_bw() +
  ylab('Public Employment Ratio to Total Employment in percent')
tikz('test.tex', height=6.5, width=9)
print(gg)
dev.off()

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

## Quarter
## Scale?
## log population?
## GDP per capita

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
##  [1] "ecsa"      "gdpml"     "gdpmlv"    "gdpofs"    "gdpofsv"   "gdpvcsa"   "gnp"       "gnpv"      "icsa"      "iobv"
## [11] "ioilv"     "ishv"      "nlgc"      "nlgcq"     "nlgml"     "nlgmlq"    "oilcon"    "oilmnt"    "oilsto"    "oilsup"
## [21] "oilxnt"    "pgdpml"    "pgdpofs"   "pgnp"      "pmgsd"     "psbr"      "psbrq"     "pxgsd"     "r_gdpvwds" "savgml"
## [31] "tdd"       "tindml"    "tindofs"   "tocrml"    "tocrofs"   "ttrade"    "tya"       "tybml"     "tybofs"    "tyml"
## [41] "unrs"      "wpbrent"   "wphamd"    "wphd"      "wphfbd"    "wphfd"     "wphmmd"    "wphtbd"    "wpi"       "wpoil"
## [51] "ypergml"   "ypergofs"  "ypgct"     "ypgtx"     "yrgct"     "yrgml"     "yrgofs"    "yrgtml"

par(mar=c(2.5, 10, 2.5, 2.5))
barplot(missing.rate, horiz=T, las=2)
