source('init.R')
pckgs <- loadPackages()

dtas <-  list.files('../data/') %>% { .[grep('\\.dta$', .)] }
eos <- lapply(dtas, function(x) read.dta13(file.path('..', 'data', x)))
names(eos) <- gsub('\\.dta$', '', dtas)


eos <- lapply(eos, as.data.table)
setkey(eos[[1]], 'country')
setkey(eos[[2]], 'country')

eos[[1]]['Switzerland'][, list(year, ypgtq)]
eos[[1]]['Japan'][, list(year, ypgtq)]
eos[[1]]['United States'][, list(year, 100*eg/et, eg, et)]
eos[[1]]['Norway', list(year, 100*eg/et, eg, et)]

eos[[1]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.a
eos[[2]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.q

missing.country <- eos[[1]][, setdiff(unique(country), country.a)]


# x is the data set with annual observation for eg
x <- eos[[1]][country.a]
x[, egr := eg/et]
x[, country:=as.factor(country)]
x[, lpop := log(pop1574)]
x <- x[!is.na(egr)] # Non na observation

summary(x)
par(mar=c(2.5, 10, 2.5, 2.5))
barplot(sort(table(x$country)), horiz=T, las=2)

options(tikzDefaultEngine = 'pdftex')
gg <- ggplot(x, aes(year, 100*egr)) +
  geom_line() + facet_wrap(~country) + theme_bw() +
  ylab('Public Employment Ratio to Total Employment in percent')
tikz('test.tex', height=6.5, width=9)
print(gg)
dev.off()


cols <- c('egr', # public employement
          'year',
          'gdpv_annpct', # gdp growth
          'unr', # 'unemployment rate'
          'ypgtq', # Total disburrsements, general government
          ## 'pop1574', # population proxy (from 15 to 74)
          'lpop', # log population
          'country',
          'ydrh' # net money income per capita
          )

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


## Scale?
## log population?
## GDP per capita

## Many missing data
library(mice)
library(impute)
library(magrittr)
y <- x[, cols, with=FALSE][, lapply(.SD, as.numeric)]
y <- x[, lapply(.SD, as.numeric)]
imputations <- impute::impute.knn(as.matrix(y), colmax = 0.99999)
data.mice <- lapply(1:n, function(i) mice::complete(imputations, i)) # mice and mi conflicts here

## Look for column with more than 80% data

vapply(x, function(y, p) mean(is.na(y)) >= p, p=p, TRUE) %>% {Filter(identity, .)}

p <- 0.95
vapply(x, function(y, p) mean(is.na(y)), 0.0) %>% sort(TRUE) %>% round(4) %>%
  {Filter(function(x) x >= p, .)}
