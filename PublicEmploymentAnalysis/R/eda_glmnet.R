source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(glmnet)

unselect <- function(data, cols){
    new.cols <- Filter(function(x) !x %in% cols, colnames(data))
    subset(data, select=new.cols)
}

unscale <- function(s) {
    s * attr(s, 'scaled:scale') + attr(s, 'scaled:center')
}

imputeDataSoftImpute <- function(dataset, ...){

    args <- list(...)
    is.null.args <- length(args) == 1 & is.null(args[[1]])

    ## boolean vectors stating factors columns
    fctrs <- sapply(1:ncol(dataset), function(jdx)
        any(c("factor", "string") %in% class(dataset[1, jdx])))
    lvls <- lapply(dataset[, fctrs], levels)

    dataset[, fctrs] <- lapply(dataset[, fctrs], as.numeric)
    x <- as.matrix(dataset)

    fit <- if (is.null.args){
               do.call(softImpute::softImpute, c(list(x)))
           } else {
               do.call(softImpute::softImpute, c(list(x), args))
           }

    dataset <- as.data.frame(softImpute::complete(x, fit))

    ## Correct the factors
    f <- function(s) {
        cut(round(dataset[, s]), c(0, seq_along(lvls[[s]])),
            labels=lvls[[s]], include.lowest=TRUE)
    }

    dataset[, fctrs] <- lapply(names(lvls), f)
    list(dataset)
}

cols <- c('egr', # public employement rate
          'TIME', # year
          'gdpv_annpct', # gdp growth
          'unr', # 'unemployment rate'
          'ypgtq', # Total disburrsements, general government
          ## 'pop1574', # population proxy (aged from 15 to 74)
          ## 'lpop', # log population
          'country',
          'ydrh' # net money income per capita
          )

eos <- readRDS('../data/eo-rds-ethz.rds')
eos[[2]][ , list(country, eg)] %>% na.omit %>% {unique(.$country)} -> country.q

## x is the data set with annual observation for eg
setkey(eos[[2]], 'country')
x <- eos[[2]][country.q]
x[, egr := eg/et] # et: General Government employment, et: Total employment
x[, country:=as.factor(country)]
# x[, lpop := log(pop1574)]
x <- x[!is.na(egr)] # Non na observation

x.lm <- lm(egr ~ ., x[, cols, with=FALSE])
summary(x.lm)

new.data <- c('gini', 'government_spending', 'population', 'gdp_capita')
new.data %<>% {paste0('../data/', ., '_cleaned.csv')} %>% lapply(fread)

par(mar=c(4, 10, 4, 4))
x.step <- step(x.lm, egr ~.)

y <- x
y[, QUARTER:=as.factor(QUARTER)]
y[, country:=as.factor(country)]
y[, YEAR:= as.numeric(YEAR)]
# y <- y[, lapply(.SD, as.numeric)]

y.impute <- imputeDataSoftImpute(as.data.frame(y), ncol(y)-1)[[1]] %>% as.data.table
y.impute[, QUARTER:=as.factor(QUARTER)]
y.impute[, country:=as.factor(country)]

# Use lasso methods to find the variable.
response <- y.impute$egr
ff <- egr ~ .
m <- model.frame(ff, y.impute)
mat <- model.matrix(ff, m)
y.lasso <- glmnet(mat, response)
y.lasso.cv <- cv.glmnet(mat, reponse)
cf <- coef(y.lasso.cv)
cf.eda <- data.table(cf@x, cf@i)[V1 > 10e-4][, list(V1, cf@Dimnames[[1]][V2])][order(V1)]
saveRDS(cf.eda, '../data/lasso-coeff.rds')







