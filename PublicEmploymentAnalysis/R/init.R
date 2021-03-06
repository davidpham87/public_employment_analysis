PCKGS <- c('DescTools',
           'data.table',
           'readstata13',
           'magrittr',
           'ggplot2',
           'tikzDevice',
           'stargazer',
           'rsdmx',
           'softImpute',
           'rpart',
           'glmnet',
           'parallel',
           'plm',
           'plotly',
           'tempdisagg')

loadPackages <- function(){
  lapply(PCKGS, require, character.only=TRUE)
}

###########################################################################
### Utilities

joinDataTable <- function(lDT, kx=c('LOCATION', 'TIME')){
  Reduce(function(x, y) merge(x, y, all=TRUE), lDT)
}


unscale <- function(s) {
  s * attr(s, 'scaled:scale') + attr(s, 'scaled:center')
}

VIF <- function(data){
  vif.unique <- . %>% {reformulate('.', .)} %>% lm(data) %>% summary %$%
  adj.r.squared %>% {1/(1-.)}
  cn <- colnames(data)
  f <- function(s) tryCatch(vif.unique(s), error=function(error) NaN)
  res <- mclapply(cn, f, mc.cores=8)
  names(res) <- cn
  res
}

missingRatePerColumn <- function(x, p=0){
  missing.rate <- vapply(x, function(y) mean(is.na(y)), 0.0) %>%
    sort(TRUE) %>% round(4) %>% {Filter(function(x) x >= p, .)}
  missing.rate
}

unselect <- function(data, cols){
  new.cols <- Filter(function(x) !x %in% cols, colnames(data))
  subset(data, select=new.cols)
}

unselectVector <- function(x, kx) x[!x %in% kx]

butlast <- function(x, k=1) x[1:(length(x)-k)]

fctr2num <- function(x) as.numeric(levels(x)[x])

robustnessAnalysis <- function(data, cols, to.drop, formula=egr ~ .){
  cols.extended <- unselectVector(cols, to.drop)
  x.lm <- lm(formula, data[, cols.extended, with=FALSE])
  print(summary(x.lm))
  x.lm
}


###########################################################################
### Robustness Analysis

##' Useful this when doing robustness analysis (including excluding variable)
completeLmData <- function(lm.model, DT, new.cols){
  new.DT <- as.data.table(lm.model$model)
  setkey(new.DT, TIME, country)
  new.DT <- merge(new.DT, x.new[, c('country', 'TIME', new.cols), with=FALSE],
                  by=c('TIME', 'country'), all.x=TRUE)
}


###########################################################################
### Imputation functions

scaleNumeric <- function(x){
  if (mode(x) == 'numeric'){
    scale(x)
  } else {
    x
  }
}


str2fctrs <- function(dataset){
  fctrs <- sapply(1:ncol(dataset), function(jdx)
    any(c("string", "character") %in% class(dataset[1, jdx])))
  dataset[, fctrs] <- lapply(dataset[, fctrs, drop=FALSE], as.factor)
  dataset
}

imputeDataMi <- function(dataset, n, column.type.mi=NULL, ...){
  args <- list(...)
  valid.column.type <- c("unordered-categorical", "ordered-categorical",
                         "binary", "interval", "continuous", "count",
                         "irrelevant")

  ## check that the modification are valid
  if (!is.null(column.type.mi)){
    stopifnot(all(vapply(column.type.mi, is.element, TRUE,
                         set=valid.column.type)))
  } else {
    column.type.mi <- list()
  }

  mdf <- missing_data.frame(dataset) # missing data.frame

  for (k in names(column.type.mi)){
    mdf <- change(mdf, y=k, what="type", to=column.type.mi[[k]])
  }

  imputations <- do.call(mi, c(list(mdf, n.iter=30, n.chains=4), args))
  data.mi <-  mi::complete(imputations, n) # creates 20 different versions of imputations

  ## mi append columns providing the stating the missingnes, so we have to delete them
  data.mi <- lapply(data.mi,  function(df) df[, 1:ncol(dataset)]) # restrict the number of columns
  return(data.mi)
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

###########################################################################
### Quarterly Functions

##' Expects annual regular data with t being the year
interpolateQuarter <- function(t, y, max.time=2016,
                               method.interpolation=NULL){
  data <- na.omit(data.table(t, y))
  tout <- t
  t.idx <- data[, tout >= min(t) & tout <= max.time]
  data <- data[t <= max.time]
  tout <- tout[t.idx]
  yout.index <- data$t
  method.interpolation <- if (is.null(method.interpolation)) 'spline' else method.interpolation
  tryCatch({
    if (method.interpolation == 'spline'){
      yout <- spline(data$t, data$y, xout=tout)$y
    }

    if (method.interpolation == 'denton-cholette'){
      y.ts <- ts(data$y, start=min(data$t))
      y.td <- td(y.ts ~ 1, conversion='sum', to='quarterly',
                 method=method.interpolation)
      y.predict <- predict(y.td)
      yout <- as.numeric(y.predict) # max time not considered here. FIXME
      yout.index <- index(y.predict)
    }

    if (method.interpolation %in% c('locf', 'fill-forward')){
      yout <- zoo::na.locf(y)
    }

    na.size.before <- length(t[t<min(yout.index)])
    na.size.after <- length(t) - na.size.before - length(yout)
    res <- c(rep(NA, times=na.size.before),
             yout,
             rep(NA, times=na.size.after))

    return(res)
  }, error = function(e) y)
}


interpolateQuarterColumn <- function(eo.q, eo.a, col, max.time,
                                     method.interpolation=NULL){
  setkey(eo.a, country, TIME)
  setkey(eo.q, country, TIME)

  col.new <- paste0(col, '_annual_data')
  col.q <- paste0(col, '_interpolated')
  tryCatch(setnames(eo.a, col, col.new), error=function(e) NA)
  eo.q <- merge(eo.q,
                eo.a[, c('country', 'TIME', col.new), with=FALSE],
                all.x=TRUE)
  eo.q[, (col.q):=interpolateQuarter(TIME, get(col.new), get('max.time'),
                                     get('method.interpolation')),
       by='country']
  eo.q[, (col.new):=NULL]
  eo.q
}


###########################################################################
### Plots

##' Shortcut to compare to variable in data.table x
compareValue <- function(x, ...){
  require(plotly)
  argx <- unlist(list(...))
  plot.data <- melt(x[, c('TIME', 'country', argx), with=FALSE],
                    id.vars=c('TIME', 'country'))
  gg <- ggplot(plot.data, aes(TIME, value)) +
    geom_line(aes(color=variable)) +
    facet_wrap(~country)
  print(ggplotly(gg))
  gg
}

showdiag <- function(lm.obj){
  par(mfrow = c(2, 2))
  plot(lm.obj)
}
