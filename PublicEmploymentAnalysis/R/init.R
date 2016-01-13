PCKGS <- c('data.table',
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
           'DescTools',
           'plm',
           'plotly')

loadPackages <- function(){
  lapply(PCKGS, require, character.only=TRUE)
}

################################################################################
### Utiltis

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

################################################################################
### Robustness Analysis

##' Useful this when doing robustness analysis (including excluding variable)
completeLmData <- function(lm.model, DT, new.cols){
  new.DT <- as.data.table(lm.model$model)
  setkey(new.DT, TIME, country)
  new.DT <- merge(new.DT, x.new[, c('country', 'TIME', new.cols), with=FALSE],
                  by=c('TIME', 'country'), all.x=TRUE)
}


################################################################################
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
    stopifnot(all(vapply(column.type.mi, is.element, TRUE, set=valid.column.type)))
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

################################################################################
### Quarterly Functions

interpolateQuarter <- function(t, y, max.time=2016){
  data <- na.omit(data.table(t, y))
  tout <- t[t >= min(data$t) & t <= max.time]
  tryCatch({
    yout <- spline(data$t, data$y, xout=tout)$y
    c(rep(NA, times=length(t[t<min(data$t)])),
      yout,
      rep(NA, times=length(t[t>max.time])))
  }, error = function(e) y)
}


interpolateQuarterColumn <- function(eo.q, eo.a, col, max.time){
  setkey(eo.a, country, TIME)
  setkey(eo.q, country, TIME)

  col.new <- paste0(col, '_annual_data')
  col.q <- paste0(col, '_interpolated')
  setnames(eo.a, col, col.new)
  eo.q <- merge(eo.q, eo.a[, c('country', 'TIME', col.new), with=FALSE], all.x=TRUE)
  eo.q[, (col.q):=interpolateQuarter(TIME, get(col.new), get('max.time')), by='country']
  eo.q[, (col.new):=NULL]
  eo.q
}


################################################################################
### Plots

##' Shortcut to compare to variable in data.table x
compareValue <- function(x, ...){
  require(plotly)
  argx <- unlist(list(...))
  plot.data <- melt(x[, c('TIME', 'country', argx), with=FALSE],
                    id.vars=c('TIME', 'country'))
  gg <- ggplot(plot.data, aes(TIME, value)) + geom_line(aes(color=variable)) +
    facet_wrap(~country)
  print(ggplotly(gg))
  gg
}
