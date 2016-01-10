PCKGS <- c('data.table',
           'readstata13',
           'magrittr',
           'ggplot2',
           'tikzDevice',
           'stargazer',
           'rsdmx',
           'softImpute',
           'rpart',
           'glmnet'
)

loadPackages <- function(){
  lapply(PCKGS, require, character.only=TRUE)
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

unscale <- function(s) {
  s * attr(s, 'scaled:scale') + attr(s, 'scaled:center')
}
