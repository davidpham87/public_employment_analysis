## Some code when trying to use dynimac formula with R

## library(dynlm)
## # For dynamic model and plm
## DT <- copy(x[, cols.dyn, with=FALSE])
## data.plm <- as.data.frame(DT)

## form <- formula(terms(egr ~ ., data = data.plm))
## ## form.dyn <- dynformula(form, lag.form=list(egr=1))
## ## plm(form.dyn, data.plm)
## dform <- update(form, d(egr, 1) ~ . + d(L(egr, 1), 1))
## data.dynlm <- do.call(cbind, lapply(data.plm, zoo))
## summary(x.dynlm <- dynlm(dform, na.omit(data.dynlm)))

## x.gam <- gam(ff, data=x[, cols, with=FALSE])
## plot(x.gam)
## gam.check(x.gam)

## dev.new()
## par(mfrow=c(5,5))
## x[, {pacf(egr, main=.BY[[1]]);0}, by='country']

## library(sfsmisc)
## library(nlme)
## cols.dyn <- c('egr_diff', 'TIME', 'gdpv_annpct', 'unr', 'ypgtq', 'lpop',
##               'ydrh_to_gdpv')

## form <- formula(terms(egr_diff~ ., data = x[, cols.dyn, with=FALSE]))
## x.gls <- gls(form, data=na.omit(x[, c(cols.dyn), with=FALSE]),
##              correlation=corAR1(0.5, fixed=FALSE))
## anova(x.gls)
## eo.desc[cols]
