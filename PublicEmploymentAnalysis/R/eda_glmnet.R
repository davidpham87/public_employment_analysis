source('init.R')
pckgs <- loadPackages()
library(mice)
library(impute)
library(magrittr)
library(glmnet)

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
# x is the data set with annual observation for eg

setkey(eos[[2]], 'country')
x <- eos[[2]][country.q]
x[, egr := eg/et] # et: General Government employment, et: Total employment
x[, country:=as.factor(country)]
## x[, lpop := log(pop1574)]
x[, ydrh_to_gdpv:=100*ydrh/gdpv]
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

##          VARIABLE                                                                      Variable
##  1:           pcg                            Government final consumption expenditure, deflator
##  2:        pitisk                                             Gross capital formation, deflator
##  3: pcoreh_ytypct                                                     Harmonised core inflation
##  4:        pcoreh                                              Core inflation index, harmonised
##  5:          taxq               Taxes and social security contributions, as a percentage of GDP
##  6:          ptdd                                          Total domestic expenditure, deflator
##  7:          cpaa           Private final consumption expenditure, value, appropriation account
##  8:        sratio     Household and non-profit institutions serving households net saving ratio
##  9:           unr                                                             Unemployment rate
## 10:           pxc                              Competitors' price of goods and services exports
## 11:            un                                                           Unemployment, level
## 12:          nlgq                                Government net lending, as a percentage of GDP
## 13:   gdpv_annpct                                                                   GDP growth
## 14:          xmkt                                  Export market for goods and services, volume
## 15:        ypgtqu Underlying government current disbursements, as a percentage of potential GDP
## 16:          pmsh                                    Shadow price of goods and services imports
## 17:          ntrd         Net current international transfers, value, balance of payments basis
## 18:         cpidr    Competitiveness indicator, relative consumer prices (CPI), overall weights
## 19:         pxgsx                         Price of non- commodity exports of goods and services
## 20:         rpmgs                                 Relative price of imported goods and services
## 21:         ypgtu                     Underlying total disbursements, general government, value
## 22:       pgdpofs                     Gross domestic product, deflator, market prices, offshore
## 23:   (Intercept)                                                                            NA
## 24:           ppp                            Purchasing power parity, National currency per USD
## 25:         pigaa   Government fixed capital formation, deflator,based on approproation account
## 26:         pxgsd                       Exports of goods and services,  National Accounts basis
## 27:          pfdd                                          Final domestic expenditure, deflator
## 28:      cq_fbgsv                               Net exports, contribution to growth in real GDP
## 29:    pcp_ytypct                      Private final consumption expenditure, deflator,  growth
##          VARIABLE                                                                      Variable)
