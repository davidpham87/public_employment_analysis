library('readstata13')
library('data.table')
library('magrittr')

# Read and load data "../data/eo-*.dta"
dtas <-  list.files('../data/') %>% { .[grep('\\.dta$', .)] }
eos <- lapply(dtas, function(x) read.dta13(file.path('..', 'data', x)))
names(eos) <- gsub('\\.dta$', '', dtas)

# Summary

lapply(eos, colnames) # LOTS of DATA!!!! 300 columns

idx <- colnames(eos[[1]]) == colnames(eos[[2]]) # there is one difference in
                                                # the data set

colnames(eos[[1]])[which(!idx)] # year -> ann -> yearly data
colnames(eos[[2]])[which(!idx)] # date -> quarterly -> quarterly data
idx.time <- which(!idx)

lapply(eos, function(x) x[sample(nrow(x), size=5), ]) # lots of columns with
                                                      # only nas?

only.nas <- . %>% is.na %>% all
only.nas.columns <- lapply(eos, function(eo) {
  eo %>% lapply(only.nas) %>%  as.data.table %>%
    melt %>% {.[value==TRUE]}
})

lapply(only.nas.columns, nrow) # 15 for yearly, 99 for quarterly

# EDA

eos <- lapply(eos, as.data.table)
setkey(eos[[1]], 'country')
setkey(eos[[2]], 'country')

eos[[1]]['Switzerland'][, list(year, ypgtq)]
eos[[1]]['Japan'][, list(year, ypgtq)]
eos[[1]]['United States'][, list(year, 100*eg/et, eg, et)]
eos[[1]]['Norway', list(year, 100*eg/et, eg, et)]

eos[[1]][ , list(country, eg)] %>% na.omit  %>% {unique(.$country)} -> country.a
eos[[2]][ , list(country, eg)] %>% na.omit  %>% {unique(.$country)} -> country.q

missing.country <- eos[[1]][, setdiff(unique(country), country.a)]
