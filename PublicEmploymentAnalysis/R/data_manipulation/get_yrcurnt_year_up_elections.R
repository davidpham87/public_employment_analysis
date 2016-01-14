### Caution: TODO: rename column to make them consistent with the other data
### sets (country and time)

yrcurnt_corrected <- repmis::source_data('http://bit.ly/1EM8EVE')
write.csv(yrcurnt_corrected, '../../data/year_until_elections_cleaned.csv')
