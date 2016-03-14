# Meeting on 2016-02-02

Cointegration.
$\Delta y_t = \gamma(y_{t-1} - \delta x_{t-1}) + \beta \Delta \x_{t-1}$


+ [ ] GDPVD PPP over population for population for gdp per capita. (log of gdp per
capita). Formula: $log(GDPVD/POP)$, where $pop$ is interpolated from annual
data (data comes from th OECD).
+ [ ] Government spending is an alias for government revenue (defined as percent of
GDP) . Formula: $100*YRG/GDPVD$, where $YRG$ is interpolated from annual data.
+ [ ] Net lending NLG as % of GDP (interpolated from annual data). Formula:
$100*nlg/gdpvd$, with nlg interpolated from annual data.
+ [ ] Time Trend.
+ [ ] GAP (gdp output gap). Formula: $100 \cdot (GDPV/GDPVTR-1)$.
+ [ ] GAPLFP (deviation from trend of labor force).
+ [ ] GDPVDTR (Potential of total economy, at 2010 PPP, USD. Formula: $GDPVD/GDPV \cdot GDPDTR$.
+ [ ] Inequality 
+ [ ] Gini Market outcome vs Net outcome.


# Meeting on 2016-02-22

+ Joining dataset: federalism. Natural resources rents % of GDP. (WDI). World development institution. Rest database.
+ Openness: (Import + Export)/GDPV. OECD Data base (XGS + MGS)/GDP for openness.
+ Government revenue $100*YRG/GDP$.
+ Net Lending should be $100*NLG/GDP$.
+ Data from 1985.
+ Wage share -> wage/gdp.
+ Levels and not logs for data prepration
+ Gini relative ratio.
+ At least one model with the three determinants. 
+ Let's stick GDP Growth.
+ Interaction effect.
+ Take difference as dependent variable, but lagged level.
+ Nickel bias (ICKKL bias). GMM.
+ Lagged variables on the right hand side.

# Meeting on 2016-03-08

+ Clustered errors in regression (heteroscedasticiy).
+ Revenue Stronger during election. interact election with baseline.
+ Use other dummies.
+ Compare annual data compare.
+ Election dummies for annual data is summed over a year (3/4, 1/4) if the lection is in first Quarter.
+ GGFLMQ.
+ EBA Extreme base averaging.
