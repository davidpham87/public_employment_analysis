--- 
title: "Public Employment Project" 
toc: true
tocdepth: 2
numbersections: true
latexengine: pdflatex
header-includes:
    - \usepackage{amsmath}    
geometry:
    - margin=3cm
papersize: a4paper
    
---

<!-- header-includes:  -->
<!--     - "<style> body {margin: 100px 100px;} </style>" -->


# Paper notes

## It Isn't Just about Greece (James Alt, et al)

Analyzes the political origins of differences in adherence to the fiscal
framework of the EU.

It studies the interaction of opaque budget and fiscal policy for electoral
purposes falsify national statistics (`creative accounting`, `fiscal gimmickry`
[^fiscal_gimmickry])

Election year obscures statistics. The following explain variations in outcomes

+ Domestic institution (budget transparency)
+ Politics (elections)
+ Economic cycles (recessions)

and this enforces the saying 

> Source of fiscal discipline is at the domestic level.

*Gimmick* is the action of manipulating, embellish facts in order to alter
 reality:

+ Improve appearances of public finance statistics (budget balance, debt)
without effect of real statistics.
+ Asymmetric information in fiscal/economic unions.
+ *Misreport* of fiscal data, *hidden actions* (employing gimmicks involving
creative or unorthodox accounting treatments of operations to interpret rules or
cheating).

Incentives are audiences (voters, bond markets, EU). Three types of trade-off
actions:

1. Real adjustement to tax and expenditures, unpopular for voters; 
2. Do nothing, with penalties from the EU; 
3. Gimmickries, with intertemporal trade-off, future high bond yields, political
unrest if discovered.

Fiscal opacity is penalized by bond markets. Strategy: rule violation and
gimmickry with absence of market discipline. The fiscal transparency rarely
evolve through time, and if yes, seldom in the right direction.


### Stock-flow adjustments

\begin{align*}
 SAF=  D_t - D_{t-1} + B_t,
\end{align*}

where $D_t$ is the debt level at time $t$ and $B_t$ is the budget balance at
time $t$.  Critics about this measure: how can we correctly observe the changes?
For example, a decrease of debt of two with a surplus of the budget balance of 1
has the same statistics as an increase of debt of 1 and a deficit of the budget
balance of 2.

The SFA is decomposed into several components, and two of them are significant
for the study

- **shares and other equity**: used for translating net cash transfer (debt) as share purchasing.
  
- **other account payable** (OPA): goods and services that have been delivered
  but not yet paid for. The SFA increases when the OPA decreases.

### Explaining Gimmicks

- Transparency diminishes the appeal of gimmicks.

Other explanatory variables

1. Fiscal rules: if the conditions of the SGP are respected.
2. Electoral incentive: year left in the office for the incumbent government, the amount of gimmickry should be bigger with fewer years left.
3. Economics conditions: distinction between fast growth and below-trend growth.


[^fiscal_gimmickry]: Deviations from accepted and expected reporting practices


## Redistributive Public Employment

American cities public employment is used for redistributive purposes. It is a
disguises way of channeling resources from middle clas svoter to disadvantaged
citizens when an explicit tax-transfer scheme would not find political support.

Ethincally fragmented cities tend to have larger public employment. 

> But are the people employed by the government come from the disadvantaged
  groups?
  
### Theoretical framework

* A two period timeframe, with election after the first period.
* Two classes of voters: *middle* class and the *poor*.
* Two contestants for the government.
* Each contestant need the support of the middle class.
* Define $B$ as the benefit of a public project (employing the *poor* to complete it). Then $B \in \{B_L, B_H\}, 0 < B_L < B_H$ with  
\begin{align*}
B = 
\left\{
  \begin{array}{ll}
  B_L & \mbox{with probability } 1-\theta \\
  B_H & \mbox{with probability } \theta
  \end{array}
\right.
\end{align*}
with $\theta$ is random variable taking either $\theta_L$ or $\theta_H$ with $0 <
\theta_L < \theta_H < 1$. When $\theta=\theta_L$, it is more efficient to make a cash transfer than implemeting the project.

* The incumbent government observe the realisation of $\theta$ before deciding
  to implement a public project or not.
* Two type of contestant: one for the middle class and conduct the public
  project only if $\theta = \theta_H$, and one supporting the poor, implemeting
  the public project for any value of $\theta$ if the action does not prevent
  them from winning the next election.
* Voters ignore which type are the politicians, however they have perception
  (priors) about the incumbent and the challanger. 
* Depending on the priors of the voters for the incumbent, if he favors the
  poors, he might or not implement the public project even though it is not
  efficient.

### Data

All US cities with more than 25 000 inhabitants using official statistics form  the _City and County Databook, Censur of Governments.

### Regression

The following formula has been fitted

\begin{align*}
    Y_{i} = \mu + \beta_1 I_i + \beta_3 * X_i + \varepsilon_i         
\end{align*}

where 

* $Y_i$ is the government employment per 1000 population, or per 1000 working age
  population.
* $I_i$ is a measure of inequality (gini, mean/median income, percentage of
  person below the poverty level, percentage of families below the poverty
  level).
* $X_i$ is a data matrix containing the statistics: fraction of 25+ years old
  with a university degree, american state, unemployement rate, money income per
  thousand dollars, log of city population, or fraction of retired (65+)
  population and ethincity $1 - \sum_i (ethnic_i)^2$ where $ethnic_i$ is the
  share of population self-identified with ethnic origins $i$.

### Robustness

* Taking out the state
* Outliers
* Total public spending per capita
* Checking the coefficient with and without inequality
* Fragmentation of type of public employment (central administration, streets and highway, housing and community developement, libraries, natural sciences, parks and recreation, sewerage, and solide waste management.   

## Fiscal Transparency and Public Employment

### Framework

Incumbent government prefer to stay in the government even though they don't
take the right decision. Public employment is more popular than wealth transfer
and tax cut.

With windfall from gdp growth, country with low fiscal transparency will
increase their public employment.

### Control variable economic

* OECD countries between 1996 to 2010
* GDP per capita in constant price in order to control for the Wagner's Law
* Unemployment rate
* Government spending as percent of GDP
* Left or right party
* Election year
* Country fixed effect

The formula is given by, for country $i$ and time $t$:

\begin{align*}
Y_{it} = \alpha + \beta_1 G_{it} + \beta_2 G_{it} T_i + \beta_3 X_{it} + \eta_i + \tau_t + \varepsilon_{it},
\end{align*}
where the variables are

* $Y_{it}$, the public employment;
* $G_{it}$, GDP growth;
* $T_{i}$, fiscal transparency;
* $X$ is the vector of control variables;
* $\eta_i$, country fixed effect;
* $\tau_t$, year fixed effect;

Public employment also includes employee from government owned companies and is
defined as the ratio of people employed in the government and these companies
over the total work force.

### Robustness analysis

- The demographic is used as a control variable.
- Different index of sical transparency IMF's Reports on the Observance of
  Standards and Codes. Average of public information, budgetaryprocess,
  assurance of integrity. With this robustness methods, coefficient of $T_{it}$
  and $T_{it}G_{it}$ are only significant at the $0.1$ level.
- Exclusion of the beginning and ending of the period. Excluding Greece and New
  Zealand.

## Retreat of the state from entrepreneurial activities

This paper describes the evolution of privatization, deregulation in
network-based service, and the cutback of subsidies in 20 OECD countries. Our
interest lies in the evolution of the employment index used in the
privatization section.

### Relevant statistics

The paper use the ratio between the the number of employed persons by the
public government over total employment. The former is computed as a weighted
sum of employee between the following bodies:

- **Departmental Agencies (DA)**: public administrative bodies without their
  legal identity;
- **Public Corporations (PC)**: firms that are totally owned by the state but
  have a public legal body. These have a weight $\alpha = 0.75$ in the paper;
- **State Companies (SC) and Private Firms (PF)**: _SC_ are _PC_ which the
  states do not hold 100 percent of the shares. For _SC_ and _PF_, the weight
  of their number of employee is provided by $\beta \gamma$ where $\gamma$ is
  the percentage of public owned shares of the entreprise, and $\beta$ is set
  at $0.5$.

Missing value are interpolated when necessary and for small firms only there is
a cutoff of the 60% smallest firms.

## Common variables to consider for the analysis
