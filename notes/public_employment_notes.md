---
title: Public employment project
author: David Pham
bibliography: biblio.bib
geometry:
    - margin=3cm
papersize: a4paper

---

<!-- pandoc -s -S  --number-sections --table-of-contents --biblio biblio.bib --csl chicago-author-date.csl  public_employment_notes.md -o public_employment_notes.pdf -->

# Introduction

The purpose of the project is to show if in Switzerland, the unemployment rate
are artificially kept low through public employment, allowed thanks fiscal
transparency (measured by several key indicators).

This hypothesis is backed by the following observations:

* Fiscal opacity allows incumbent government to manipulate official statistics,
  the scale of the gimmickries depends on the level of opacity [@alt2014isn];
* Public employment is mean for incumbent government to redistribute wealth from
  the well-off class to the low-income class [@alesina2000redistributive].
* Public employment is positively correlated with GDP growth in country with low
  fiscal transparency [@aaskoven2015fiscal].
* Switzerland is a country with low fiscal transparency
  [@aaskoven2015fiscal, p.14].

# Common variable

+ Public employment
+ Fiscal rules, fiscal transparency
+ Electoral incentives (number of electoral years)
+ Economics conditions
+ GDP growth
+ GDP per capita in constant price
+ Portion of population having a university degree 
+ Measure of inequality
+ country fixed effect
+ unemployment rate
+ money income per capita
+ log of city population
+ Ethnicity
+ Time effect
+ OECD countries
+ Government spending as percent of GDP
+ Left or right party

# Robustness analysis

* Taking out the state
* Outliers
* Total public spending per capita
* Checking the coefficient with and without inequality
* Fragmentation of type of public employment (central administration, streets
  and highway, housing and community developement, libraries, natural sciences,
  parks and recreation, sewerage, and solide waste management.

# Data

Comes from the oecd, the goal is to use annual and quarterly data. Codes are
described in `Database_Inventory.pdf`.

# References

