# emacalc
A package to make working with ESM data easier

The emacalc package was built to make it easier to work with experience sampling data, and is especially useful when working with multiple ESM datasets, when exploring an unfamiliar a dataset for the first time, or when many calculations need to be made. 

It contains functions to automatically easily split data into person and moment level dataframes (by identifying the variables at each level), calculate person-level and day-level aggregates, flexibly lag variables at different levels, center variables, and remove individuals with pre-specified levels of missing data. 

## How to install

First, install and load the devtools package:

    install.packages('devtools')
    library(devtools)

Then, use the devtools package to install emacalc from this github repository:

    install_github('seanchrismurphy/emacalc')
    library(emacalc)

To view the details of individual functions, just run `help(emacalc)` once the package is installed.
