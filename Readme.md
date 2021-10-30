imputeMulti: Imputation methods for multivariate multinomial data
====

<!-- badges: start -->
    [![Build Status](https://travis-ci.org/alexWhitworth/imputeMulti.svg?branch=master)](https://travis-ci.org/alexWhitworth/imputeMulti.svg?branch=master)
    [![Codecov test coverage](https://codecov.io/gh/alexWhitworth/imputeMulti/branch/master/graph/badge.svg)](https://codecov.io/gh/alexWhitworth/imputeMulti?branch=master)
    [![CRAN version](http://www.r-pkg.org/badges/version/imputeMulti)](https://cran.r-project.org/package=imputeMulti)
<!-- badges: end -->


## Features:
- Uses S4 classes.
- imputation methods for multivariate multinomial data.
- Imputation of the summary statistics is done using EM and data augmentation
- Imputation of the observation level data is done via MLE
- allows for the following priors:
```
c("none", "data.dep", "flat.prior", "non.informative")
```
- If data dependent priors are chosen, calculated from approximately 20% of the observations data (assuming 20% of data are complete cases). If less than 20% of data are complete cases, all complete cases are used.

## Installation
- From CRAN
```
install.packages("imputeMulti")
```

- From Github
```
library("devtools");
install_github("alexwhitworth/imputeMulti",dependencies=TRUE)
```

## Example usage:
```
## load library and example data
library(imputeMulti)
data(tract2221)

# usage for non-informative priors for both EM and DA
# other priors may also be specified (not shown)
test_em <- multinomial_impute(tract2221[,1:4], method= "EM",
                              conj_prior = "non.informative", verbose= TRUE)

test_da <- multinomial_impute(tract2221[,1:4], method= "DA",
                                 conj_prior = "non.informative", verbose= TRUE)

# extract imputed values and parameter estimates
get_imputations(test_em)
get_parameters(test_em)
```

## References:
1. Schafer, Joseph L. Analysis of incomplete multivariate data. Chapter 7. CRC press, 1997.
2. Darnieder, William Francis. Bayesian methods for data-dependent priors. Diss. The Ohio State University, 2011.
