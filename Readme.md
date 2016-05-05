imputeMulti: Imputation methods for multivariate multinomial data
====

## Release notes:
- v0.4.5 - CRAN submission and accepted
- v0.4 - added example data, fixed some documentation, etc; closer to passing R CMD check
- v0.3 - utility functions have been moved to Rcpp.
- v0.2.2 Moving utility functions to Rcpp -- major speed improvements. In development until v0.3. Will migrate core (non-utility functions) to Rcpp for v0.4
- v0.2 Added parallel functionality.
- v01.2 all unit tests passed, minor debugging.
- V0.1 unit tests for package written and passed.
- V0.0.2 includes methods for EM and Data Augmentation
- Methods are implemented in R. A future version will migrate the core algorithms to C++ / RCPP for performance.

## Features:
- Uses S4 classes throughout.
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
```

## References:
1. Schafer, Joseph L. Analysis of incomplete multivariate data. Chapter 7. CRC press, 1997.
2. Darnieder, William Francis. Bayesian methods for data-dependent priors. Diss. The Ohio State University, 2011.






