imputeMulti: Imputation methods for multivariate multinomial data
====

## Release notes:
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
- From Github
```
library("devtools");
install_github("alexwhitworth/imputeMulti",dependencies=TRUE)
```

## Example usage:
```
### Create a toy data frame of multivariate multinomial data
### with missing data
set.seed(12315)
x1 <- factor(sample(1:5, size=100, replace= TRUE))
x2 <- factor(sample(6:10, size=100, replace= TRUE))
x3 <- factor(sample(11:15, size=100, replace= TRUE))
x4 <- factor(sample(16:20, size=100, replace= TRUE))
x5 <- factor(sample(21:26, size=100, replace= TRUE))

dat <- c(x1, x2, x3, x4, x5)
mis.ind <- sample(1:length(dat), size= 75, replace= FALSE)
dat[mis.ind] <- NA
dim(dat)<- c(100, 5)
rm(x1,x2,x3,x4,x5, mis.ind)

# ensure all factors
dat <- data.frame(apply(dat, 2, function(x) as.factor(x)))

## load library, usage for non-informative priors for both EM and DA
library(imputeMulti)
test_em <- multinomial_impute(dat, method= "EM",
                              conj_prior = "non.informative", verbose= TRUE)

test_da <- multinomial_impute(dat, method= "DA",
                                 conj_prior = "non.informative", verbose= TRUE)
```

## References:
1. Schafer, Joseph L. Analysis of incomplete multivariate data. Chapter 7. CRC press, 1997.
2. Darnieder, William Francis. Bayesian methods for data-dependent priors. Diss. The Ohio State University, 2011.






