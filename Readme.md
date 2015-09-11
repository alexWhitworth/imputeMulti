imputeMulti: Imputation methods for multivariate multinomial data
====

## Release notes:
- V0.0.2 includes methods for EM and Data Augmentation
- Methods are implemented in R. A future version will migrate the core algorithms to C++ / RCPP for performance.

## Features:
- Uses S4 classes throughout.
- imputation methods for multivariate multinomial data.
- Imputation of the summary statistics is done using EM and data augmentation
- Imputation of the observation level data is done via MLE
- allows for the following priors:
```
\code{c("none", "data.dep", "flat.prior", "non.informative")}
```
- If data dependent priors are chosen, calculated from approximately 20% of the observations data (assuming 20% of data are complete cases). If less than 20% of data are complete cases, all complete cases are used.

## Installation
- From Github
```
library("devtools");
install_github("alexwhitworth/imputeMulti",dependencies=TRUE)
```

## References:
1. Schafer, Joseph L. Analysis of incomplete multivariate data. Chapter 7. CRC press, 1997.
2. Darnieder, William Francis. Bayesian methods for data-dependent priors. Diss. The Ohio State University, 2011.






