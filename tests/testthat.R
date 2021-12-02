library(testthat)
library(imputeMulti)
library(parallel)

# dat <- readRDS(file= system.file("tests", "testdata", 'data-dat.rds'
#                                  , package= "imputeMulti"))
# enum_w_miss <- readRDS(file= system.file("tests", "testdata", 'data-enum_w_miss.rds'
#                                          , package= "imputeMulti"))
# enum_wo_miss <- readRDS(file= system.file("tests", "testdata", 'data-enum_wo_miss.rds'
#                                           , package= "imputeMulti"))
# x_y <- readRDS(file= system.file("tests", "testdata", 'data-x_y.rds'
#                                  , package= "imputeMulti"))
# z_Os_y <- readRDS(file= system.file("tests", "testdata", 'data-z_Os_y.rds'
#                                  , package= "imputeMulti"))

test_check("imputeMulti")
