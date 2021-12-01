
library(testthat)
library(imputeMulti)

context("int- impute multinomial")

test_that("missing value imputation works", {
  ### set up testing inputs:
  enum_comp <- enum_w_miss[complete.cases(enum_w_miss),]
  enum_miss <- enum_w_miss[!complete.cases(enum_w_miss),]
  
  dat_comp <- dat[complete.cases(dat),]
  dat_miss <- dat[!complete.cases(dat),]
  
  # 01. Run 6 iterations, impute missing values
  #------------------------------------
  iter6 <- multinomial_em(x_y= x_y, z_Os_y= z_Os_y, enum_comp= enum_comp, 
                          n_obs= nrow(dat), conj_prior= "none", 
                          verbose= FALSE, max_iter= 6) 
  
  dat_miss2 <- impute_multinomial_all(dat_miss, iter6@mle_x_y, p=ncol(dat))
  imputed_data <- rbind(dat_comp, dat_miss2)
  
  ### tests ###
  #------------------------------------
  expect_equal(sum(!complete.cases(imputed_data)), 0)
  expect_equal(sum(complete.cases(dat_miss2)), nrow(dat_miss2))
  expect_equal(
    lapply(dat_miss2, levels), lapply(dat, levels)
  )
})



