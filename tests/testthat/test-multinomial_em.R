

library(testthat)
library(imputeMulti)

context("multinomial EM")


test_that("basic error and CP error checks", {
  expect_error(multinomial_em(conj_prior= "foo"))
  expect_error(multinomial_em(conj_prior= c("none", "data.dep")))
  expect_error(multinomial_em(conj_prior= "data.dep", alpha= NULL))
  expect_error(multinomial_em(conj_prior= "flat.prior", alpha= NULL))
  expect_error(multinomial_em(conj_prior= "flat.prior", alpha= c(1,2,3)))
  expect_error(multinomial_em(conj_prior= "flat.prior", alpha= rnorm(100)))
  
  expect_error(multinomial_em(enum_comp= enum_wo_miss, conj_prior= "data.dep",
                              alpha= 1))
  expect_error(multinomial_em(enum_comp= enum_wo_miss, conj_prior= "data.dep",
                              alpha= vector("numeric", length= nrow(enum_wo_miss))))
})


test_that("multinomial EM is converging", {
  ### set up testing inputs:
  enum_comp <- enum_w_miss[complete.cases(enum_w_miss),]
  enum_miss <- enum_w_miss[!complete.cases(enum_w_miss),]
  # 01. Run 6 iterations, make sure log-lik is increasing
  #------------------------------------
  iter2 <- multinomial_em(x_y= x_y, z_Os_y= z_Os_y, enum_comp= enum_comp, 
                          n_obs= nrow(dat), conj_prior= "none", 
                          verbose= FALSE, max_iter= 2)
  iter4 <- multinomial_em(x_y= x_y, z_Os_y= z_Os_y, enum_comp= enum_comp, 
                          n_obs= nrow(dat), conj_prior= "none", 
                          verbose= FALSE, max_iter= 4) 
  iter6 <- multinomial_em(x_y= x_y, z_Os_y= z_Os_y, enum_comp= enum_comp, 
                          n_obs= nrow(dat), conj_prior= "none", 
                          verbose= FALSE, max_iter= 6) 
  
  # tests:
  expect_lt(iter2@mle_log_lik, iter4@mle_log_lik)
  expect_lt(iter2@mle_log_lik, iter6@mle_log_lik)
  expect_lte(iter2@mle_log_lik, iter6@mle_log_lik)
  expect_null(iter6@mle_x_y$alpha) # no prior
  expect_null(iter6@mle_x_y$counts)
  expect_null(iter6@mle_x_y$theta_y1)
  expect_equal(iter6@mle_cp, "none")
  expect_equal(iter2@mle_iter, 2)
  expect_equal(sum(iter6@mle_x_y$theta_y), 1)
  
})
