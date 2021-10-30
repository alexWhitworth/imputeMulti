

library(testthat)
library(imputeMulti)

context("multinomial DA")


test_that("basic error and CP error checks", {
  expect_error(multinomial_em(conj_prior= "foo"))
  expect_error(multinomial_em(conj_prior= c("none", "data.dep")))
  expect_error(multinomial_em(conj_prior= "data.dep", alpha= NULL))
  expect_error(multinomial_em(conj_prior= "flat.prior", alpha= NULL))
  expect_error(multinomial_em(conj_prior= "flat.prior", alpha= c(1,2,3)))
  expect_error(multinomial_em(conj_prior= "flat.prior", alpha= rnorm(100)))
  
  ## now run test:
  expect_error(multinomial_data_aug(enum_comp= enum_wo_miss, conj_prior= "data.dep",
                              alpha= 1))
  expect_error(multinomial_data_aug(enum_comp= enum_wo_miss, conj_prior= "data.dep",
                              alpha= vector("numeric", length= nrow(enum_wo_miss))))
})


test_that("multinomial DA is converging", {
  ### set up testing inputs:
  enum_comp <- enum_w_miss[complete.cases(enum_w_miss),]
  enum_miss <- enum_w_miss[!complete.cases(enum_w_miss),]

  # 03. Run 20 iterations, make sure log-lik is increasing
  #------------------------------------
  iter1 <- multinomial_data_aug(x_y= x_y, z_Os_y= z_Os_y, enum_comp= enum_comp,
                          conj_prior= "none", verbose= FALSE, burnin= 1)
  iter20 <- multinomial_data_aug(x_y= x_y, z_Os_y= z_Os_y, enum_comp= enum_comp,
                          conj_prior= "none", verbose= FALSE, burnin= 20)
  iter10 <- multinomial_data_aug(x_y= x_y, z_Os_y= z_Os_y, enum_comp= enum_comp,
                          conj_prior= "none", verbose= FALSE, burnin= 10)

  # tests:
  expect_lt(iter1@mle_log_lik, iter20@mle_log_lik)
  expect_lt(iter1@mle_log_lik, iter10@mle_log_lik)
  expect_null(iter20@mle_x_y$alpha) # no prior
  expect_null(iter20@mle_x_y$counts)
  expect_null(iter20@mle_x_y$theta_y1)
  expect_equal(iter20@mle_cp, "none")
  expect_equal(iter20@mle_iter, 20)
  expect_equal(sum(iter20@mle_x_y$theta_y), 1)

})