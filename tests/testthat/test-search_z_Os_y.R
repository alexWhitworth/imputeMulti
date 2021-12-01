
library(testthat)
library(imputeMulti)

context("int- search z_Os_y")

test_that("works as designed", {
  # as designed means that x_possible, x_y, and z_Os_y have been specified as anticipated
  #  (variable names, factor variables, counts, etc)
  data(tract2221)
  t2 <- tract2221[,1:4]
  x_p <- multinomial_stats(t2, "possible.obs")
  x_y <- multinomial_stats(t2, "x_y")
  z_Os_y <- multinomial_stats(t2, "z_Os_y")
  
  incomp_ind <- imputeMulti:::search_z_Os_y(z_Os_y, x_p)
  
  expect_equal(length(incomp_ind), nrow(z_Os_y))
  expect_true(is.list(incomp_ind))
  expect_true(all(unlist(lapply(incomp_ind, is.numeric))))
  expect_true(all(unlist(lapply(incomp_ind, is.integer))))
  expect_true(all(unlist(lapply(incomp_ind, function(l) all(l %% 1 == 0)))))
  expect_true(all(unlist(lapply(incomp_ind, length)) > 0))
})

#context("int- count_sumStats")

