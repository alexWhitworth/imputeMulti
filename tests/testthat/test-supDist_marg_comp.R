
library(testthat)
library(imputeMulti)

context("int- supDist works")

test_that("supDist errors and results", {
  set.seed(315)
  x1 <- rnorm(10)
  x2 <- rnorm(100)
  y  <- rnorm(100)
  
  expect_equal(supDist(x1, y), -1) # supDist returns -1 for error
  expect_equal(supDist(y, x1), -1)
  
  expect_equal(supDist(x2,y), max(abs(x2-y)))
})


#--------------------------------------
context("int- marg_compare works")

test_that("errors work; return type is correct", {
  x <- 1:5; dim(x) <- c(1,5)
  x <- rbind(x,x)
  x2 <- 1:6; dim(x2) <- c(1,6)
  x2 <- rbind(x2,x2)
  x3 <- x2[,1:5]
  
  expect_equal(marg_comp_compare(x, x2, FALSE), list())
  expect_equal(marg_comp_compare(x, x2, TRUE), list())
  # expect_error(marg_comp_compare(x, x2, "blah"), list())
  
  expect_true(is.list(marg_comp_compare(x, x3, FALSE)))
  expect_true(is.list(marg_comp_compare(x, x3, TRUE)))
  expect_equal(length(marg_comp_compare(x, x3, FALSE)), 2)
  expect_equal(length(marg_comp_compare(x, x3, TRUE)), 2)
})

test_that("marg_compare works correctly", {
  # set up
  set.seed(125)
  x1 <- factor(sample(1:5, size=10, replace= TRUE))
  x2 <- factor(sample(6:10, size=10, replace= TRUE))
  x3 <- factor(sample(11:15, size=10, replace= TRUE))
  x4 <- factor(sample(16:20, size=10, replace= TRUE))
  x5 <- factor(sample(21:26, size=10, replace= TRUE))
  
  dat2 <- dat <- c(x1, x2, x3, x4, x5)
  # insert missing values
  mis.ind <- sample(1:length(dat), size= 10, replace= FALSE)
  dat[mis.ind] <- NA
  rm(x1,x2,x3,x4,x5, mis.ind)
  dim(dat)<- dim(dat2) <- c(10, 5)
  
  expect_equal(unlist(marg_comp_compare(dat, dat, TRUE)), 1:10)
  expect_equal(unlist(marg_comp_compare(dat, dat, FALSE)), 1:10)
  expect_equal(unlist(marg_comp_compare(dat2, dat2, TRUE)), 1:10)
  expect_equal(unlist(marg_comp_compare(dat2, dat2, FALSE)), 1:10)
  expect_equal(unlist(marg_comp_compare(dat, dat2, TRUE)), 1:10)
  expect_equal(unlist(marg_comp_compare(dat, dat2, FALSE)), 1:10)
  
  
  ## make sure works with factor variables
  dat <- apply(dat, 2, function(x) as.factor(x))
  dat2 <- apply(dat2, 2, function(x) as.factor(x))
  
  expect_equal(unlist(marg_complete_compare(dat, dat, TRUE)), 1:10)
  expect_equal(unlist(marg_complete_compare(dat, dat, FALSE)), 1:10)
  expect_equal(unlist(marg_complete_compare(dat2, dat2, TRUE)), 1:10)
  expect_equal(unlist(marg_complete_compare(dat2, dat2, FALSE)), 1:10)
  expect_equal(unlist(marg_complete_compare(dat, dat2, TRUE)), 1:10)
  expect_equal(unlist(marg_complete_compare(dat, dat2, FALSE)), 1:10)
  
})


