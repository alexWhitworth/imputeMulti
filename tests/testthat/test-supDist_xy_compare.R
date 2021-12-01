
library(testthat)
library(imputeMulti)

context("test helper functions")

test_that("supDist errors and results", {
  set.seed(315)
  x1 <- rnorm(10)
  x2 <- rnorm(100)
  y  <- rnorm(100)
  
  expect_error(supDist(x1, y)) 
  expect_error(supDist(y, x1))
  
  expect_equal(supDist(x2,y), max(abs(x2-y)))
})

test_that("fact_to_int", {
  # 0. gen test data
  set.seed(2345)
  x <- sample.int(10, 5, replace=FALSE)
  y <- sample(letters[1:10], 10, replace= FALSE)
  xx <- as.character(sample.int(10, 5, replace= TRUE))
  
  # 1. test
  expect_equal(imputeMulti:::fact_to_int(x), x)
  expect_equal(imputeMulti:::fact_to_int(y), y)
  expect_equal(imputeMulti:::fact_to_int(factor(x)), c(2,4,5,1,3))
  expect_equal(imputeMulti:::fact_to_int(factor(y)), c(3,2,1,4,8,9,5,6,10,7))
  expect_true(is.integer(imputeMulti:::fact_to_int(factor(xx))))
  expect_true(is.integer(imputeMulti:::fact_to_int(factor(y))))
}) 


#--------------------------------------
context("int- xy_compare works")

test_that("errors work; return type is correct", {
  x <- 1:5; dim(x) <- c(1,5)
  x <- rbind(x,x)
  x2 <- 1:6; dim(x2) <- c(1,6)
  x2 <- rbind(x2,x2)
  x3 <- x2[,1:5]
  
  expect_error(mx_my_compare(x, x2))
  expect_error(mx_my_compare(x, x2))
})

test_that("xy_compare and mx_my_compare work correctly", {
  # set up
  set.seed(125)
  x1 <- factor(sample(1:5, size=10, replace= TRUE))
  x2 <- factor(sample(6:10, size=10, replace= TRUE))
  x3 <- factor(sample(11:15, size=10, replace= TRUE))
  x4 <- factor(sample(16:20, size=10, replace= TRUE))
  x5 <- factor(sample(21:26, size=10, replace= TRUE))
  
  dat2 <- dat <- c(x1, x2, x3, x4, x5)
  # insert missing values
  dim(dat)<- dim(dat2) <- c(10, 5)
  dat <- t(apply(dat, 1, function(x) {x[sample.int(length(x),1)] <- NA; return(x)}))
  rm(x1,x2,x3,x4,x5)
  
  set.seed(125)
  dat2 <- data.frame(x1= factor(sample(1:5, size=10, replace= TRUE)),
                     x2= factor(sample(6:10, size=10, replace= TRUE)),
                     x3= factor(sample(11:15, size=10, replace= TRUE)),
                     x4= factor(sample(16:20, size=10, replace= TRUE)),
                     x5= factor(sample(21:26, size=10, replace= TRUE)))
  
  # insert missing values
  dat <- dat2
  dat[c(3,7),1] <- NA
  dat[c(1,5),2] <- NA
  dat[c(2,6),3] <- NA
  dat[c(7,9),4] <- NA
  dat[c(4,10),5] <- NA
  
  expect_equal(unlist(imputeMulti:::mx_my_compare(dat2, dat2)), 1:10)
  expect_equal(unlist(imputeMulti:::mx_my_compare(dat, dat2)), 1:10)
  
  ## on matrices
  # set.seed(14)
  # mx <- matrix(as.character(sample.int(10, 50, replace= TRUE)), ncol= 5)
  # my <- matrix(sample.int(10, 50, replace= TRUE), ncol= 5)
  # mx <- apply(mx, 2, factor); my<- apply(my, 2, factor)
  # 
  # expect_equal(unlist(imputeMulti:::mx_my_compare(mx,my)), vector("integer", length= 0))
  # expect_equal(unlist(imputeMulti:::mx_my_compare(my,mx)), vector("integer", length= 0))
})


