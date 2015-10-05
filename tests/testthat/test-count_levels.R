
library(testthat)
library(imputeMulti)

context("int- count_levels works")

test_that("errors work", {
  df <- data.frame(x=rnorm(100))
  expect_error(count_levels(dat=df, hasNA= "absolutely"))
  expect_error(count_levels(dat=df, hasNA= "count"))
})


test_that("count levels works with all missing data options", {
  # create some data
  set.seed(12315)
  x1 <- factor(sample(1:5, size=100, replace= TRUE))
  x2 <- factor(sample(6:10, size=100, replace= TRUE))
  x3 <- factor(sample(11:15, size=100, replace= TRUE))
  x4 <- factor(sample(16:20, size=100, replace= TRUE))
  x5 <- factor(sample(21:26, size=100, replace= TRUE))
  
  dat <- c(x1, x2, x3, x4, x5)
  # insert some missing values
  mis.ind <- sample(1:length(dat), size= 75, replace= FALSE)
  dat[mis.ind] <- NA
  rm(x1,x2,x3,x4,x5, mis.ind)
  dim(dat)<- c(100, 5)
  dat <- data.frame(apply(dat, 2, function(x) as.factor(x)))
  
  enum <- expand.grid(sapply(dat, function(x) return(c(levels(x), NA))))
  cnt.comp <- count_levels(dat[complete.cases(dat),], enum_list= enum, hasNA= "no")
  cnt.ob <- count_levels(dat, enum_list= enum, hasNA= "count.obs")
  cnt.mis <- count_levels(dat, enum_list= enum, hasNA= "count.miss")
  
  ### no missing data tests
  expect_equal(sum(cnt.comp$counts), sum(complete.cases(dat)))
  expect_equal(sum(cnt.comp$counts == 0), 0)
  expect_less_than(nrow(cnt.comp), nrow(enum))
  expect_less_than(ncol(enum), ncol(cnt.comp))
  
  ### missing data tests -- count.obs
  expect_equal(sum(cnt.ob$counts == 0), 0)
  expect_less_than(nrow(cnt.ob), nrow(enum))
  expect_less_than(ncol(enum), ncol(cnt.ob))
  
  ### missing data tests -- count.miss
  expect_equal(sum(cnt.mis$counts), sum(!complete.cases(dat)))
  expect_equal(sum(cnt.mis$counts == 0), 0)
  expect_less_than(nrow(cnt.mis), nrow(enum))
  expect_less_than(ncol(enum), ncol(cnt.mis))
  expect_equal(sum(cnt.mis$counts) + sum(cnt.comp$counts), nrow(dat))
})