context("int- count_levels works")
options(mc.cores = 2L)

test_that("errors work", {
  df <- data.frame(x=rnorm(100))
  expect_error(count_levels(dat=df, hasNA= "absolutely", parallel= FALSE))
  expect_error(count_levels(dat=df, hasNA= "count", parallel= FALSE))
  expect_error(count_levels(dat=df, hasNA= "count.obs", parallel= TRUE,
                            cores= -1))
  expect_error(count_levels(dat=df, hasNA= "count.obs", parallel= TRUE,
                            cores= 1.5))
})

test_that("count levels works with all missing data options... parallel = FALSE", {
  cnt.comp <- count_levels(dat[complete.cases(dat),], enum= enum_w_miss,
                           hasNA= "no", parallel= FALSE)
  cnt.ob <- count_levels(dat, enum= enum_w_miss, hasNA= "count.obs", parallel= FALSE)
  cnt.mis <- count_levels(dat[!complete.cases(dat),], enum= enum_w_miss,
                          hasNA= "count.miss", parallel= FALSE)

  ### no missing data tests
  expect_equal(sum(cnt.comp$counts), sum(complete.cases(dat)))
  expect_equal(sum(cnt.comp$counts == 0), 0)
  expect_lt(nrow(cnt.comp), nrow(enum_w_miss))
  expect_lt(ncol(enum_w_miss), ncol(cnt.comp))

  ### missing data tests -- count.obs
  expect_equal(sum(cnt.ob$counts == 0), 0)
  expect_lt(nrow(cnt.ob), nrow(enum_w_miss))
  expect_lt(ncol(enum_w_miss), ncol(cnt.ob))

  ### missing data tests -- count.miss
  expect_equal(sum(cnt.mis$counts), sum(!complete.cases(dat)))
  expect_equal(sum(cnt.mis$counts == 0), 0)
  expect_lt(nrow(cnt.mis), nrow(enum_w_miss))
  expect_lt(ncol(enum_w_miss), ncol(cnt.mis))
  expect_equal(sum(cnt.mis$counts) + sum(cnt.comp$counts), nrow(dat))
})

test_that("count levels works with all missing data options... parallel = TRUE", {
  cnt.comp <- count_levels(dat[complete.cases(dat),], enum= enum_w_miss,
                           hasNA= "no", parallel= TRUE)
  cnt.ob <- count_levels(dat, enum= enum_w_miss, hasNA= "count.obs", parallel= TRUE)
  cnt.mis <- count_levels(dat[!complete.cases(dat),], enum= enum_w_miss,
                          hasNA= "count.miss", parallel= TRUE)

  ### no missing data tests
  expect_equal(sum(cnt.comp$counts), sum(complete.cases(dat)))
  expect_equal(sum(cnt.comp$counts == 0), 0)
  expect_lt(nrow(cnt.comp), nrow(enum_w_miss))
  expect_lt(ncol(enum_w_miss), ncol(cnt.comp))

  ### missing data tests -- count.obs
  expect_equal(sum(cnt.ob$counts == 0), 0)
  expect_lt(nrow(cnt.ob), nrow(enum_w_miss))
  expect_lt(ncol(enum_w_miss), ncol(cnt.ob))

  ### missing data tests -- count.miss
  expect_equal(sum(cnt.mis$counts), sum(!complete.cases(dat)))
  expect_equal(sum(cnt.mis$counts == 0), 0)
  expect_lt(nrow(cnt.mis), nrow(enum_w_miss))
  expect_lt(ncol(enum_w_miss), ncol(cnt.mis))
  expect_equal(sum(cnt.mis$counts) + sum(cnt.comp$counts), nrow(dat))
})


test_that("(parallel = TRUE) == (parallel = FALSE)", {
  # parallel = TRUE
  cnt.comp1 <- count_levels(dat[complete.cases(dat),], enum= enum_w_miss,
                           hasNA= "no", parallel= TRUE)
  cnt.ob1 <- count_levels(dat, enum= enum_w_miss, hasNA= "count.obs", parallel= TRUE)
  cnt.mis1 <- count_levels(dat, enum= enum_w_miss, hasNA= "count.miss", parallel= TRUE)

  # parallel = FALSE
  cnt.comp2 <- count_levels(dat[complete.cases(dat),], enum= enum_w_miss,
                           hasNA= "no", parallel= FALSE)
  cnt.ob2 <- count_levels(dat, enum= enum_w_miss, hasNA= "count.obs", parallel= FALSE)
  cnt.mis2 <- count_levels(dat, enum= enum_w_miss, hasNA= "count.miss", parallel= FALSE)

  ### equality with parallel options
  expect_equal(cnt.comp1, cnt.comp2)
  expect_equal(cnt.ob1, cnt.ob2)
  expect_equal(cnt.mis1, cnt.mis2)
})
