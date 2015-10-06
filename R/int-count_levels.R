#### internal
# @title Count Levels
# @description Given a dataset and a data.frame of comparison patterns, 
# count the number of occurances of each pattern.
# @param dat A \code{data.frame}. All variables must be factors
# @param enum_list A \code{data.frame} consisting of all possible patterns for matching
# with \code{dat}.
# @param hasNA A string. Denotes if \code{dat} has complete data or not. 
  # \code{"no"} - there are no missing values, count observed patterns
  # \code{"count.obs"} - there are missing values, count the marginally observed patterns
  # \code{"count.obs"} - there are missing values, count the full observed-and-missing patterns
# @param parallel Logical. Do you wish to parallelize the code? Defaults to \code{TRUE}
# @param leave_cores How many cores do you wish to leave open to other processing?
# 
count_levels <- function(dat, enum_list, hasNA= c("no", "count.obs", "count.miss"),
                         parallel= TRUE, leave_cores= ifelse(detectCores() <= 4, 1, 2)) {
  hasNA <- match.arg(hasNA, several.ok= FALSE)
  if (parallel == TRUE) {
    if (leave_cores < 0 | leave_cores %% 1 != 0) stop("leave_cores must be an integer >= 0")
  }
  
  enum_list$counts <- NA
  # get counts
  if(hasNA == "no") {
    if (parallel == FALSE) {
      enum_list$counts <- apply(enum_list[, -ncol(enum_list)], 1, function(x, dat) {
        sum(apply(dat, 1, function(y, case) all(y == case), case= x))
      }, dat= dat)
    } else {
      cl <- makeCluster(detectCores() - leave_cores)
      enum_list$counts <- parRapply(cl, enum_list[, -ncol(enum_list)], function(x, dat) {
        sum(apply(dat, 1, function(y, case) all(y == case), case= x))
      }, dat= dat)
      stopCluster(cl)
    }
  } else if (hasNA == "count.obs") {
    dat <- dat[!complete.cases(dat),]; options(warn= -1) # length warnings
    
    if (parallel == FALSE) {
      enum_list$counts <- apply(enum_list[, -ncol(enum_list)], 1, function(x, dat) {
        sum(apply(dat, 1, function(y, case) {
          case_obs <- !any(is.na(case[is.na(y)])) # no NA in case where y is NA 
          val_eq <- all(y[!is.na(y)] == case[!is.na(y)]) # obs values equal
          return(all(case_obs, val_eq))
        }, case= x), na.rm= TRUE)
      }, dat= dat)
    } else {
      cl <- makeCluster(detectCores() - leave_cores)
      enum_list$counts <- parRapply(cl, enum_list[, -ncol(enum_list)], function(x, dat) {
        sum(apply(dat, 1, function(y, case) {
          case_obs <- !any(is.na(case[is.na(y)])) # no NA in case where y is NA 
          val_eq <- all(y[!is.na(y)] == case[!is.na(y)]) # obs values equal
          return(all(case_obs, val_eq))
        }, case= x), na.rm= TRUE)
      }, dat= dat)
      stopCluster(cl)
    }
  } else if (hasNA == "count.miss") {
    dat <- dat[!complete.cases(dat),]; options(warn= -1) # length warnings
    if (parallel == FALSE) {
      enum_list$counts <- apply(enum_list[, -ncol(enum_list)], 1, function(x, dat) {
        sum(apply(dat, 1, function(y, case) {
          num_na_y <- sum(is.na(y)); num_na_case <- sum(is.na(case))
          num_na_eq <- num_na_y == num_na_case # same number missing
          ind_na_eq <- ifelse(num_na_y > 0 & num_na_case > 0, 
                              all(which(is.na(y)) == which(is.na(case))), num_na_eq) # same indices missing
          val_eq <- all(y[!is.na(y)] == case[!is.na(y)]) # obs values equal
          return(all(num_na_eq, ind_na_eq, val_eq))
        }, case= x))
      }, dat= dat)
    } else {
      cl <- makeCluster(detectCores() - leave_cores)
      enum_list$counts <- parRapply(cl, enum_list[, -ncol(enum_list)], function(x, dat) {
        sum(apply(dat, 1, function(y, case) {
          num_na_y <- sum(is.na(y)); num_na_case <- sum(is.na(case))
          num_na_eq <- num_na_y == num_na_case # same number missing
          ind_na_eq <- ifelse(num_na_y > 0 & num_na_case > 0, 
                              all(which(is.na(y)) == which(is.na(case))), num_na_eq) # same indices missing
          val_eq <- all(y[!is.na(y)] == case[!is.na(y)]) # obs values equal
          return(all(num_na_eq, ind_na_eq, val_eq))
        }, case= x))
      }, dat= dat)
      stopCluster(cl)
    }
  }
  options(warn= 0)
  return(enum_list[!is.na(enum_list$counts) & enum_list$counts > 0,])
}


# @description Compare an array with missing values \code{marg} and an array  
# with complete values \code{complete}. Return matching indices. Can compare
# either marginal-to-complete or complete-to-marginal.
# @param marg A two dimensional array with missing values
# @param complete A two dimensional array without missing values
# @param marg_to_comp Logical. Do you wish to compare marginal values to 
# complete values/matches? Defaults to \code{FALSE} ie- complete values compared
# to marginal matches.
# @return A \code{list} of matches.
marg_comp_compare <- function(marg, complete, marg_to_comp= FALSE) {
  if (marg_to_comp == FALSE) {
    return(apply(complete, 1, function(comparison) {
      which(apply(marg, 1, function(x, comparison) all(x == comparison, na.rm=TRUE), 
                  comparison= comparison))  
    }))
  } else {
    return(apply(marg, 1, function(comparison) {
      which(apply(complete, 1, function(x, comparison) all(x == comparison, na.rm=TRUE), 
                  comparison= comparison))  
    }))
  }
}

# sup of L1 distance between x and y
supDist <- function (x, y) return (max (abs (x - y)))


