#### internal
# #' @title Count Levels
# #' @description Given a dataset and a data.frame of comparison patterns, 
# #' count the number of occurances of each pattern.
# #' @param dat A \code{data.frame}. All variables must be factors
# #' @param enum_list A \code{data.frame} consisting of all possible patterns for matching
# #' with \code{dat}.
# #' @param hasNA A string. Denotes if \code{dat} has complete data or not. If missing
# #' then should only observed patterns be counted or should missing patterns be counted?
count_levels <- function(dat, enum_list, hasNA= c("no", "count.obs", "count.miss")) {
  hasNA <- match.arg(hasNA, several.ok= FALSE)
  
  enum_list$counts <- NA
  # get counts
  if(hasNA == "no") {
    #------------- language agnostic code... R implementation below
    #     for (i in 1:nrow(enum_list)) {
    #       enum_list$counts[i] <- sum(apply(dat, 1, function(x, case) all(x == case), 
    #                                        case= enum_list[i, -ncol(enum_list)]))
    #     }
    #-------------
    enum_list$counts <- apply(enum_list[, -ncol(enum_list)], 1, function(x, dat) {
      sum(apply(dat, 1, function(y, case) all(y == case), case= x))
    }, dat= dat)
    
  } else if (hasNA == "count.obs") {
    dat <- dat[!complete.cases(dat),]; options(warn= -1) # length warnings
    #-------------language agnostic code... R implementation below
    #     for (i in 1:nrow(enum_list)) {
    #       enum_list$counts[i] <- sum(apply(dat, 1, function(x, case) all(x == case, na.rm=TRUE), 
    #                                        case= enum_list[i, -ncol(enum_list)]))
    #     }
    #-------------
    enum_list$counts <- apply(enum_list[, -ncol(enum_list)], 1, function(x, dat) {
      sum(apply(dat, 1, function(y, case) {
        all(which(is.na(y)) == which(is.na(case))) & all(y == case, na.rm= TRUE)
      }, case= x))
    }, dat= dat)
  } else if (hasNA == "count.miss") {
    dat <- dat[!complete.cases(dat),]; options(warn= -1) # length warnings
    #------------- language agnostic code... R implementation below
    #     for (i in 1:nrow(enum_list)) {
    #       enum_list$counts[i] <- sum(apply(dat, 1, function(x, case) {
    #         which(is.na(x)) == which(is.na(case)) && all(x == case, na.rm= TRUE)
    #       }, case= enum_list[i, -ncol(enum_list)]))
    #     }
    #-------------
    enum_list$counts <- apply(enum_list[, -ncol(enum_list)], 1, function(x, dat) {
      sum(apply(dat, 1, function(y, case) {
        all(which(is.na(y)) == which(is.na(case))) & all(y == case, na.rm= TRUE)
      }, case= x))
    }, dat= dat)
  }
  options(warn= 0)
  return(enum_list[enum_list$counts > 0,])
}


#' @description Compare an array with missing values \code{marg} and an array  
#' with complete values \code{complete}. Return matching indices. Can compare
#' either marginal-to-complete or complete-to-marginal.
#' @param marg A two dimensional array with missing values
#' @param complete A two dimensional array without missing values
#' @param marg_to_comp Logical. Do you wish to compare marginal values to 
#' complete values/matches? Defaults to \code{FALSE} ie- complete values compared
#' to marginal matches.
#' @return A \code{list} of matches.
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

