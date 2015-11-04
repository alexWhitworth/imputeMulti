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
  # \code{"count.miss"} - there are missing values, count the full observed-and-missing patterns
# @param parallel Logical. Do you wish to parallelize the code? Defaults to \code{TRUE}
# @param leave_cores How many cores do you wish to leave open to other processing?
#
count_levels <- function(dat, enum_list, hasNA= c("no", "count.obs", "count.miss"),
                         parallel= TRUE, leave_cores= 1L) {
  # parameter checking
  hasNA <- match.arg(hasNA, several.ok= FALSE)
  if (parallel == TRUE) {
    if (leave_cores < 0 | leave_cores %% 1 != 0) stop("leave_cores must be an integer >= 0")
  }
  if (ncol(dat) != ncol(enum_list)) stop("ncol(dat) and ncol(enum_list) must match.")

  # convert from factors to integers
  e2 <- apply(enum_list, 2, as.integer)
  dat2 <- apply(dat, 2, as.integer)

  # get counts
  if (parallel == FALSE) {
    enum_list$counts <- count_compare(x= e2, dat= dat2, hasNA= hasNA)
  } else {
    # resolve edge case when nnodes > nrow(dat2)
    nnodes <- min(nrow(dat2), detectCores() - leave_cores)
    cl <- makeCluster(nnodes)
    temp <- do.call("cbind", clusterApply(cl,
          # split data across clusters, share: comparison (e2) and hasNA
          x= parallel:::splitRows(dat2, nnodes), fun= function(x, e2, hasNA) {
            return(imputeMulti:::count_compare(x= e2, dat= x, hasNA= hasNA))
            # wrapper function needed for parameter-name-confusion b/w clusterApply
            # and count_compare
          }, e2= e2, hasNA= hasNA))
    enum_list$counts <- apply(temp, 1, sum)
    stopCluster(cl)
  }
  # return
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
marg_complete_compare <- function(marg, complete, marg_to_complete= FALSE) {
  ## 0. Pre-processing: convert factors to integers
  marg <- apply(marg, 2, as.integer)
  complete <- apply(complete, 2, as.integer)

  ## 1. Run code in C
  .Call('imputeMulti_marg_comp_compare', PACKAGE = 'imputeMulti',
        marg, complete, marg_to_complete)
}

