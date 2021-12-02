

# [4/2016] Moving supDist wrapper to R -- for R CMD Check
# wrapper to supDistC to move error checking outside of C++
supDist <- function(x,y) {
  if (length(x) != length(y)) stop("Length of x and y differ.")
  supDistC(x,y)
}

# convert a factor-vector to an integer vector, where the integers correspond
# to the levels of the factor.
fact_to_int <- function(f) {
  if (is.factor(f)) {
    l <- levels(f)
    return(unlist(
      sapply(f, function(i) {
        ifelse(!is.na(i), which(i == l), NA)
      })
    ))
  } else {
    return(f)
  }
}

# function to get the character mapping from a factor type
get_level_text <- function(var, val) {
  lvls <- levels(var)
  return(lvls[val])
}

# @description Compare two two-dimensional arrays (\code{mat_x}, \code{mat_y}), where \code{mat_x}
# permits missing values. Return a \code{list} of length \code{nrow(mat_x)} such that each list
# element contains a vector of row indices from \code{mat_y} with row-equivalence of the non
# missing values.
# @param DT_x A \code{data.table} which may contain missing values
# @param DT_y A \code{data.table} without missing values
# @return A \code{list} of matches.
mx_my_compare <- function(DT_x, DT_y) {
  if (ncol(DT_x) != ncol(DT_y)) stop("ncol of DT_x and DT_y do not match.")
  ## 0. Pre-processing: convert factors to integers
  data.table::setDT(DT_x); data.table::setDT(DT_y)
  res <- vector("list", length= nrow(DT_x))
  join_cols <- names(DT_x)
  DT_y[, rowid := .I]
  
  for (s in seq_len(nrow(DT_x))) {
    tmp <- DT_x[s,]
    na_idx <- which(apply(tmp, 1, is.na))
    if (length(na_idx) > 0) {
      res[[s]] <- DT_y[tmp, on= join_cols[-na_idx]]$rowid  
    }
    else {
      res[[s]] <- DT_y[tmp, on= join_cols]$rowid
    }
  }
  
  DT_y[, rowid := NULL]
  return(res)
}


#### internal
# @title Count Levels
# @description Given a dataset and a data.frame of comparison patterns,
# count the number of occurrences of each pattern.
# @param dat A \code{data.frame}. All variables must be factors
# @param enum_list A \code{data.frame} consisting of all possible patterns for matching
# with \code{dat}.
# @param hasNA A string. Denotes if \code{dat} has complete data or not.
  # \code{"no"} - there are no missing values, count observed patterns
  # \code{"count.obs"} - there are missing values, count the marginally observed patterns
  # \code{"count.miss"} - there are missing values, count the full observed-and-missing patterns
# @param parallel Logical. Do you wish to parallelize the code? Defaults to \code{FALSE}
# @param cores How many cores do you wish to leave open to other processing?
#
count_levels <- function(dat, enum_list, hasNA= c("no", "count.obs", "count.miss"),
                         parallel= FALSE, 
                         cores = getOption("mc.cores", parallel::detectCores() - 1)) {
  # parameter checking
  hasNA <- match.arg(hasNA, several.ok= FALSE)
  if (parallel == TRUE) {
    if (cores < 0 || cores %% 1 != 0) stop("cores must be an integer >= 0")
  }
  if (ncol(dat) != ncol(enum_list)) stop("ncol(dat) and ncol(enum_list) must match.")

  # convert from factors to integers
  e2 <- do.call("cbind", lapply(enum_list, fact_to_int))
  dat2 <- do.call("cbind", lapply(dat, fact_to_int))

  # get counts
  if (parallel == FALSE) {
    enum_list$counts <- count_compare(x= e2, dat= dat2, hasNA= hasNA)
  } else {
    # resolve edge case when nnodes > nrow(dat2)
    # setup cluster
    nnodes <- min(nrow(dat2), cores)
    if (.Platform$OS.type != "unix") {cl <- parallel::makeCluster(nnodes, type= "PSOCK")}
    else {cl <- parallel::makeCluster(nnodes, type= "FORK")}
    parallel::clusterCall(cl, assign, "count_compare", count_compare, envir = .GlobalEnv)
    
    # run parallel count_compare() 
    temp <- do.call("cbind", parallel::clusterApply(cl,
          # split data across clusters, share: comparison (e2) and hasNA
          x= splitRows(dat2, nnodes), fun= function(x, e2, hasNA) {
            return(count_compare(x= e2, dat= x, hasNA= hasNA))
            # wrapper function needed for parameter-name-confusion b/w clusterApply
            # and count_compare
          }, e2= e2, hasNA= hasNA))
    enum_list$counts <- apply(temp, 1, sum)
    parallel::stopCluster(cl)
  }
  # return
  return(enum_list[!is.na(enum_list$counts) & enum_list$counts > 0,])
}
