#### internal
# #' @title Count Levels
# #' @description Given a dataset and a data.frame of comparison patterns, 
# #' count the number of occurances of each pattern.
# #' @param dat A \code{data.frame}. All variables must be factors
# #' @param enum_list A \code{data.frame} consisting of all possible patterns for matching
# #' with \code{dat}.
# #' @param hasNA A string. Denotes if \code{dat} has complete data or not. If missing
# #' then should only observed patterns be counted or should missing patterns be counted?
count_levels <- function(dat, enum_list, hasNA= c("no", "count,obs", "count.miss")) {
  hasNA <- match.arg(hasNA, several.ok= FALSE)
  
  enum_list$counts <- NA
  # get counts
  if(hasNA == "no") {
    for (i in 1:nrow(enum_list)) {
      enum_list$counts[i] <- sum(apply(dat, 1, function(x, case) all(x == case), 
                                       case= enum_list[i, -ncol(enum_list)]))
    }
  } else if (hasNA == "count.obs") {
    dat <- dat[!complete.cases(dat),]
    for (i in 1:nrow(enum_list)) {
      enum_list$counts[i] <- sum(apply(dat, 1, function(x, case) all(x == case, na.rm=TRUE), 
                                       case= enum_list[i, -ncol(enum_list)]))
    }
  } else if (hasNA == "count.miss") {
    dat <- dat[!complete.cases(dat),]; options(warn= -1) # length warnings
    for (i in 1:nrow(enum_list)) {
      enum_list$counts[i] <- sum(apply(dat, 1, function(x, case) {
        which(is.na(x)) == which(is.na(case)) && all(x == case, na.rm= TRUE)
      }, case= enum_list[i, -ncol(enum_list)]))
    }
  }; options(warn= 0)
  return(enum_list[enum_list$counts > 0,])
}
