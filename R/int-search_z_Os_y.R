
# @title Pattern matching marginally missing to complete
# @description Performs pattern matching from marginally missing (z_Os_y) to potential
# complete observations (x_possible). Uses RSQLite instead of C++ selection search (marg_comp_compare)
# @param z_Os_y the marginally missing observations
# @param x_possible the set of possible fully observed observations
# @return A list of length \code{nrow(z_Os_y)}. Each element of the list is the set of row-indices 
# in x_possible which marginally match the associated row of \code{z_Os_y}
search_z_Os_y <- function(z_Os_y, x_possible) {
  if (is.null(names(x_possible)) | is.null(names(z_Os_y))) 
    stop("both arguments / parameters must have names.")
  if (any(is.na(names(x_possible))) | any(is.na(names(z_Os_y)))) 
    stop("names may not be NA.")
  
  data.table::setDT(z_Os_y)
  data.table::setDT(x_possible)
  
  search_out <- vector("list", length= nrow(z_Os_y))
  join_cols <- names(x_possible)[1:ncol(x_possible)]
  x_possible[, rowid := .I]
  
  for (s in 1:nrow(z_Os_y)) {
    tmp <- z_Os_y[s,]
    na_idx <- which(apply(tmp, 1, is.na))
    search_out[[s]] <- x_possible[tmp, on= join_cols[-na_idx]]$rowid
  }
  
  x_possible[, rowid := NULL]
  return(search_out)
}
