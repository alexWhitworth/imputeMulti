
# @title Pattern matching marginally missing to complete
# @description Performs pattern matching from marginally missing (z_Os_y) to potential
# complete observations (x_possible). Uses RSQLite instead of C++ selection search (marg_comp_compare)
# @param z_Os_y the marginally missing observations
# @param x_possible the set of possible fully observed observations
search_z_Os_y <- function(z_Os_y, x_possible) {
  ## setup output list and SQLite database
  search_out <- vector("list", length= nrow(z_Os_y))
  
  x_p <- RSQLite::dbConnect(RSQLite::SQLite(), ":memory:")
  x_possible2 <- x_possible
  x_possible2$rownames <- 1:nrow(x_possible2)
  RSQLite::dbWriteTable(x_p, "x_possible", x_possible2)
  RSQLite::dbGetQuery(x_p, paste0("create index idx on x_possible (", paste(names(x_possible),collapse=", "), ")"))
  
  ## run
  for (s in 1:nrow(z_Os_y)) {
    search_out[[s]] <- as.integer(RSQLite::dbGetQuery(x_p, create_query(z_Os_y, z_Os_y[s, -ncol(z_Os_y)], 
                                                              var_names= names(x_possible)))$rownames)
  }
  return(search_out)
}

# helper fucntion for creating queries in search_z_Os_y
create_query <- function(df, row, var_names) {
  idx <- which(!is.na(row))
  n_na <- length(idx)
  q <- paste0("select rownames from x_possible where ", var_names[ idx[1] ], "= '", 
              get_level_text(get(var_names[ idx[1] ], as.environment(df)) , as.integer(row[ idx[1] ])),"'")
  for (i in 2:n_na) {
    q <- paste0(q, "and ", var_names[idx[i]], "= '", 
                get_level_text(get(var_names[ idx[i] ], as.environment(df)) , as.integer(row[ idx[i] ])), "'")
  }
  return(q)
}

# function to get the character mapping from a factor type
get_level_text <- function(var, val) {
  lvls <- levels(var)
  return(lvls[val])
}



# nnodes <- min(nrow(dat2), parallel::detectCores() - leave_cores)
# if (grepl("Windows", utils::sessionInfo()$running)) {cl <- parallel::makeCluster(nnodes, type= "PSOCK")}
# else {cl <- parallel::makeCluster(nnodes, type= "FORK")}
# 
# comp_ind <- parallel::clusterApply(cl, x= splitRows(z_Os_y, nnodes), fun= search_z_Os_y,
#                                      x_possible= x_possible)
# 
# parallel::stopCluster(cl)
