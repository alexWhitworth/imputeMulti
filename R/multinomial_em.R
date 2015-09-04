


multinomial_em <- function(x_y, z_Os_y, O_s, M_s, enum_comp, n_obs,
                           conj_prior= c("none", "data.dep", "flat.prior", "non.informative", "select"), 
                           alpha= NULL) {
  # check some errors
  conj_prior <- match.arg(conj_prior, several.ok= FALSE)
  if (conj_prior %in% c("data.dep", "flat.prior") & is.null(alpha) ) {
    stop("Please supply argument alpha as prior.")
  }
  
  count_col <- ncol(z_Os_y)
  
  # 01. Merge in prior if supplied; calculate if requested
  #----------------------------------------------
  if (conj_prior != "none") {
    if (conj_prior == "data.dep") {
      if (nrow(alpha) != nrow(enum_comp)) {
        stop("nrow(alpha) must match nrow(enum_comp).")
      }
      enum_comp <- merge(enum_comp, prior)
    } else if (conj_prior == "flat.prior") {
      if (!(is.vector(alpha) & length(alpha) == 1)) {
        stop("Flat priors must be supplied as a scalar.")
      }
      enum_comp$alpha <- alpha
    } else if (conj_prior == "non.informative") {
      enum_comp$alpha <- 1
    } else if (conj_prior == "select") {
      stop("Functionality not implemented yet.")
    }
    # calc theta_y from alpha
    enum_comp$theta_y <- enum_comp$alpha / sum(enum_comp$alpha)
  } else {
    enum_comp$theta_y <- runif(nrow(enum_com))
    enum_comp$theta_y <- enum_comp$theta_y / sum(enum_comp$theta_y)
  }
  
  
  
  # 02. E and M Steps
  #----------------------------------------------
  repeat{
  # E Step
  enum_comp$counts <- 0
  for (y in 1:nrow(enum_comp)) {
    # complete data
    if (y %in% rownames(x_y)) enum_comp$counts[y] <- x_y$counts[rownames(x_y) == y]
    else {
      for (s in 1:nrow(z_Os_y)) {
        if (length(O_s[[s]]) > 0) {
          if (z_Os_y$counts[s] > 0) {
            if (length(M_s[[s]]) == 0) {
              cnts[] <- cnts[s] + z_Os_y$counts[s] # not quite right
            } else {
              n <- 0 # need to update this part
              sub_rows <- as.vector(marg_array_comp(z_Os_y[s, -count_col], enum_comp))
              n <- n + sum(enum_comp$theta_y[sub_rows])
              cnts
            }
          }
        }
      }
    }
  }
  
  # M Step
  if (conj_prior == "none") {
    enum_comp$theta_y1 <- enum_comp$counts / n_obs
  } else {
    alpha_0 <- sum(enum_comp$alpha)
    enum_comp$theta_y1 <- (enum_comp$counts + enum_comp$alpha - 1) / (n_obs + alpha_0 - D) 
    # double check what D should be; also currently undefined in code
  }
  
  # 03. check to exit
  #----------------------------------------------
  
  
  }
  
}
