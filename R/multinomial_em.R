


multinomial_em <- function(x_y, O_s, M_s, enum_comp,
                           conj_prior= c("none", "data.dep", "flat.prior", "non.informative", "select"), 
                           alpha= NULL) {
  # check some errors
  conj_prior <- match.arg(conj_prior, several.ok= FALSE)
  if (conj_prior %in% c("data.dep", "flat.prior") & is.null(alpha) ) {
    stop("Please supply argument alpha as prior.")
  }
  
  
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
  enum_comp$theta <- 0
  for (s in 1:nrow(z_Os_y)) {
    if (length(O_s[[s]]) > 0) {
      if (z_Os_y$counts[s] > 0) {
        if (length(M_s[[s]]) == 0) {
          enum_comp$theta[s] <- enum_comp$theta[s] + z_Os_y$counts[s]
        } else {
          n <- 0
          
        }
      }
    }
  }
  
  
}
