

#' @title EM algorithm for multinomial data
#' @description Implement the EM algorithm for multvariate multinomial data given
#' observed counts of complete and missing data (Y_obs and Y_mis). Allows for specification
#' of a Dirichlet conjugate prior.
#' @param x_y A \code{data.frame} of observed counts for complete observations.
#' @param z_Os_y A \code{data.frame} of observed marginal-counts for incomplete observations.
#' @param enum_comp A \code{data.frame} specifying a vector of all possible observed patterns.
#' @param conj_prior A string specifying the conjugate prior. One of 
#' \code{c("none", "data.dep", "flat.prior", "non.informative", "select")}.
#' @param alpha The vector of counts $\alpha$ for a $Dir(\alpha)$ prior. Must be specified if 
#' \code{conj_prior} is either \code{c("data.dep", "flat.prior")}. If \code{flat.prior}, specify 
#' as a scalar. If \code{data.dep}, specify as a vector with key matching \code{enum_comp}.
#' @param tol A scalar specifying the convergence criteria. Defaults to \code{1e-8}
#' @param max_iter An integer specifying the maximum number of allowable iterations. Defaults 
#' to \code{10000}.
#' @param verbose Logical. If \code{TRUE}, provide verbose output on each iteration.
#' @return A \code{list} containing the following: (1) The function call; (2) the 
#' number of iterations; (3) the MLE conjugate prior (if requested); (4) the ML estimates from EM.
#' @export
multinomial_em <- function(x_y, z_Os_y, enum_comp, n_obs,
                           conj_prior= c("none", "data.dep", "flat.prior", "non.informative", "select"), 
                           alpha= NULL, tol= 1e-8, max_iter= 10000,
                           verbose= FALSE) {
  # check some errors
  conj_prior <- match.arg(conj_prior, several.ok= FALSE)
  if (conj_prior %in% c("data.dep", "flat.prior") & is.null(alpha) ) {
    stop("Please supply argument alpha as prior.")
  }
  
  mc <- match.call()
  z_p <- ncol(z_Os_y)
  count_p <- ncol(enum_comp)
  
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
      # ... in return ... cp <- selected conj_prior
    }
    # calc theta_y from alpha
    enum_comp$theta_y <- enum_comp$alpha / sum(enum_comp$alpha)
  } else {
    enum_comp$theta_y <- runif(nrow(enum_comp))
    enum_comp$theta_y <- enum_comp$theta_y / sum(enum_comp$theta_y)
  }
  
  # 02. E and M Steps
  #----------------------------------------------
  iter <- 0
  log_lik <- 0
  while (iter < max_iter) {
    # E Step
    enum_comp$counts <- 0
    for (y in 1:nrow(enum_comp)) {
      # which missing patterns marginally-match complete pattern y?
      miss_ind <- marg_comp_compare(marg= z_Os_y[, -z_p], complete= enum_comp[y, 1:count_p],
                                  marg_to_comp= FALSE)
      if (length(miss_ind) == 0) { # if no missing, all observed
        if (any(rownames(x_y) == y)) {
          enum_comp$counts[y] <- x_y$counts[which(rownames(x_y) == y)]
          log_lik <- log_lik + x_y$counts[which(rownames(x_y) == y)] * 
            log(enum_comp$theta_y[which(rownames(x_y) == y)])
        } else {
          enum_comp$counts[y] <- 0
        }
        
      } else { # allocate observed marginal counts proportionally to complete pattern y
        # E(x_y| z_Os_y, theta) = \sum_s [E_Xsy_Zy_theta]
        # E_Xsy_Zy_theta = (z_Os_y * theta_y) / b_Os_y
        
        E_Xsy_Zy_theta <- vector(mode= "numeric", length= length(miss_ind))
        for (i in 1:length(miss_ind)) {
          comp_ind <- marg_comp_compare(z_Os_y[i, -z_p], enum_comp[, 1:count_p], 
                                        marg_to_comp= TRUE) # pattern match to complete
          b_Os_y <- sum(enum_comp$theta_y[comp_ind])
          E_Xsy_Zy_theta[i] <- z_Os_y$counts[miss_ind[i]] * enum_comp$theta_y[y] / b_Os_y # normalize
          log_lik <- log_lik + z_Os_y$counts[miss_ind[i]] * log(b_Os_y)
        }
        # expected count = observed + proportional marginally-observed
        if (any(rownames(x_y) == y)) {
          enum_comp$counts[y] <- x_y$counts[which(rownames(x_y) == y)] + sum(E_Xsy_Zy_theta)
        } else {
          enum_comp$counts[y] <- sum(E_Xsy_Zy_theta)
        }
      }
    }
  
    # M Step
    if (conj_prior == "none") {
      enum_comp$theta_y1 <- enum_comp$counts / n_obs
    } else {
      D <- nrow(enum_comp)
      alpha_0 <- sum(enum_comp$alpha)
      enum_comp$theta_y1 <- (enum_comp$counts + enum_comp$alpha - 1) / (n_obs + alpha_0 - D) 
    }
  
    # update iteration; print likelihood if verbose
    iter <- iter + 1
    if (verbose) {
      print(paste("Iteration", iter, ": log-likelihood =", round(log_lik, 6)))
    }
    
  # 03. check to exit
  #----------------------------------------------
    if (supDist(enum_comp$theta_y, enum_comp$theta_y1) < tol) {
      enum_comp$theta_y1 <- NULL
      return(list(call= mc, iter= iter, mle_cp= NULL, MLEx_y= enum_comp))
    }
    enum_comp$theta_y <- enum_comp$theta_y1
  }
  # 04. if iter >= max_iter, exit
  #----------------------------------------------
  enum_comp$theta_y1 <- NULL
  return(list(call= mc, iter= iter, mle_cp= NULL, MLEx_y= enum_comp))
}
