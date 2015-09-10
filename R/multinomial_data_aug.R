

#' @title Data Augmentation algorithm for multinomial data
#' @description Implement the Data Augmentation algorithm for multvariate multinomial data given
#' observed counts of complete and missing data (\eqn{Y_obs} and \eqn{Y_mis}). Allows for specification
#' of a Dirichlet conjugate prior. 
#' @param x_y A \code{data.frame} of observed counts for complete observations.
#' @param z_Os_y A \code{data.frame} of observed marginal-counts for incomplete observations.
#' @param enum_comp A \code{data.frame} specifying a vector of all possible observed patterns.
#' @param conj_prior A string specifying the conjugate prior. One of 
#' \code{c("none", "data.dep", "flat.prior", "non.informative")}.
#' @param alpha The vector of counts \eqn{\alpha} for a \eqn{Dir(\alpha)} prior. Must be specified if 
#' \code{conj_prior} is either \code{c("data.dep", "flat.prior")}. If \code{flat.prior}, specify 
#' as a scalar. If \code{data.dep}, specify as a vector with key matching \code{enum_comp}.
#' @param tol A scalar specifying the convergence criteria. Defaults to \code{1e-8}
#' @param burnin A scalar specifying the number of iterations to use as a burnin. Defaults 
#' to \code{500}.
#' @param post_draws An integer specifying the number of draws from the posterior distribution.
#'  Defaults to \code{1000}.
#' @param max_iter An integer specifying the maximum number of allowable iterations. Defaults 
#' to \code{10000}.
#' @param verbose Logical. If \code{TRUE}, provide verbose output on each iteration.
#' @return An object of class \code{mod_imputeMulti}.
#' @seealso \code{\link{multinomial_em}}, \code{\link{multinomial_impute}}
#' @export
multinomial_data_aug <- function(x_y, z_Os_y, enum_comp, n_obs,
                                 conj_prior= c("none", "data.dep", "flat.prior", "non.informative"), 
                                 alpha= NULL, tol= 1e-8, burnin= 500, post_draws= 1000, max_iter= 10000,
                                 verbose= FALSE) {
  require(gtools)
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
    } 
    # calc theta_y from alpha
    enum_comp$theta_y <- enum_comp$alpha / sum(enum_comp$alpha)
  } else {
    enum_comp$theta_y <- runif(nrow(enum_comp))
    enum_comp$theta_y <- enum_comp$theta_y / sum(enum_comp$theta_y)
  }
  
  # 02. I and P Steps
  #----------------------------------------------
  iter <- 0
  while (iter < max_iter) {
    # I Step
    log_lik <- log_lik0 <- 0
    enum_comp$counts <- 0
    for (y in 1:nrow(enum_comp)) {
      # which missing patterns marginally-match complete pattern y?
      miss_ind <- marg_comp_compare(marg= z_Os_y[, -z_p], complete= enum_comp[y, 1:count_p],
                                    marg_to_comp= FALSE)
      if (length(miss_ind) == 0) { # if no missing, all observed
        if (any(rownames(x_y) == y)) {
          enum_comp$counts[y] <- x_y$counts[which(rownames(x_y) == y)]
          if (enum_comp$theta_y[which(rownames(x_y) == y)] > 0) { # make sure log-lik not -Inf
            log_lik <- log_lik + x_y$counts[which(rownames(x_y) == y)] * 
              log(enum_comp$theta_y[which(rownames(x_y) == y)])
          }
        } else { # random / structural 0
          enum_comp$counts[y] <- 0
        }
        
      } else { # random allocation of observed marginal counts to complete pattern y
        # (x_y| z_Os_y, theta) = \sum_s (Xsy|Zsy, gamma)
        # (Xsy|Zy_theta) ~ M(Zsy, gamma)
        
        E_Xsy_Zy_theta <- vector(mode= "numeric", length= length(miss_ind))
        if (length(miss_ind) > 0) {
          for (i in 1:length(miss_ind)) {
            comp_ind <- marg_comp_compare(z_Os_y[miss_ind[i], -z_p], enum_comp[, 1:count_p], 
                                          marg_to_comp= TRUE) # pattern match to complete
            b_Os_y <- sum(enum_comp$theta_y[comp_ind])
            E_Xsy_Zy_theta[i] <- rmultinom(1, size= z_Os_y$counts[miss_ind[i]], 
                                           prob= enum_comp$theta_y[comp_ind] / b_Os_y) # normalized probability
          }
        }
        # expected count = observed + random draw from multinomial based on marginally-observed
        if (any(rownames(x_y) == y)) {
          enum_comp$counts[y] <- x_y$counts[which(rownames(x_y) == y)] + sum(E_Xsy_Zy_theta)
          if (b_Os_y > 0) {
            log_lik <- log_lik + sum(z_Os_y$counts[miss_ind] * log(b_Os_y))
          }
        } else {
          enum_comp$counts[y] <- sum(E_Xsy_Zy_theta)
          if (b_Os_y > 0) {
            log_lik <- log_lik + sum(z_Os_y$counts[miss_ind] * log(b_Os_y))
          }
        }
      }
    }
    
    # P Step
    if (conj_prior == "none") {
      # in case of random zeros: use non-informative prior
      enum_comp$theta_y1 <- rdirichlet(n=1, alpha= enum_comp$counts + 1) 
    } else {
      enum_comp$theta_y1 <- rdirichlet(n=1, alpha= enum_comp$counts + enum_comp$alpha)
    }
    
    # update iteration; print likelihood if verbose
    iter <- iter + 1
    if (verbose) {
      cat("Iteration", iter, ": log-likelihood =", sprintf("%.10f", log_lik), "\n Convergence Criteria =",
          sprintf("%.10f", supDist(enum_comp$theta_y, enum_comp$theta_y1)), "... \n")
    }
    
    # 03. check convergence to exit and return
    # MLE for theta_y is taken to be the mean of n= post_draws draws from the 
    # posterior distribution
    #----------------------------------------------
    if (iter > burnin & (supDist(enum_comp$theta_y, enum_comp$theta_y1) < tol |
        abs(log_lik - log_lik0) < tol * 100)) {
      # update log-lik for prior
      if (conj_prior != "none") {
        log_lik <- log_lik + sum(ifelse(enum_comp$alpha == 0 | enum_comp$theta_y == 0, 0,
                                        enum_comp$alpha * log(enum_comp$theta_y)))
      }
      enum_comp$theta_y1 <- NULL
      enum_comp$counts <- NULL
      
      if (conj_prior == "none") {
        # in case of random zeros: use non-informative prior
        theta_post <- rdirichlet(n= post_draws, alpha= enum_comp$counts + 1) 
        enum_comp$theta_y <- colMeans(theta_post)
      } else {
        theta_post <- rdirichlet(n= post_draws, alpha= enum_comp$counts + enum_comp$alpha)
        enum_comp$theta_y <- colMeans(theta_post)
      }
      
      mod <- new("mod_imputeMulti",
                 method= "EM",
                 mle_call= mc,
                 mle_iter= iter,
                 mle_log_lik= log_lik,
                 mle_cp= NULL,
                 mle_x_y= enum_comp)
      
      return(mod)
    } else {
      enum_comp$theta_y <- enum_comp$theta_y1
      log_lik0 <- log_lik
    }
  }
  
  # 04. if iter >= max_iter, exit
  # MLE for theta_y is taken to be the mean of n= post_draws draws from the 
  # posterior distribution
  #----------------------------------------------
  # update log-lik for prior
  if (conj_prior != "none") {
    log_lik <- log_lik + sum(ifelse(enum_comp$alpha == 0 | enum_comp$theta_y == 0, 0,
                                    enum_comp$alpha * log(enum_comp$theta_y)))
  }
  enum_comp$theta_y1 <- NULL
  enum_comp$counts <- NULL
  
  if (conj_prior == "none") {
    # in case of random zeros: use non-informative prior
    theta_post <- rdirichlet(n= post_draws, alpha= enum_comp$counts + 1) 
    enum_comp$theta_y <- colMeans(theta_post)
  } else {
    theta_post <- rdirichlet(n= post_draws, alpha= enum_comp$counts + enum_comp$alpha)
    enum_comp$theta_y <- colMeans(theta_post)
  }
  
  mod <- new("mod_imputeMulti",
             method= "DA",
             mle_call= mc,
             mle_iter= iter,
             mle_log_lik= log_lik,
             mle_cp= NULL,
             mle_x_y= enum_comp)
  
  return(mod)
}