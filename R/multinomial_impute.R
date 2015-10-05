
#' @title Impute Values for missing multinomial values
#' @description Impute values for multivariate multinomial data using either EM or Data
#' Augmentation.
#' @param dat A \code{data.frame}. All variables must be factors.
#' @param method \code{c("EM", "DA")} A string specifying EM or Data Augmentation (DA)
#' @param conj_prior A string specifying the conjugate prior. One of 
#' \code{c("none", "data.dep", "flat.prior", "non.informative")}.
#' @param alpha The vector of counts \eqn{\alpha} for a \eqn{Dir(\alpha)} prior. Must be specified if 
#' \code{conj_prior} is either \code{c("data.dep", "flat.prior")}. If \code{flat.prior}, specify 
#' as a scalar. If \code{data.dep}, specify as a vector with key matching \code{enum_comp}.
#' @param alpha The vector of counts \eqn{\alpha} for a \eqn{Dir(\alpha)} prior. Must be specified if 
#' \code{conj_prior} is either \code{c("data.dep", "flat.prior")}. If \code{flat.prior}, specify 
#' as a scalar. If \code{data.dep}, specify as a vector with key matching \code{enum_comp}.
#' @param verbose Logical. If \code{TRUE}, provide verbose output on each iteration.
#' #' @param tol A scalar specifying the convergence criteria. Defaults to \code{5e-7}
#' @references Schafer, Joseph L. Analysis of incomplete multivariate data. Chapter 7. 
#' CRC press, 1997. 
#' @seealso \code{\link{expand.grid}}, \code{\link{data_dep_prior_multi}}, \code{\link{multinomial_em}}
#' @export
multinomial_impute <- function(dat, method= c("EM", "DA"),
                           conj_prior= c("none", "data.dep", "flat.prior", "non.informative"),
                           alpha= NULL, verbose= FALSE, tol= 5e-7) {
  if (!all(apply(dat, 2, is.factor))) {
    # enforce factor variables
    dat <- data.frame(apply(dat, 2, function(x) as.factor(x)))
  }
  
  conj_prior <- match.arg(conj_prior, several.ok= FALSE)
  if (conj_prior %in% c("flat.prior") & is.null(alpha) ) {
    stop("Please supply argument alpha as prior.")
  }
  method <- match.arg(method, several.ok= FALSE)
  
  # 01. initialize: 
  #   enumerate observed and missing patterns
  #----------------------------------------------
  mc <- match.call()
  p <- ncol(dat)
  
  enum <- expand.grid(sapply(dat, function(x) return(c(levels(x), NA))))
  enum_comp <- enum[complete.cases(enum),] 
  enum_miss <- enum[!complete.cases(enum),]
  enum_miss <- enum_miss[apply(enum_miss, 1, function(x) !all(is.na(x))),] # not all missing
  rownames(enum_comp) <- 1:nrow(enum_comp) # y \in Y
  
  # 02. get counts / sufficient statistics
  #   parse / compute prior
  #----------------------------------------------
  dat_comp <- dat[complete.cases(dat),]
  dat_miss <- dat[!complete.cases(dat),]
  # complete data sufficient statistics
  x_y     <- count_levels(dat_comp, enum_list= enum_comp, hasNA= "no") 
  # missing data marginal sufficient statistics
  z_Os_y  <- count_levels(dat_miss, enum_list= enum_miss, hasNA= "count.miss") 
  # rownames(z_Os_y) <- 1:nrow(z_Os_y) # ID's for missingness patterns {S} 
  
  if (conj_prior == "data.dep") {
    if (!is.null(alpha)) {
      message("Using user-supplied data dependent prior.")
    } else {
      message("Calculating data dependent prior.")
      alpha <- data_dep_prior_multi(dat= dat)
    }
  } else if (conj_prior == "flat.prior") {
    if (!(is.vector(alpha) & length(alpha) == 1)) {
      stop("Flat priors must be supplied as a scalar.")
    }
    alpha <- alpha
  } else if (conj_prior == "non.informative") {
    alpha <- 1
  }
  
  # 03. EM -- get MLE for theta_y
    # NOTE:: need to implement data augmentation option
  #----------------------------------------------
  # EM
  if (method == "EM") {
    # Use defaults  for max_iter
    mle_multinomial <- multinomial_em(x_y= x_y, z_Os_y= z_Os_y, enum_comp= enum_comp, 
                                      n_obs= nrow(dat), conj_prior= conj_prior, 
                                      alpha= alpha, verbose= verbose, tol= tol) 
  }
  # Data Augmentation
  else if (method == "DA") {
    # Use defaults for max_iter, burnin, post_draws
    mle_multinomial <- multinomial_data_aug(x_y= x_y, z_Os_y= z_Os_y, enum_comp= enum_comp, 
                                            n_obs= nrow(dat), conj_prior= conj_prior, 
                                            alpha= alpha, verbose= verbose)     
  }
  
  # 04. Impute missing values 
  #----------------------------------------------
  # EM & DA
  dat_miss2 <- impute_multinomial_all(dat_miss, mle_multinomial@mle_x_y)
  
  #combine:
  imputed_data <- rbind(dat_comp, dat_miss2)
  
  # 05. return
  #----------------------------------------------
  ret <- new("imputeMulti",
             Gcall= mc, method= mle_multinomial@method,
             mle_call= mle_multinomial@mle_call,
             mle_iter= mle_multinomial@mle_iter, 
             mle_log_lik= mle_multinomial@mle_log_lik,
             mle_cp= mle_multinomial@mle_cp,
             mle_x_y= mle_multinomial@mle_x_y,
             data= list(missing_data= dat_miss, imputed_data= imputed_data),
             nmiss= nrow(dat_miss)
             )
  
  return(ret)
  
}



