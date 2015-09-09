

### functions needed:
# F1. extract levels {d_j} of all variables (Y_j \in Y), j= 1,..., p --- redundant, use levels() ; done
# F2. Create named vector of length D = prod_{j=1}^p d_j corresponding to F1 -- use rownames(expand.grid()); done
# F3. count all instances of P(Y = y); return counts {x_y} to vector from F2; done
# F4. ID and enumerate all missingness patterns {s: s= 1,..., S}; done
# F5. For each variable (Y_j), define r_sj -- if Y has missingness pattern s \in [1,S] or not
# F6. Enumerate O_s(y) = {y_j : r_sj = 1} --> O_s and M_s(y) = {y_j : r_sj = 0} --> M_s
# F7. For each missingness pattern, count observed Y=y {x_y^(s)} --> z_O(s)y ; done



#' @title Compute MLE estimates for missing multinomial
#' @description blah blah blah -- update
#' @param dat A \code{data.frame}. All variables must be factors
#' @param conj_prior A string specifying the conjugate prior. One of 
#' \code{c("none", "data.dep", "flat.prior", "non.informative", "select")}.
#' @param alpha The vector of counts \eqn{\alpha} for a \eqn{Dir(\alpha)} prior. Must be specified if 
#' \code{conj_prior} is either \code{c("data.dep", "flat.prior")}. If \code{flat.prior}, specify 
#' as a scalar. If \code{data.dep}, specify as a vector with key matching \code{enum_comp}.
#' @param alpha The vector of counts \eqn{\alpha} for a \eqn{Dir(\alpha)} prior. Must be specified if 
#' \code{conj_prior} is either \code{c("data.dep", "flat.prior")}. If \code{flat.prior}, specify 
#' as a scalar. If \code{data.dep}, specify as a vector with key matching \code{enum_comp}.
#' @param verbose Logical. If \code{TRUE}, provide verbose output on each iteration.
#' @references Schafer, Joseph L. Analysis of incomplete multivariate data. Chapter 7. 
#' CRC press, 1997. 
#' @seealso \code{\link{expand.grid}}, \code{\link{data_dep_prior_multi}}, \code{\link{multinomial_em}}
#' @export
data_lik_multi <- function(dat,
                           conj_prior= c("none", "data.dep", "flat.prior", "non.informative", "select"),
                           alpha= NULL, verbose= FALSE) {
  if (!all(apply(dat, 2, is.factor))) {
    # enforce factor variables
    dat <- data.frame(apply(dat, 2, function(x) as.factor(x)))
  }
  
  conj_prior <- match.arg(conj_prior, several.ok= FALSE)
  if (conj_prior %in% c("data.dep", "flat.prior") & is.null(alpha) ) {
    stop("Please supply argument alpha as prior.")
  }
  
  
  
  # 01. initialize: 
  #   enumerate observed and missing patterns
  #----------------------------------------------
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
  z_Os_y  <- count_levels(dat_miss, enum_list= enum_miss, hasNA= "count.obs") 
  # rownames(z_Os_y) <- 1:nrow(z_Os_y) # ID's for missingness patterns {S} 
  
  if (conj_prior == "data.dep") {
    alpha <- data_dep_prior_multi(dat= dat)
  } else if (conj_prior == "flat.prior") {
    if (!(is.vector(alpha) & length(alpha) == 1)) {
      stop("Flat priors must be supplied as a scalar.")
    }
    alpha <- alpha
  } else if (conj_prior == "non.informative") {
    enum_comp$alpha <- 1
  } else if (conj_prior == "select") {
    stop("Functionality not implemented yet.")
  }
  
  
  # 03. E and M Steps
  #----------------------------------------------
  # defaults  for tol and max_iter
  mle_multinomial <- multinomial_em(x_y= x_y, z_Os_y= z_Os_y, n_obs= nrow(dat),
                                    conj_prior= conj_prior, alpha= alpha, verbose= verbose) 
  
  
}




