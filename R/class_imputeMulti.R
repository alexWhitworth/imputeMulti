
#' Class "mod_imputeMulti"
#'  
#' @name mod_imputeMulti-class
#' @description A multivariate multinomial model imputed by EM or Data Augmentation is 
#' represented as a \code{\linkS4class{mod_imputeMulti}} object. A complete 
#' dataset and model is represented as an \code{\linkS4class{imputeMulti}} object.
#' Slots for \code{mod_imputeMulti} objects include: (1) the modeling method; 
#' (2) the call to the estimation function; (3) the number of iterations in estimation;
#' (4) the final log-likelihood; (5) the conjugate prior if any; (6) the MLE estimate of
#' the sufficient statistics and parameters.
#' @docType class
#' @section Objects from the class: Objects are created by calls to
#' \code{\link{multinomial_impute}}, \code{\link{multinomial_em}}, or
#' \code{\link{multinomial_data_aug}}.
#' @seealso \code{\link{multinomial_impute}}, \code{\link{multinomial_em}}, 
#' \code{\link{multinomial_data_aug}}
#' @export
setClass("mod_imputeMulti",
         representation= list(
           method= "character",
           mle_call= "call",
           mle_iter= "numeric",
           mle_log_lik= "numeric",
           mle_cp= "character",
           mle_x_y= "data.frame"),
         validity= function(object) {
           if (!object@method %in% c("EM", "DA", "NULL")) {
             return("Currently only EM and DA methods are defined.")
           } else if (object@mle_iter < 0) {
             return("A negative iteration was given.")
           }
           return(TRUE)
         }
)
  

###########################################################
## Methods
###########################################################
## Print
print.mod_imputeMulti <- function(object) {
  cat("\n Call: \n", paste(deparse(object@mle_call)),
      "\n Method: ", object@method,
      "\n\n Iterations: ", object@mle_iter,
      "\n\n Log-Likelihood: ", object@mle_log_lik)
}

setMethod("show", signature= "mod_imputeMulti",
          print.mod_imputeMulti)


## Summary
setGeneric("summary")

summary.mod_imputeMulti <- function(object) {
  print(object)
  
  summary.data.frame(object@mle_x_y[, c("alpha", "theta_y")])
}

setMethod("summary", signature="mod_imputeMulti", def=summary.mod_imputeMulti)


#' Class "imputeMulti" 
#'  
#' @name imputeMulti-class
#' @description A multivariate multinomial model imputed by EM or Data Augmentation is 
#' represented as a \code{\linkS4class{mod_imputeMulti}} object. A complete 
#' dataset and model is represented as an \code{\linkS4class{imputeMulti}} object.
#' Inherits from \code{mod_imputeMulti}. Additional slots are supplied for (1) the
#' call to \code{multinomial_impute}; (2) the missing and imputed data;
#' and (3) the number of observations with missing values.
#' @docType class
#' @section Objects from the class: Objects are created by calls to
#' \code{\link{multinomial_impute}}, \code{\link{multinomial_em}}, or
#' \code{\link{multinomial_data_aug}}.
#' @seealso \code{\link{multinomial_impute}}, \code{\link{multinomial_em}}, 
#' \code{\link{multinomial_data_aug}}
#' @export
setClass("imputeMulti",
         representation= list(Gcall= "call",
                              method= "character",
                              mle_call= "call",
                              mle_iter= "numeric",
                              mle_log_lik= "numeric",
                              mle_cp= "character",
                              mle_x_y= "data.frame",
                              data= "list",
                              nmiss= "numeric"),
         contains= "mod_imputeMulti")



###########################################################
## Methods
###########################################################
## Print
print.imputeMulti <- function(object) {
  cat("\n Global Call: \n", paste(deparse(object@Gcall)),
      "\n Call: \n", paste(deparse(object@mle_call)),
      "\n Method: ", object@method,
      "\n\n Iterations: ", object@mle_iter,
      "\n\n Log-Likelihood: ", object@mle_log_lik,
      "\n Number Missing: ", object@nmiss)
}

setMethod("show", signature= "imputeMulti",
  print.imputeMulti)

## Summary
setGeneric("summary")

summary.imputeMulti <- function(object) {
  print(object)
  
  summary(object@mle_x_y[, c("alpha", "theta_y")])
  
  lapply(object@data, dim)
}

setMethod("summary", signature= "imputeMulti",
  summary.imputeMulti)