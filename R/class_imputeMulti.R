
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
         validity= function(obj) {
           if (!obj@method %in% c("EM", "DA", NULL)) {
             return("Currently only EM and DA methods are defined.")
           } else if (obj@mle_iter < 0) {
             return("A negative iteration was given.")
           }
           return(TRUE)
         }
)


###########################################################
## Methods
###########################################################
## Print
print.mod_imputeMulti <- function(obj) {
  cat("\n Call: \n", paste(deparse(obj@mle_call)),
      "\n Method: ", obj@method,
      "\n\n Iterations: ", obj@mle_iter,
      "\n\n Log-Likelihood: ", obj@mle_log_lik)
}

setGeneric("print",
           def= function(obj) {
             standardGeneric("print.mod_imputeMulti")
           })


setMethod("print", signature= "mod_imputeMulti",
          function(obj) {
            print.mod_imputeMulti(obj)
          })


## Summary
summary.mod_imputeMulti <- function(obj) {
  print(obj)
  
  summary(obj@mle_x_y[, c("alpha", "theta_y")])
}


setGeneric("summary", 
           def= function(obj) {
             standardGeneric("summary")
           })

setMethod("summary", signature= "mod_imputeMulti",
          function(obj) {
            summary.mod_imputeMulti(obj)
          })


#' Class "imputeMulti" 
#'  
#' @name imputeMulti-class
#' @description A multivariate multinomial model imputed by EM or Data Augmentation is 
#' represented as a \code{\linkS4class{mod_imputeMulti}} object. A complete 
#' dataset and model is represented as an \code{\linkS4class{imputeMulti}} object.
#' Inherits from \code{mod_imputeMulti}. Additional slots are supplied for (1) the
#' call to \code{multinomial_impute}; (2) the complete, missing, and imputed data;
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
print.imputeMulti <- function(obj) {
  cat("\n Global Call: \n", paste(deparse(obj@Gcall)),
      "\n Call: \n", paste(deparse(obj@mle_call)),
      "\n Method: ", obj@method,
      "\n\n Iterations: ", obj@mle_iter,
      "\n\n Log-Likelihood: ", obj@mle_log_lik,
      "\n Number Missing: ", obj@nmiss)
}

setGeneric("print",
           def= function(obj) {
             standardGeneric("print.imputeMulti")
           })


setMethod("print", signature= "imputeMulti",
  function(obj) {
    print.imputeMulti(obj)
  })

## Summary
summary.imputeMulti <- function(obj) {
  print(obj)
  
  summary(obj@mle_x_y[, c("alpha", "theta_y")])
  
  lapply(obj@data, dim)
}

setGeneric("summary", 
           def= function(obj) {
             standardGeneric("summary")
           })

setMethod("summary", signature= "imputeMulti",
  function(obj) {
    summary.imputeMulti(obj)
  })