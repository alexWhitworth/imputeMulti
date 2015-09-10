

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
           } else if (obj@iter < 0) {
             return("A negative iteration was given.")
           }
           return(TRUE)
         }
)


#' Class "imputeMulti" of imputed multinomial data
#'  
#' @name imputeMulti-class
#' @description A multivariate multinomial model imputed by EM or Data Augmentation is 
#' represented as a \code{\linkS4class{mod_imputeMulti}} object. A complete 
#' dataset and model is represented as an \code{\linkS4class{imputeMulti}} object.
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
setGeneric("print",
           def= function(obj) {
             standardGeneric("print")
           })


setMethod("print", signature= "mod_imputeMulti",
  function(obj) {
    cat("\n Call: \n", paste(deparse(obj@mle_call)),
        "\n Method: \n", obj@method,
        "\n\n Iterations: ", obj@mle_iter,
        "\n\n Log-Likelihood: ", obj@mle_log_lik)
  })


setMethod("print", signature= "imputeMulti",
  function(obj) {
    cat("\n Global Call: \n", paste(deparse(obj@Gcall)),
        "\n Call: \n", paste(deparse(obj@mle_call)),
        "\n Method: \n", obj@method,
        "\n\n Iterations: ", obj@mle_iter,
        "\n\n Log-Likelihood: ", obj@mle_log_lik,
        "\n Number Missing: \n", obj@nmiss)
  })

## Summary
setGeneric("summary", 
           def= function(obj) {
             standardGeneric("summary")
           })

setMethod("summary", signature= "mod_imputeMulti",
  function(obj) {
    print(obj)
    
    summary(obj@mle_x_y[, c("alpha", "theta_y")])
  })

setMethod("summary", signature= "imputeMulti",
  function(obj) {
    print(obj)
    
    summary(obj@mle_x_y[, c("alpha", "theta_y")])
    
    lapply(obj@data, dim)
  })