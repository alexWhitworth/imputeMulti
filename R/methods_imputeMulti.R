
###########################################################
## Methods - mod_imputeMulti
###########################################################
## Print

#' @name show-mod_imputeMulti
#' @aliases show,mod_imputeMulti-method
#' @docType methods
#' @rdname mod_imputeMulti-methods
#' @include class_imputeMulti.R
#' @title Print mod_imputeMulti class objects
#' @description print method for class "mod_imputeMulti"
#' @param object an object of class "mod_imputeMulti"
setMethod("show", signature= "mod_imputeMulti",
          def= function(object) {
            cat("\n Call: \n", paste(deparse(object@mle_call), sep= "\n"),
                "\n Method: ", object@method,
                "\n\n Iterations: ", object@mle_iter,
                "\n\n Log-Likelihood: ", object@mle_log_lik)
          })


## Summary
setGeneric("summary")
# @export
summary.mod_imputeMulti <- function(object, ...) {
  methods::show(object)
  cat("\n\n")
  if (object@mle_cp != "none") {
    summary(object@mle_x_y[, c("alpha", "theta_y")])  
  } 
}


#' @title Summarizing mod_imputMulti objects
#' @description summary method for class "mod_imputeMulti"
#' @param object an object of class "mod_imputeMulti"
#' @param ... further arguments passed to or from other methods.
#' @exportMethod summary
setMethod("summary", signature="mod_imputeMulti", def=summary.mod_imputeMulti)

#' @name get_parameters
#' @aliases get_parameters
#' @title Get parameter estimates 
#' @description Extract parameter estimates from a 'mod_imputeMulti' object 
# @param object an object of class "mod_imputeMulti"
#' @rdname mod_imputeMulti-methods
#' @exportMethod get_parameters
#' @export
setGeneric("get_parameters", 
           function(object) standardGeneric("get_parameters"))

#' @rdname mod_imputeMulti-methods
setMethod("get_parameters", signature= "mod_imputeMulti",
          function(object) {
            return(object@mle_x_y) 
          })

#' @name get_prior
#' @aliases get_prior
#' @title Get prior from a 'mod_imputeMulti' object 
#' @description Extract the character string specifying the prior from a 'mod_imputeMulti' object 
# @param object an object of class "mod_imputeMulti"
#' @rdname mod_imputeMulti-methods
#' @exportMethod get_prior
#' @export
setGeneric("get_prior", 
           function(object) standardGeneric("get_prior"))

#' @rdname mod_imputeMulti-methods
setMethod("get_prior", signature= "mod_imputeMulti",
          function(object) {
            return(object@mle_cp) 
          })

#' @name get_iterations
#' @aliases get_iterations
#' @title Get number of iterations used in fitting
#' @description Extract the number of iterations used in fitting a 'mod_imputeMulti' object 
# @param object an object of class "mod_imputeMulti"
#' @rdname mod_imputeMulti-methods
#' @exportMethod get_iterations
#' @export
setGeneric("get_iterations", 
           function(object) standardGeneric("get_iterations"))

#' @rdname mod_imputeMulti-methods
setMethod("get_iterations", signature= "mod_imputeMulti",
          function(object) {
            return(object@mle_iter) 
          })


#' @name get_logLik
#' @aliases get_logLik
#' @title Get final log likelihood
#' @description Extract the final log likelihood from fitting a 'mod_imputeMulti' object 
# @param object an object of class "mod_imputeMulti"
#' @rdname mod_imputeMulti-methods
#' @exportMethod get_logLik
#' @export
setGeneric("get_logLik", 
           function(object) standardGeneric("get_logLik"))

#' @rdname mod_imputeMulti-methods
setMethod("get_logLik", signature= "mod_imputeMulti",
          function(object) {
            return(object@mle_log_lik) 
          })

#' @name get_method
#' @aliases get_method
#' @title Get method used in fitting
#' @description Extract the method used in fitting a 'mod_imputeMulti' object. Returns either 
#' EM (expectation-maximization) or DA (data-augmentation).
# @param object an object of class "mod_imputeMulti"
#' @rdname mod_imputeMulti-methods
#' @exportMethod get_method
#' @export
setGeneric("get_method", 
           function(object) standardGeneric("get_method"))

#' @rdname mod_imputeMulti-methods
setMethod("get_method", signature= "mod_imputeMulti",
          function(object) {
            return(object@method) 
          })


###########################################################
## Methods - mod_imputeMulti
###########################################################
## Print

#' @name show-imputeMulti
#' @aliases show,imputeMulti-method
#' @docType methods
#' @rdname imputeMulti-methods
#' @include class_imputeMulti.R
#' @title Print imputeMulti class objects
#' @description print method for class "imputeMulti"
#' @param object an object of class "imputeMulti"
setMethod("show", signature= "imputeMulti",
          def= function(object) {
            cat("\n Global Call: \n", paste(deparse(object@Gcall), sep= "\n"),
                "\n Call: \n", paste(deparse(object@mle_call)),
                "\n Method: ", object@method,
                "\n\n Iterations: ", object@mle_iter,
                "\n\n Log-Likelihood: ", object@mle_log_lik,
                "\n Number Missing: ", object@nmiss)
          })

## Summary
setGeneric("summary")
# @export
summary.imputeMulti <- function(object, ...) {
  methods::show(object)
  if (object@mle_cp != "none") {
    summary(object@mle_x_y[, c("alpha", "theta_y")])  
  } 
}

#' @title Summarizing imputMulti objects
#' @description summary method for class "imputeMulti"
#' @param object an object of class "imputeMulti"
#' @param ... further arguments passed to or from other methods.
#' @exportMethod summary
setMethod("summary", signature= "imputeMulti",
          summary.imputeMulti)


#' @name get_imputations
#' @aliases get_imputations
#' @title Get observation level imputed values 
#' @description Extract observation level imputed values from imputMulti objects
# @param object an object of class "imputeMulti"
#' @rdname imputeMulti-methods
#' @exportMethod get_imputations
#' @export
setGeneric("get_imputations", 
           function(object) standardGeneric("get_imputations"))

#' @rdname imputeMulti-methods
setMethod("get_imputations", signature= "imputeMulti",
          function(object) {
            return(object@data$imputed_data) 
          })

#' @name n_miss
#' @aliases n_miss
#' @title Get number of missing observations prior to imputing
#' @description Extract the number of missing observations prior to imputation from a 'imputeMulti' 
#' object.
# @param object an object of class "mod_imputeMulti"
#' @rdname imputeMulti-methods
#' @exportMethod n_miss
#' @export
setGeneric("n_miss", 
           function(object) standardGeneric("n_miss"))

#' @rdname mod_imputeMulti-methods
setMethod("n_miss", signature= "imputeMulti",
          function(object) {
            return(object@nmiss) 
          })