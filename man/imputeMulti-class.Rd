% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_imputeMulti.R, R/methods_imputeMulti.R
\docType{class}
\name{imputeMulti-class}
\alias{imputeMulti-class}
\alias{show-imputeMulti}
\alias{show,imputeMulti-method}
\alias{get_imputations}
\alias{get_imputations,imputeMulti-method}
\alias{n_miss}
\title{Class "imputeMulti"}
\usage{
\S4method{show}{imputeMulti}(object)

get_imputations(object)

\S4method{get_imputations}{imputeMulti}(object)

n_miss(object)
}
\arguments{
\item{object}{an object of class "imputeMulti"}
}
\description{
A multivariate multinomial model imputed by EM or Data Augmentation is 
represented as a \code{\linkS4class{mod_imputeMulti}} object. A complete 
dataset and model is represented as an \code{\linkS4class{imputeMulti}} object.
Inherits from \code{mod_imputeMulti}. Additional slots are supplied for (1) the
call to \code{multinomial_impute}; (2) the missing and imputed data;
and (3) the number of observations with missing values.
}
\section{Slots}{

\describe{
\item{\code{Gcall}}{the call to \code{multinomial_impute}}

\item{\code{method}}{the modeling method}

\item{\code{mle_call}}{the call to the estimation function}

\item{\code{mle_iter}}{the number of iterations in estimation}

\item{\code{mle_log_lik}}{the final log-likelihood}

\item{\code{mle_cp}}{the conjugate prior if any}

\item{\code{mle_x_y}}{the MLE estimate of the sufficient statistics and parameters}

\item{\code{data}}{a \code{list} of the missing and imputed data}

\item{\code{nmiss}}{the number of observations with missing data}
}}

\section{Objects from the class}{
 Objects are created by calls to
\code{\link{multinomial_impute}}, \code{\link{multinomial_em}}, or
\code{\link{multinomial_data_aug}}.
}

\seealso{
\code{\link{multinomial_impute}}, \code{\link{multinomial_em}}, 
\code{\link{multinomial_data_aug}}
}
