#' @title Data Dependent Prior for Multinomial Distribution
#' @description Creates a data depedent prior for p-dimensional multinomial distributions
#' using a conjugate prior (eg \eqn{Dirichlet(\alpha)}) based on 20% of the data.
#' @param dat A \code{data.frame}. All variables must be factors
#' @references Darnieder, William Francis. Bayesian methods for data-dependent priors. 
#' Dissertation. The Ohio State University, 2011. 
#' @return A \code{data.frame} containing identifiers for all possible \eqn{P(Y=y)} and 
#' the associated prior-counts, \eqn{\alpha}
#' @seealso \code{\link{expand.grid}}
#' @export
data_dep_prior_multi <- function(dat) {
  if (!all(apply(dat, 2, is.factor))) {
    # enforce factor variables
    dat <- data.frame(apply(dat, 2, function(x) as.factor(x)))
  }
  
  enum <- expand.grid(sapply(dat, levels))
  
  comp <- which(complete.cases(dat))
  comp_frac <- length(comp) / nrow(dat) 
  
  if (comp_frac < .2) {
    prior <- count_levels(dat, enum_list= enum, hasNA= "no") 
  } else {
    n <- round(.2 * length(comp) / comp_frac, 0)
    samp <- sample(comp, size= n)
    prior <- count_levels(dat[samp,], enum_list= enum, hasNA= "no")
  }
  
  names(prior)[ncol(prior)] <- "alpha" # naming convention of dirichlet prior
  return(prior)
}
