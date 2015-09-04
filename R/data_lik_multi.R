

### functions needed:
# F1. extract levels {d_j} of all variables (Y_j \in Y), j= 1,..., p --- redundant, use levels() ; done
# F2. Create named vector of length D = prod_{j=1}^p d_j corresponding to F1 -- use rownames(expand.grid()); done
# F3. count all instances of P(Y = y); return counts {x_y} to vector from F2; done
# F4. ID and enumerate all missingness patterns {s: s= 1,..., S}; done
# F5. For each variable (Y_j), define r_sj -- if Y has missingness pattern s \in [1,S] or not
# F6. Enumerate O_s(y) = {y_j : r_sj = 1} --> O_s and M_s(y) = {y_j : r_sj = 0} --> M_s
# F7. For each missingness pattern, count observed Y=y {x_y^(s)} --> z_O(s)y ; done




#' @param dat A \code{data.frame}. All variables must be factors
#' @references Schafer, Joseph L. Analysis of incomplete multivariate data. Chapter 7. 
#' CRC press, 1997. 
#' @seealso \code{\link{expand.grid}}, \code{\link{data_dep_prior_multi}}
#' @export
data_lik_multi <- function(dat) {
  if (!all(apply(dat, 2, is.factor))) {
    # enforce factor variables
    dat <- data.frame(apply(dat, 2, function(x) as.factor(x)))
  }
  
  # 01. initialize: 
  #   enumerate observed and missing patterns
  #----------------------------------------------
  p <- ncol(dat)
  
  enum <- expand.grid(sapply(dat, function(x) return(c(levels(x), NA))))
  enum_comp <- enum[complete.cases(enum),] 
  enum_miss <- enum[!complete.cases(enum),]
  rownames(enum_comp) <- 1:nrow(enum_comp) # y \in Y
  
  # 02. get counts / sufficient statistics
  #   calculate data-dependent prior
  #   Define O_s, M_s 
  #----------------------------------------------
  dat_comp <- dat[complete.cases(dat),]
  dat_miss <- dat[!complete.cases(dat),]
  # complete data sufficient statistics
  x_y     <- count_levels(dat_comp, enum_list= enum_comp, hasNA= "no") 
  # missing data marginal sufficient statistics
  z_Os_y  <- count_levels(dat_miss, enum_list= enum_miss, hasNA= "count.obs") 
  # rownames(z_Os_y) <- 1:nrow(z_Os_y) # ID's for missingness patterns {S} 
  
  alpha <- data_dep_prior_multi(dat= dat)
  
  # Define O_s, M_s 
    ### O_s(y) is the set of missingness patterns for which y is observed  (y \in [1, p])
    ### M_s(y) is the set of missingness patterns for which y is missing
    ### O_s = {O_s(1), ..., O_s(p)}
    ### M_s = {M_s(1), ..., M_s(p)}
  O_s <- apply(z_Os_y, 1, function(x) which(!is.na(x)))
  M_s <- apply(z_Os_y, 1, function(x) which(is.na(x)))
  
  
  # 03. E and M Steps
  #----------------------------------------------
  
  
  
}







