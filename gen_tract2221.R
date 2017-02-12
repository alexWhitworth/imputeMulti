
# This script generates the example dataset for imputeMulti using the methods in 
# the synthACS package for population microsimulation
library(synthACS)

# pull census data
t2221 <- acs::geo.make(state= "CA", county= "Los Angeles", tract= "*")
tt_dat <- pull_synth_data(2014, 5, t2221)

tt_dat2 <- list(tt_dat[[1]],
                tt_dat[[2]],
                lapply(tt_dat$estimates, function(l) return(l[688,])),
                lapply(tt_dat$standard_error, function(l) return(l[688,])),
                tt_dat[[5]], tt_dat[[6]])
names(tt_dat2) <- names(tt_dat)
class(tt_dat2) <- "macroACS"

# do SMSM
library(parallel)
tt_dat2 <- derive_synth_datasets(tt_dat2, parallel = FALSE)

a <- all_geog_constraint_age(tt_dat2, method = "macro.table")[[1]]
g <- all_geog_constraint_gender(tt_dat2, method = "macro.table")[[1]]
m <- all_geog_constraint_marital_status(tt_dat2, method = "synthetic")[[1]]
r <- all_geog_constraint_race(tt_dat2, method = "synthetic")[[1]]
e <- all_geog_constraint_edu(tt_dat2, method = "synthetic")[[1]]

cll <- add_constraint(attr_name = "age",attr_totals = a, 
                      micro_data = tt_dat2[[1]]$synthetic_micro)
cll <- add_constraint(attr_name = "gender", attr_totals = g, 
                      micro_data = tt_dat2[[1]]$synthetic_micro, constraint_list = cll)
cll <- add_constraint(attr_name = "marital_status",attr_totals = m, 
                      micro_data= tt_dat2[[1]]$synthetic_micro, constraint_list = cll)
cll <- add_constraint(attr_name = "race", attr_totals = r, 
                      micro_data= tt_dat2[[1]]$synthetic_micro, constraint_list = cll)
cll <- add_constraint(attr_name = "edu_attain", attr_totals = e, 
                      micro_data= tt_dat2[[1]]$synthetic_micro, constraint_list = cll)

tract2221_full <- optimize_microdata(tt_dat2[[1]]$synthetic_micro, constraint_list = cll, seed= 34211L)

tract2221 <- data.frame(tract2221_full$best_fit)
rm(a,g,m,r,e, cll, t2221, tt_dat, tt_dat2)

lvls <- lapply(tract2221, levels)

# insert missing values
set.seed(176254L)
tract2221[,3:9] <- apply(tract2221[,3:9], 2, function(j) {
  na_ind <- sample.int(length(j), size= 400, replace= FALSE)
  j[na_ind] <- NA
  return(j)
})

tract2221 <- as.data.frame(mapply(function(vec, lvl) {
  if (!is.factor(vec)) {
    return(factor(vec, levels= lvl)) 
  } else {return(vec)}
}, vec= tract2221, lvl= lvls, SIMPLIFY = FALSE))

setwd("c:/Github/imputeMulti/")
devtools::use_data(tract2221, overwrite = TRUE)
