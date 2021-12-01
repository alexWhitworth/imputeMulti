#----------------------------------------------------------
# create test data
set.seed(12315)
x1 <- sample(1:5, size=100, replace= TRUE)
x2 <- sample(6:10, size=100, replace= TRUE)
x3 <- sample(11:15, size=100, replace= TRUE)
x4 <- sample(16:20, size=100, replace= TRUE)
x5 <- sample(21:26, size=100, replace= TRUE)

dat <- c(x1, x2, x3, x4, x5)
# insert missing values
mis.ind <- sample(1:length(dat), size= 75, replace= FALSE)
dat[mis.ind] <- NA
rm(x1,x2,x3,x4,x5, mis.ind)
dim(dat)<- c(100, 5)
dat <- data.table::data.table(dat)

# dat, and enumerated possible values
dat <- dat[, lapply(.SD, as.factor)]
enum_wo_miss <- expand.grid(sapply(dat, levels))
enum_w_miss <- expand.grid(sapply(dat, function(x) return(c(levels(x), NA))))
idx <- which(apply(enum_w_miss, 1, function(i) sum(is.na(i))) == ncol(enum_w_miss))
enum_w_miss <- enum_w_miss[-idx,]

# sufficient statistics
enum_comp <- enum_w_miss[complete.cases(enum_w_miss),] 
enum_miss <- enum_w_miss[!complete.cases(enum_w_miss),]
rownames(enum_comp) <- 1:nrow(enum_comp) # y \in Y
dat_comp <- dat[complete.cases(dat),]
dat_miss <- dat[!complete.cases(dat),]

# complete data sufficient statistics
x_y     <- count_levels(dat_comp, enum_list= enum_comp, hasNA= "no") 
# missing data marginal sufficient statistics
z_Os_y  <- count_levels(dat_miss, enum_list= enum_miss, hasNA= "count.miss") 

# saveRDS(dat, file= "./tests/testdata/data-dat.rds")
# saveRDS(enum_w_miss, file= "./tests/testdata/data-enum_w_miss.rds")
# saveRDS(enum_wo_miss, file= "./tests/testdata/data-enum_wo_miss.rds")
# saveRDS(x_y, file= "./tests/testdata/data-x_y.rds")
# saveRDS(z_Os_y, file= "./tests/testdata/data-z_Os_y.rds")
#----------------------------------------------------------