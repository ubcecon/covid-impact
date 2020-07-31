# 7/27/2020 Hiro Kasahara
# Modification of the code written by Shuowen Chen ``debiasedfe_shuowen.R''
# 7/3/2020 Shuowen Chen
# The script contains code that implement debiased fixed effects and panel bootstrap
# standard errors for dataset in Chernozhukov, Kasahara and Schrimpf (2020)
# Serves as application for Chen, Chernozhukov and Fernandez-Val (2020)
# Acknowledgements: functions extend those written by Paul Schrimpf.

library(lfe)
library(speedglm)
library(boot)
# load data (sdf)
# load("~/Desktop/Research/Panel-Cross-over-jackknife/application/covidsince0307.RData")

# add weight to sdf
sdf$sweight <- 1
# create week factor to the data
sdf$week <- as.factor(strftime(sdf$date, format = "%V"))

#### Functions ####
# Create formula
# The function extends the createfmla to accommodate FE specifications
# yvar: dependent variable for bpiy and piy
# xvars: regressors.
#  (a) For bpiy, regressors are behavior, policy and information
#  (b) For pib, regressors are policy and information, dependent variable is 1
#      of the 4 behavior variables
#  (c) For piy, regressors are policy and information variables
# iv: This is to accommodate the syntax of felm. "0" means no iv.
# cluster: cluster at state level (treatment).
# Note: interacted fixed effect hasn't been tested.
createfmla_fe <- function(yvar, xvars, iv = "0", cluster = "state",
                          fe = c("state + month", "state * month",
                                 "week + state", "date + state")) {
  rhs <- paste(xvars, collapse = " + ")
  if (fe == "state * month") {
    # we will use speedlm, which is faster
    rhs <- paste(rhs, "+", fe, collapse = " ")
  } else {
    rhs <- paste(rhs," | ", fe, " | ", iv, " | ", paste(cluster, collapse = " + "))
  }
  return(list(as.formula(paste(yvar, "~", rhs, sep = " ")), rhs))
}

# a function to conduct multiple split (along the cross section) bias correction
# to be called in the policyreg function
# s: number of splits
# n: number of cross section units
# dat: data in use
# uc: uncorrected estimators
# fm: regression formula
multiple_split <- function(s, n, unit, time, dat, uc, fm) {
  across <- 0 * uc
  for (k in 1:s) {
    sample1 <- sample(n, ceiling(n/2), replace = FALSE)
    subsample1 <- (unit %in% sample1 & time <= median(time)) |
      (!(unit %in% sample1) & time >= median(time))
    subsample2 <- (unit %in% sample1 & time >= median(time)) |
      (!(unit %in% sample1) & time <= median(time))
    cross1 <- felm(fm, data = dat[subsample1, ],
                   weights = dat[subsample1, ]$sweight)
    cross2 <- felm(fm, data = dat[subsample2, ],
                   weights = dat[subsample2, ]$sweight)
    across <- across + ((coef(cross1) + coef(cross2))/2)/s
  }
  # average cross over corrected
  acbc <- 2 * uc - across
  return(acbc)
}

##### DFE estimation based on cross-over jackknife
# Note: The output of this function facilitates calculation of bootstrap se
reg_fe <- function(df, # data
                   yvar, # dependent variable (1 of 4 bevahiors if pib)
                   pols, # policies
                   bvars, # behavior (NULL if not behavior)
                   x, # controls
                   iv, # iv for felm, if not needed just input "0"
                   l = 14,
                   fixedeffect) {
  if (l == 0) {
    # This is for pib
    p <- pols
    b <- bvars
  } else {
    # This is for pbiy and piy
    p <- sprintf("lag(%s, %d)", pols, l)
    b <- sprintf("lag(%s, %d)", bvars, l)
  }
  # create regression formula
  xvars <- c(p, b, x)
  fmla <- createfmla_fe(yvar, xvars, fe = fixedeffect, iv = iv)
  # two cross-over subsamples
  stopifnot(is.factor(df$state)) # we need as.double(df$state) to return values in 1:n
  unit <- as.double(df$state)
  # Split by day. Data is linear so no bias coming from the time effects,
  # no cross section dependence as well.
  time <- as.double(df$date)
  subsample1 <- (unit <= (median(unit)) & time <= median(time)) |
    (unit >= median(unit) & time >= median(time))
  subsample2 <- (unit <= median(unit) & time >= median(time)) |
    (unit >= median(unit) & time <= median(time))
  # Uncorrected and corrected estimator
  whole <- felm(fmla[[1]], data = df, weights = df$sweight)
  cross1 <- felm(fmla[[1]], data = df[subsample1, ],
                 weights = df[subsample1, ]$sweight)
  cross2 <- felm(fmla[[1]], data = df[subsample2, ],
                 weights = df[subsample2, ]$sweight)
  s1 <- df[subsample1,]
  s2 <- df[subsample2,]
  s1$state <- as.character(s1$state)
  s2$state <- paste(as.character(s2$state),"2",sep="")
  sdf <- rbind(s1, s2)
  sdf$state <- as.factor(sdf$state)
  cross3 <- felm(fmla[[1]], data = sdf,
                 weights = sdf$sweight)
  # Debiased estimates
  coef_cbc <- 2*coef(whole) - 0.5*(coef(cross1) + coef(cross2))
  coef_cbc2 <- 2*coef(whole) - coef(cross3)
  coef_nobc <- coef(whole)
  # Multiple split
  coef_acbc <- multiple_split(s = 5, length(levels(df$state)), unit,
                              time, df, coef(whole), fmla[[1]])
  # Sum of policy ciefficients (uncorrected and corrected)
  peff <- c(sum(coef_nobc[p]), sum(coef_cbc[p]))
  if (!is.null(bvars)) {
    # weight is from April 1st to 10th
    w <- colMeans(subset(df, df$date >= as.Date("2020-04-01") &
                           df$date <= as.Date("2020-04-10"))[, bvars])
    # weighted sum of behavior coefficients (both uncorrected and corrected)
    beff <- c(sum(coef_nobc[b]*w), sum(coef_cbc[b]*w))
  } else {
    # for piy and pib no such metric
    beff <- NULL
  }
  # outputs: noncorrected, corrected, sum of policy and behavioral coefficients
  return(list(reg = whole, nobc = coef_nobc, bc = coef_cbc, bc2 = coef_cbc2, peff=peff, beff=beff, acbc = coef_acbc))
}



# # The following function extends the function mainregressions
# # NOTE: there is one change in the code
# # the original code put xlist in the controls, but xlist is specified as an empty list
# # in the program, so I'm not sure what it stands for. I think it's meant for creating a list
# # so as to loop over choices of specifications. I deleted it in the argument. I checked the
# # results with and without xlist in the control for fe = "0" in the original code
# # and they are identical.
#
# mainregressions_fe <- function(df, # data
#                                yvar, # dependent variable (Y)
#                                pols, # policy variables
#                                bvars, # behavior variables (B)
#                                infovars, # information structures
#                                tvars, # key confounder from SIR
#                                ivlist = "0",
#                                L = 14,
#                                fixed) {
#   # This is considering both pols and interacted of pmask with months
#   plist <- list(pols, c("pmask.april","pmask.may", pols[-1]))
#   # for pols only
#   # plist <- list(pols)
#
#   ijs <- expand.grid(1:length(plist), 1:length(infovars))
#
#   # The function loops over plist, infovars to run different specifications
#   pbiy <- apply(ijs, 1, function(ij) {
#     reg_fe(df, yvar, plist[[ij[1]]], bvars,
#            c(sprintf("lag(%s, %d)", infovars[[ij[2]]], L), tvars),
#            ivlist, l = L, fixedeffect = fixed)
#   })
#
#   piy <- apply(ijs, 1, function(ij) {
#     reg_fe(df, yvar, plist[[ij[1]]], NULL,
#            c(sprintf("lag(%s, %d)", infovars[[ij[2]]], L), tvars),
#            ivlist, l = L, fixedeffect = fixed)
#   })
#
#   # for pib, note the four behavioral variables each can be a
#   # dependent variable, so loop over bvars, plist and infovars
#   ijs <- expand.grid(1:length(bvars), 1:length(plist))
#   pib <- list()
#   if (!is.null(infovars)) {
#     infonum <- length(infovars)
#   } else {
#     infonum <- 1
#   }
#   for (k in 1:infonum) {
#     pib[[k]] <- apply(ijs, 1, function(ij) {
#       reg_fe(df, bvars[ij[1]], plist[[ij[2]]], NULL,
#              c(infovars[[k]]), ivlist, l = 0, fixedeffect = fixed)
#     })
#   }
#   # return output as three lists
#   return(list(pib = pib, pbiy = pbiy, piy = piy))
# }
#
# ## Auxiliary function: produce data for nonparametric panel bootstrap
# # Note: if data gets updated please change the date in the data_b$date command
# data_rg <- function(data, mle) {
#   n <- length(unique(data$state))
#   t <- length(unique(data$date))
#   # swap state index
#   ids <- kronecker(sample.int(n, n, replace = TRUE), rep(1, t))
#   data_b <- data[(ids - 1)*t + rep(c(1:t), n), ]
#   data_b$state <- kronecker(c(1:n), rep(1, T))
#   data_b$date <- rep(seq(as.Date("2020-03-07"), as.Date("2020-06-03"), by = "day"), n)
#   data_b <- data.frame(data_b)
#   #data_b <- pdata.frame(data_b, index = c("state", "date"))
#   return(data_b)
# }
#
# # Function for weighted bootstrap
# # More robust to close to singular designs because it resamples all the states
# data_wb <- function(data, mle) {
#   # number of states
#   n <- length(unique(data$state))
#   t <- length(unique(data$date))
#   # Exponential weights
#   multipliers <- rexp(n)
#   # For each state, weight is the same for all dates
#   weight <- rep(multipliers/sum(multipliers), each = t)
#   # Add to the data and treat it as sampling weight
#   data$sweight <- weight
#   return(data)
# }
#
#                                       #### Empirical Analysis ####
# #### 1. Defining lhs and rhs variables
# # dependent variable for pbiy and piy
# yvar <- "dlogdc" # cases
# yvar2 <- "dlogdd" # deaths
# # policy variables
# pols <- c("pmaskbus", "pk12", "pshelter", "pmovie", "prestaurant", "pnonessential")
# # behavioral variables
# bvars <- c("workplaces", "retail", "grocery", "transit")
# # confounder from the SIR
# tvars <- "dlogtests"
# # information structure I and III
# # Note: to run only one structure, say I, just specify
# # infovars <- list(c("dlogdc", "logdc"))
# infovars <- list(c("dlogdc", "logdc"),
#                  c("dlogdc", "logdc", "dlogdc.national", "logdc.national")) # cases
#
# infovarsd <- list(c("dlogdd", "logdd"),
#                   c("dlogdd", "logdd", "dlogdd.national", "logdd.national")) # deaths
#
# # Regression
# case2 <- mainregressions_fe(sdf, yvar, pols, bvars, infovars, tvars, L = 14, fixed = "state + month")
# death_case <- mainregressions_fe(sdf, yvar2, pols, bvars, infovars, NULL, L = 21, fixed = "state + month")
# death_death <- mainregressions_fe(sdf, yvar2, pols, bvars, infovarsd, NULL, L = 21, fixed = "state + month")
#
# # Week and state fixed effects
# case_week <- mainregressions_fe(sdf, yvar, pols, bvars, infovars, tvars, L = 14, fixed = "state + week")
# death_case_week <- mainregressions_fe(sdf, yvar2, pols, bvars, infovars, NULL, L = 21, fixed = "state + week")
# death_death_week <- mainregressions_fe(sdf, yvar2, pols, bvars, infovarsd, NULL, L = 21, fixed = "state + week")
#
# # Consider date and state fixed effect. Drop information structure as there is no policy variation
# case_date <- mainregressions_fe(sdf, yvar, pols, bvars, NULL, tvars, L = 14, fixed = "state + date")
# death_date <- mainregressions_fe(sdf, yvar2, pols, bvars, NULL, NULL, L = 21, fixed = "state + date")
#
#
# #### 2. Bootstrap Standard Errors
# # Compute estimates in each bootstrap
# bootstat_fe <- function(data, form) {
#   # change the specification here
#   tot <- mainregressions_fe(data, yvar, pols, bvars, infovars, tvars, fixed = "state + month")
#   return(unlist(tot))
# }
#
# # Call boot command to conduct bootstrap
# set.seed(88) # seed for replication
# num_boot <- 500 # number of bootstraps
# ncores <- 1 # number of cpus (speed)
#
# # nonpar will throw warning wrt multiple splitting bias correction
# result_boot <- boot(data = sdf, statistic = bootstat_fe, sim = "parametric", ran.gen = data_rg,
#                     mle = 0, parallel = "multicore", ncpus = ncores, R = 5)
#
# result_wb <- boot(data = sdf, statistic = bootstat_fe, sim = "parametric", ran.gen = data_wb,
#                   mle = 0, parallel = "multicore", ncpus = ncores, R = 5)
#
# # Compute standard errors
# result <- structure(vapply(result_boot$t, as.double, numeric(1)), dim = dim(result_boot$t))
# tot_bse <- apply(result, 2, function(x) {
#   # Normal scaled IQR
#   return((quantile(x, .75, na.rm = TRUE) - quantile(x, .25, na.rm = TRUE))/(qnorm(.75) - qnorm(.25)))
# })
#
# ref <- rbind(result_boot$t0, tot_bse)
#
# # compute p values for each variable and put stars
# starnum <- function(oldmat, starp = c(0.1, 0.05, 0.01)) {
#   pval <- matrix(0, nrow = 1, ncol = dim(oldmat)[2])
#   for (i in 1:dim(oldmat)[2]) {
#     pval[1,i] <- sum(pnorm(-abs(oldmat[1,i]), sd = oldmat[2,i]) < starp/2)
#   }
#   newmat <- rbind(oldmat, pval)
#   return(newmat)
# }
#
# ### A preliminary storage of results
# stars <- starnum(ref)
# ref <- rbind(ref, stars)
#
# #### Produce tables for bootstrap (Incomplete)
# # Depending on what regression type, produce a correspondent table
# # info: don't be a list
# # lag: number of lags for covariates
# tableproduction <- function(ref, type = c("pib", "piy", "pbiy"),
#                             pols, bvars, tvars, info, lag) {
#   if (type == "pib") {
#     # four behaviors as dependent, each has nobc, bc, abc
#     # num rows: 3 * num of pols and info (estimate, bse and stars)
#     tab <- matrix(0, ncol = 12, nrow = 3*(length(pols) + length(info)))
#     colnames(tab) <- rep(c("nobc","bc","avgbc"), 4)
#     rownames(tab) <- rep(c("Est", "CSE", "Stars"), (length(pols) + length(info)))
#
#   } else if (type == "piy") {
#
#   } else {
#
#   }
#   return(tab)
# }
