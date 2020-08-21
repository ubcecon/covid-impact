#' Gaussian multiplier bootstrap for a collection of felm models
#'
#'
bootstrap_felm <- function(data, models, S=999) {


  data[,"rowid"] <- 1:nrow(data)
  fmlas <- lapply(models, function(x) x$formula)

  # find cluster identifier
  cvar <- unique(sapply(fmlas, function(x) {
    trimws(strsplit(as.character(x)[3], "\\|")[[1]][4])
  }))
  cvar[is.na(cvar)] <- "rowid" # no clustering
  stopifnot(length(cvar)==1) # formulas have different clustering

  # make sure keepCX==TRUE
  models <- lapply(models, function(m) {
    if (m$keepCX) return(m)
    m <- felm(m$formula, data=data, keepCX=TRUE)
  })


  # extract index of observations in each model
  idx <- lapply(models, function(m) {
    yvar <- as.character(m$formula)[2]
    data[,"rid"] <- data$rowid
    data$rid[is.na(data[,yvar])] <- NA
    f <- update.felm.formula(m$formula, rid ~ .)
    return(felm(f, data=data)$response)
  })
  for (i in 1:length(models)) stopifnot(length(idx[[i]])==length(models[[i]]$cY))


  betahat <- lapply(models, function(m) m$coef)
  betastar <- list()

  # draw multipliers
  cvals <- unique(data[,cvar])
  gc <- matrix(rnorm(S*length(cvals)), nrow=S)
  colnames(gc) <- cvals
  gi <- gc[,data[,cvar]]

  # bootstrap
  for (i in 1:length(models)) {
    X <- models[[i]]$cX[,!is.na(models[[i]]$coef)]
    score <- matrix(0, nrow=nrow(data), ncol=ncol(X))
    iH <- solve(t(X) %*% X)
    score[idx[[i]],] <-X * as.vector(models[[i]]$residuals)
    betastar[[i]] <- matrix(NA, nrow=length(betahat[[i]]), ncol=S)
    betastar[[i]][!is.na(betahat[[i]]),] <-
      as.vector(betahat[[i]][!is.na(betahat[[i]])]) +
      iH %*% t(score) %*% t(gi)
    rownames(betastar[[i]]) <- rownames(betahat[[i]])
  }

  return(betastar)
}


bootstrap_felm2 <- function(data, models, S=999) {
  
  
  data[,"rowid"] <- 1:nrow(data)
  fmlas <- lapply(models, function(x) x$formula)
  
  # find cluster identifier
  cvar <- unique(sapply(fmlas, function(x) {
    trimws(strsplit(as.character(x)[3], "\\|")[[1]][4])
  }))
  cvar[is.na(cvar)] <- "rowid" # no clustering
  stopifnot(length(cvar)==1) # formulas have different clustering
  
  # # make sure keepCX==TRUE
  # models <- lapply(models, function(m) {
  #   if (m$keepCX) return(m)
  #   m <- felm(m$formula, data=data, keepCX=TRUE)
  # })
  for (i in 1:length(models)){
    sdf <- data
    if (i <= 4){
      sdf$pmaskbus <- 0*sdf$pmaskbus
    } else if (i==5) {
      sdf$pindex <- 0*sdf$pindex
    }
    m <- models[[i]]
    if (m$keepCX) return(m)
    models[[i]] <- felm(m$formula, data=sdf, keepCX=TRUE)
  }
  
  
  # extract index of observations in each model
  idx <- lapply(models, function(m) {
    yvar <- as.character(m$formula)[2]
    data[,"rid"] <- data$rowid
    data$rid[is.na(data[,yvar])] <- NA
    f <- update.felm.formula(m$formula, rid ~ .)
    return(felm(f, data=data)$response)
  })
  for (i in 1:length(models)) stopifnot(length(idx[[i]])==length(models[[i]]$cY))
  
  
  betahat <- lapply(models, function(m) m$coef)
  betastar <- list()
  
  # draw multipliers
  cvals <- unique(data[,cvar])
  gc <- matrix(rnorm(S*length(cvals)), nrow=S)
  colnames(gc) <- cvals
  gi <- gc[,data[,cvar]]
  
  # bootstrap
  for (i in 1:length(models)) {
    X <- models[[i]]$cX[,!is.na(models[[i]]$coef)]
    score <- matrix(0, nrow=nrow(data), ncol=ncol(X))
    iH <- solve(t(X) %*% X)
    score[idx[[i]],] <-X * as.vector(models[[i]]$residuals)
    betastar[[i]] <- matrix(NA, nrow=length(betahat[[i]]), ncol=S)
    betastar[[i]][!is.na(betahat[[i]]),] <-
      as.vector(betahat[[i]][!is.na(betahat[[i]])]) +
      iH %*% t(score) %*% t(gi)
    rownames(betastar[[i]]) <- rownames(betahat[[i]])
  }
  
  return(betastar)
}

 