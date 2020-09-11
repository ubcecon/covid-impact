library(ggplot2)
library(ggthemes)
#' Create lags for panel data.
#'
#' This function creates lags (or leads) of panel data variables.
#' Input data should be sorted by i, t --- e.g.
#' df <- df[order(i,t),]
#' @param x Vector or matrix to get lags of.
#' @param i unit index
#' @param t time index
#' @param lag How many time periods to lag. Can be negative if leading
#' values are desired.
#' @return Lagged copy of x.
panellag <- function(x, i, t, lag=1) {
  if (!identical(order(i,t),1:length(i))) {
    stop("inputs not sorted.")
  }
  if (is.matrix(x)) {
    return(apply(x,MARGIN=2,FUN=function(c) { panel.lag(c,i,t,lag) }))
  }
  if (length(i) != length(x) || length(i) != length(t) ) {
    stop("Inputs not same length")
  }
  if (lag>0) {
    x.lag <- x[1:(length(x)-lag)]
    x.lag[i[1:(length(i)-lag)]!=i[(1+lag):length(i)] ] <- NA
    x.lag[t[1:(length(i)-lag)]+lag!=t[(1+lag):length(i)] ] <- NA
    val <- (c(rep(NA,lag),x.lag))
  } else if (lag<0) {
    lag <- abs(lag)
    x.lag <- x[(1+lag):length(x)]
    x.lag[i[1:(length(i)-lag)]!=i[(1+lag):length(i)] ] <- NA
    x.lag[t[1:(length(i)-lag)]+lag!=t[(1+lag):length(i)] ] <- NA
    val <- (c(x.lag,rep(NA,lag)))
  } else { # lag=0
    return (x)
  }
  if (inherits(x,"Date") & class(val)=="numeric") {
    stopifnot(0==as.numeric(as.Date("1970-01-01")))
    val <- as.Date(val, origin="1970-01-01")
  }
  return(val)
}

#' Create differences of panel data.
#'
#' @param x Vector or matrix to get differences of.
#' @param id unit index
#' @param t time index
#' @param lag How many time periods to lag. Can be negative if leading
#' values are desired.
#' @return Differenced x.
paneldiff <- function(x, id, t, lag=1) {
  return(x - panellag(x,id,t,lag=lag))
}


#' Create moving average from panel data
#'
#' @param x vector to compute moving average of `x` should be sorted by `order(id,t)`
#' @param id unit index
#' @param t time index
#' @param len
#' @return Average of x[t:(t-len)]
panelma <- function(x, id, t, len=1) {
  stopifnot(len>=0)
  ma  <- rep(0, length(x))#as.numeric(x)
  for(i in 0:len) {
    ma  <- ma + panellag(x, id,t, lag=i)
  }
  return(ma/(len+1))
}


smoothedpolicy <- function(df, pdatevar, enddatevar=NA, type="pnorm",
  bw=7, lag=0, id="state") {
  if (type=="pnorm") {
    p <- as.numeric(pnorm(df$date, df[,pdatevar], bw))
    p[is.na(p)] <- 0.0
    if (!is.na(enddatevar)) stop("smoothedpolicy with type='pnorm' and enddatevar not NA is not implemented.")
  } else if(type=="ma") {
    d <- df$date >= df[,pdatevar]
    d[is.na(d)] <- FALSE
    if (!is.na(enddatevar)) {
      over <- df$date>df[,enddatevar]
      over[is.na(over)] <- FALSE
      d[over] <- FALSE
    }
    p <- panelma(d, df[,id], df$date, len=bw)
  }
  if (lag != 0) p  <- panellag(p, df[,id], df$date, lag=lag)
  return(p)
}

update.felm.formula <- function(old, new, ...) {
  fc <- as.character(old)
  if (!is.null(grep("\\|",fc[3]))) {
    rhsparts <- strsplit(fc[3], "\\|")[[1]]
    oldf <- as.formula(paste(fc[2], fc[1], rhsparts[1], sep=""))
    newf <- update.formula(oldf, new, ...)
    nfc <- as.character(newf)
    as.formula(paste(nfc[2], nfc[1], paste(c(nfc[3], rhsparts[-1]), collapse="|")))
  } else {
    update.formula(old, new, ...)
  }
}

figtheme <- theme_pander() + theme(plot.title=element_text(face="plain"))
