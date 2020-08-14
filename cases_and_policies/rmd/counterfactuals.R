###################################################################################
library(lfe)
library(stargazer)
library(knitr)
library(plm)
library(latex2exp)
library(ggplot2)
library(ggthemes)
library(estimatr)
library(gridExtra)
library(grid)
library(mvtnorm)
library(kableExtra)
library(data.table)
library(reshape2)

sgtype <- opts_knit$get("rmarkdown.pandoc.to")
sgstyle <- 'default'
colors <-  scale_color_solarized
colors_fill <- scale_fill_solarized
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
source(paste(rootdir,"cases_and_policies/R/dataprep.R",sep="/"))
source(paste(rootdir,"cases_and_policies/rmd/varlabels.R", sep="/"))
source(paste(rootdir,"cases_and_policies/rmd/regprep.R", sep="/"))
source(paste(rootdir,"cases_and_policies/rmd/generatetables.R", sep="/"))
source(paste(rootdir,"cases_and_policies/rmd/bootstrap_felm.R", sep="/"))

datelims <- c(as.Date("2020-03-07"), max(df$date))
################################################################################

# ```{r cffuncs, cache=FALSE}

#' Given an estimated felm model with lags and differences, returns a
#' function that simulates the model.
#'
#' @param m estimated felm model
#' @param df Data
#' @param dvar Name of variable repressenting differences in the
#' model. dvar should be a difference of length difflag.
#' @param var Name of varaible representing levels in the model.
#' @param difflag difference length for dvar
#' @param lhsdiff if true the outcome is also dvar with the same
#' difflag, if not the outcome is var
#'
#' @return A function with arguments idvar==value of identifier for
#'   which to simulate and shockgen=function(n) that generates values
#'   for residuals. The default value of shockgen is to resample model
#'   residuals with replacement. The function returns a simulated path
#'   var beginning from the initial values for id==idvar and using the
#'   exogenous covariates of id==idvar.
#'
#' Example:
#' df <- df[order(df$id,$df$t),]
#' df$dy <- paneldiff(df$y, df$id, df$t, lag=2)
#' fmla <- dy ~ lag(y,3) + lag(dy,1) + x
#' # hack to get get id
#'
#' sim <- createforwardsim(fmla, df, "dy", "y", 2, TRUE)
#' # simulate with errors set to 0's
#' yhat <- sim(df$id[1], function(n) fill(0, n))
#' #' # simulate sampling residuals with replacement
#' yhat <- sim(df$id[1])
createforwardsim <- function(m, df, id="state",
                       dvar="dlogdc", var="logdc",
                       difflag=7, lhsdiff=TRUE) {
  m$coef[is.nan(m$coef)] <- 0

  # get index of observations used in mdl
  df$idxcfs <- 1:nrow(df)
  mid <- felm(update.felm.formula(m$formula, idxcfs ~ .), data=df, keepX=TRUE)
  idx <- mid$response

  # This  regexp will match dvar if preceded by start of string or (
  # and followed by end of string or ,
  di <- grepl(sprintf("(^|\\()%s(,|$)",dvar), rownames(m$coef))
  li <- grepl(sprintf("(^|\\()%s(,|$)",var), rownames(m$coef))
  dc <- m$coef[di]
  names(dc) <- rownames(m$coef)[di]
  lc <- m$coef[li]
  names(lc) <- rownames(m$coef)[li]
  # exogenous part of model
  if (!is.null(getfe(m))) {
    stop("createforwardsim not implemented with fixed effects. Include factors in the formula instead")
  }

  xt <- mid$X %*% (m$coef * as.numeric(!(di | li)))

  ## convert into AR model
  llags <- as.numeric(gsub("lag\\(.+, (\\d+)\\)","\\1", names(lc)))
  dlags <- as.numeric(gsub("lag\\(.+, (\\d+)\\)","\\1", names(dc)))
  maxlags <- max(c(llags, dlags+difflag, ifelse(lhsdiff, difflag, 0)))
  ar <- rep(0, maxlags)
  if (lhsdiff) ar[difflag]  <- ar[difflag] + 1
  ar[llags]  <- ar[llags] + lc
  ar[dlags] <- ar[dlags] +dc
  ar[dlags+difflag] <- ar[dlags+difflag]-dc

  if (!is(df, "pdata.frame")) {
    warning("df is not a pdata.frame. This function assumes df is sorted by id, time, and within each id, time is consecutive. Results will be incorrect if these assumptions are false.")
  } else {
    stopifnot(all(is.pconsecutive(df)))
  }

  sim <- function(idvar, shockgen=NULL, #shockgen=function(n=1) { sample(m$residuals, size=n,
                         #                                replace=TRUE)},
                  samplecoefs=FALSE, nsim=1) {
    ids <- df[idx,id]
    if (samplecoefs) {
      V <- vcov(m)
      nac <- is.na(m$coefficients)
      e <- rep(0,nrow(V))
      Vnona <- V[!nac,!nac]
      e[!nac] <- t(rmvnorm(1, mean=rep(0, nrow(Vnona)), sigma=Vnona))

      dc <- m$coef[di] + e[di]
      lc <- m$coef[li] + e[li]
      ## convert into AR model
      ar <- rep(0, maxlags)
      if (lhsdiff) ar[difflag]  <- ar[difflag] + 1
      ar[llags]  <- ar[llags] + lc
      ar[dlags] <- ar[dlags] +dc
      ar[dlags+difflag] <- ar[dlags+difflag]-dc

      x <- xt[ids==idvar] + (mid$X[ids==idvar,] %*% (e * as.numeric(!(di | li))))
    } else {
      x <- xt[ids==idvar]
    }

    date <- df$date[idx[df[idx,id]==idvar]]
    names(date) <- NULL
    it0 <- which(df$date==date[1] & df[,id]==idvar)
    Y0 <- as.vector(sapply(1:(maxlags), function(k) lag(df[,var], k)[it0]))
    dY0 <- as.vector(sapply(1:(maxlags), function(k) lag(df[,dvar], k)[it0]))
    lagy0 <- Y0-dY0
    check <- lagy0[1:(length(Y0)-difflag)]- Y0[(difflag+1):length(Y0)]
    stopifnot(all(abs(check)<1e-8, na.rm=TRUE))
    na0 <- is.na(Y0[(difflag+1):length(Y0)])
    Y0[(difflag+1):length(Y0)][na0] <- lagy0[1:(length(Y0)-difflag)][na0]

    if (is.null(shockgen)) {
      stopifnot(nsim==1)
      e <- matrix(m$residuals[ids==idvar],nrow=nsim)
    } else {
      e <- matrix(shockgen(length(x)*nsim),nrow=nsim)
    }
    Y0 <- t(matrix(rep(Y0,nsim),ncol=nsim))
    Y <- Y0
    yt <- matrix(0,nrow=nsim,ncol=length(x))
    for (t in 1:length(x)) {
      yt[,t] <- x[t] + Y %*% ar + e[,t]
      #Y <- c(yt[t], Y[-length(Y)])
      Y[,2:ncol(Y)] <- Y[,1:(ncol(Y)-1)]
      Y[,1] <- yt[,t]
    }
    return(list(y=cbind(matrix(Y0[, ncol(Y0):1], nrow=nrow(Y0)),yt),
                date=c(date[1]-((maxlags):1),date)))
  }
}



meanandci <- function(yi, cfyi, date, p=0.9, difflag=7) {

  y <- colMeans(yi)
  cfy <- colMeans(cfyi)
  dy <- matrix(NA,nrow=nrow(y),ncol=ncol(y))
  dy[(difflag+1):nrow(y),]  <- diff(y,difflag)

  dcfy <- matrix(NA,nrow=nrow(cfy),ncol=ncol(cfy))
  dcfy[(difflag+1):nrow(cfy),]  <- diff(cfy,difflag)
  statframe <- function(y,suffix) {
    d <- data.frame(m=rowMeans(y, na.rm=TRUE),
                    cl=apply(y, 1, function(x) quantile(x, (1-p)/2,na.rm=TRUE)),
                    ch=apply(y, 1, function(x) quantile(x,1-(1-p)/2,na.rm=TRUE)))
    names(d) <- paste(names(d), suffix, sep="")
    d
  }

  cbind(data.frame(date=date),
        statframe(colMeans(exp(yi)),"y"), statframe(dy,"dy"), statframe(colMeans(exp(cfyi)),"cf"),
        statframe(dcfy,"dcf"), statframe(colMeans(exp(cfyi)-exp(yi)),"p"),
        statframe(dcfy-dy,"dp"))

}

nopstatesim <- function(st, nsim=200, simobs, simcf, data, var="logdc", dvar="dlogdc", nresid=1) {
  seed <- .Random.seed
  set.seed(seed)
  yi <- replicate(nsim, simobs(st, samplecoefs=TRUE, nsim=nresid)$y)
  set.seed(seed)
  cfyi <- replicate(nsim, simcf(st, samplecoefs=TRUE, nsim=nresid)$y)
  date <- simobs(st, samplecoefs=TRUE, nsim=min(2, nresid))$date
  est <- meanandci(yi,cfyi, date)
  est <- merge(est, data.frame(state=st,
                        obs=exp(data[data$state==st, var]),
                        dobs=data[data$state==st, dvar],
                        date=data[data$state==st,]$date),
               by="date", all=TRUE)
  return(list(edf=est, y=yi, cfy=cfyi))
}


cfplots <- function(st, cfdata=NULL, simobs=NULL, simcf=NULL,
                    yvar="cases", data) {
  xlims <- datelims
  pds <-
    c("Mandate.face.mask.use.by.employees.in.public.facing.businesses",
      "Date.closed.K.12.schools",
      "Stay.at.home..shelter.in.place",
      "Closed.movie.theaters",
      "Closed.restaurants.except.take.out",
      "Closed.non.essential.businesses")
  if (is.null(cfdata)) {
    est <- nopstatesim(st, simobs=simobs, simcf=simcf, data=data)
  } else {
    est <- subset(cfdata, cfdata$state==st)
  }

  dt  <- c()
  labels <- c()
  for (lbl in pds) {
    d <- unique(data[data$state==st,lbl])
    i <- which(dt==d)
    if (length(i)==0) {
      if (length(dt)==0)
        dt <- d
      else
        dt <- c(dt,d)
      labels <- c(labels,lbl)
    } else {
      labels[i]  <- paste(labels[i],lbl,sep=", ")
    }
  }
  labels <- labels[!is.na(dt)]
  dt <- dt[!is.na(dt)]

  figl <-
    ggplot(data=est, aes(x=date, y=obs)) +
    geom_line(aes(color="Observed", fill="Observed"), size=1.5) +
    geom_line(aes(y=my,color="Estimated"), size=1.2) +
    #geom_ribbon(aes(ymin=cly, ymax=chy, fill="Estimated"),
    #            alpha=0.3, show.legend=FALSE) +
    geom_line(aes(y=mcf, color="Counterfactual"), size=1.2) +
    #geom_ribbon(aes(ymin=clcf, ymax=chcf, fill="Without Policies"),
    #            alpha=0.3, show.legend=FALSE) +
    figtheme + colors() + colors_fill() +
    ylab(sprintf("%s in past week",yvar)) + labs(color="")  +
    theme(legend.position=c(0.2, 0.8)) +
    ggtitle(TeX(sprintf("%s in past week",yvar))) + xlim(xlims)

  figp <- ggplot(data=est, aes(x=date, y=mp)) +
    geom_line(size=1.2) +
    geom_ribbon(aes(ymin=clp, ymax=chp), alpha=0.3) +
    ylab(sprintf("Change in %s in past week", yvar)) + figtheme + colors() + colors_fill() +
    ggtitle(TeX(sprintf("Change in %s",yvar))) +
    xlim(xlims) +
    annotate("text",x=dt, y=rep(0.01,length(dt)),
             label=gsub("\\."," ", labels), size=4, angle=90, hjust=0)


  figd <- ggplot(data=est, aes(x=date, y=dobs)) +
    geom_line(aes(y=mdcf, color="Counterfactual"), size=1.5) +
    geom_line(aes(color="Observed", fill="Observed"), size=1.5) +
    geom_line(aes(y=mdy,color="Estimated"), size=1.5) +
    #geom_ribbon(aes(ymin=cldy, ymax=chdy, fill="Estimated"),
    #            alpha=0.3, show.legend=FALSE) +
    #geom_ribbon(aes(ymin=cldcf, ymax=chdcf, fill="Without Policies"),
    #            alpha=0.3, show.legend=FALSE) +
    figtheme + colors() + colors_fill() +
    ylab(TeX(sprintf("$\\Delta \\log \\Delta %s$",toupper(substring(yvar,1,1))))) +
    labs(color="")  +
    theme(legend.position=c(0.8, 0.8)) +
    ggtitle(TeX(sprintf("$\\Delta \\log \\Delta %s$",toupper(substring(yvar,1,1))))) +
    xlim(xlims)

  figdp <- ggplot(data=est, aes(x=date, y=mdp)) +
    geom_line(size=1.2) +
    geom_ribbon(aes(ymin=cldp, ymax=chdp), alpha=0.3) +
    ylab(TeX(sprintf("Change in $\\Delta \\log \\Delta %s$",toupper(substring(yvar,1,1))))) +
    figtheme + colors() + colors_fill() +
    ggtitle(TeX(sprintf("Change in $\\Delta \\log \\Delta %s$",toupper(substring(yvar,1,1))))) +
    xlim(xlims) +
    annotate("text",x=dt, y=rep(0.01,length(dt)),
             label=gsub("\\."," ", labels), size=4, angle=90, hjust=0)

  return(list(figd=figd, figdp=figdp, figl=figl, figp=figp))
}

# Extract E[log(dc) |state, t, parameters] with parameters ~ N(m$coef, vcov(m))
getmeans <- function(sl) {
  date <- sl$edf$date[!is.na(sl$edf$my)]
  state <- sl$edf$state[!is.na(sl$edf$state)]
  stopifnot(length(unique(state))==1)
  y0 <- apply(exp(sl$y), c(2,3), mean)
  long <- melt(y0)
  names(long)[names(long)=="value"] <- "meany0"

  log0 <- apply(sl$y, c(2,3), mean)
  long1 <- melt(log0)
  names(long1)[names(long1)=="value"] <- "meanlog0"
  long <- merge(long, long1, by=c("Var1","Var2"))

  y1 <- apply(exp(sl$cf), c(2,3), mean)
  long1 <- melt(y1)
  names(long1)[names(long1)=="value"] <- "meany1"
  long <- merge(long,long1, by=c("Var1","Var2"))

  log1 <- apply(sl$cf, c(2,3), mean)
  long1 <- melt(log1)
  names(long1)[names(long1)=="value"] <- "meanlog1"
  long <- merge(long, long1, by=c("Var1","Var2"))

  long$date <- date[long$Var1]
  long$state <- unique(state)
  long$sidx <- long$Var2
  long$Var1 <- NULL
  long$Var2 <- NULL
  return(long)
}

nationalplots <- function(alldf,cfdf, cfname, yvar="cases") {
  alldf[order(state, sidx, date), dlog1:=c(rep(NA,7),diff(meanlog1,7)), by=.(state, sidx)]
  alldf[order(state, sidx, date),dlog0:=c(rep(NA,7),diff(meanlog0,7)), by=.(state, sidx)]
  national <- alldf[ , .(diff=sum(meany1-meany0), y0=sum(meany0), dlog=mean(dlog1-dlog0)), by=.(date, sidx)]

  adf <- national[, .(mp=mean(diff),
                      clo=quantile(diff, 0.05),
                      chi=quantile(diff, 0.95),
                      rel=mean(diff/y0),
                      rlo=quantile(diff/y0, 0.05),
                      rhi=quantile(diff/y0, 0.95),
                      mlog=mean(dlog, na.rm=TRUE),
                      llo=quantile(dlog, 0.025, na.rm=TRUE),
                      lhi=quantile(dlog, 0.975, na.rm=TRUE)
                      ),
                  keyby=.(date)]

  figdlog <- ggplot(cfdf, aes(x=date)) +
    #geom_text(aes(y=mdp, label=ST),alpha=0.2) +
    geom_point(aes(y=mdp),alpha=0.2, size=0.5) +
    xlim(datelims) +
    geom_line(data=adf,  aes(x=date, y=mlog, color=""), size=2) + figtheme +
    geom_ribbon(data=adf, aes(ymin=llo, ymax=lhi, fill=""),alpha=0.3) +
    colors() + theme(legend.position="none") +
    ylab(TeX(sprintf("Change in $\\Delta \\log \\Delta %s$",
                     toupper(substring(yvar,1,1))
                     ))) +
    colors_fill() +
     ggtitle(sprintf("Effect of %s on %s growth",cfname, substring(yvar,1,nchar(yvar)-1)))

  figc <- ggplot(cfdf, aes(x=date)) +
    #geom_text(aes(y=mp, label=ST),alpha=0.5) +
    geom_point(aes(y=mp),alpha=0.2, size=0.5) +
    xlim(datelims) +
    geom_line(data=adf, aes(x=date, y=mp, color=""), size=2) +
    figtheme +
    geom_ribbon(data=adf, aes(ymin=clo, ymax=chi, fill=""),alpha=0.3) +
    colors() + theme(legend.position="none") +
    ylab(sprintf("%s in the past week", yvar)) +
    colors_fill() +
    ggtitle(sprintf("Effect of %s on %s",cfname, yvar))

  figr <- ggplot(adf, aes(x=date, y=rel)) +
    xlim(datelims) +
    geom_line(aes(color=""), size=2) + figtheme +
    geom_ribbon(data=adf, aes(ymin=rlo, ymax=rhi, fill=""),alpha=0.3) +
    colors() + theme(legend.position="none") +
    ylab(sprintf("relative increase in %s",yvar)) +
    colors_fill() +
    ggtitle(sprintf("Relative effect of %s",cfname))
  return(list(g=figdlog, c=figc, r=figr))
}
