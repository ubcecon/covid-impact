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
source(paste(rootdir,"cases_and_policies/rmd/smoothtests.R", sep="/"))
figdatelims <- c(as.Date("2020-03-07"), max(df$date))
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
#' @return A function with argument idvar==value of identifier for
#'   which to simulate. The function returns a simulated path
#'   var beginning from the initial values for id==idvar and using the
#'   exogenous covariates of id==idvar.
#'
#' Example:
#' df <- df[order(df$id,$df$t),]
#' df$dy <- paneldiff(df$y, df$id, df$t, lag=2)
#' fmla <- dy ~ lag(y,3) + lag(dy,1) + x
#'
#' sim <- createforwardsim(fmla, df, "dy", "y", 2, TRUE)
createforwardsim <- function(m, df, id="state",
                       dvar="dlogdc", var="logdc",
                       difflag=7, lhsdiff=TRUE) {
  df <- df[order(df[,id], df$t),]
  stopifnot(m$keepX)
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

  sim <- function(idvar, samplecoefs=FALSE) {
    ids <- df[idx,id]
    if (samplecoefs) {
      V <- vcov(m)
      nac <- is.na(m$coefficients)
      e <- rep(0,nrow(V))
      Vnona <- V[!nac,!nac]
      e[!nac] <- t(rmvnorm(1, mean=rep(0, nrow(Vnona)), sigma=Vnona))

      beta <- m$coefficients + e
      resids <- m$response - m$X %*% beta

      dc <- beta[di]
      lc <- beta[li]
      ## convert into AR model
      ar <- rep(0, maxlags)
      if (lhsdiff) ar[difflag]  <- ar[difflag] + 1
      ar[llags]  <- ar[llags] + lc
      ar[dlags] <- ar[dlags] +dc
      ar[dlags+difflag] <- ar[dlags+difflag]-dc
      #x <- xt[ids==idvar] + (mid$X[ids==idvar,] %*% (e * as.numeric(!(di | li))))
    } else {
      beta <- m$coefficients
    }
    resids <- m$response - m$X %*% beta
    xt <- mid$X %*% (beta * as.numeric(!(di | li)))
    x <- xt[ids==idvar]

    date <- df$date[idx[df[idx,id]==idvar]]
    names(date) <- NULL
    it0 <- which(as.vector(df$date)==date[1] & as.vector(df[,id])==idvar)
    #t0 <- which(df$date==min(df$date))
    Y0 <- as.vector(sapply(1:(maxlags), function(k) lag(df[,var], k)[it0]))
    dY0 <- as.vector(sapply(1:(maxlags), function(k) lag(df[,dvar], k)[it0]))
    lagy0 <- Y0-dY0
    check <- lagy0[1:(length(Y0)-difflag)]- Y0[(difflag+1):length(Y0)]
    stopifnot(all(abs(check)<1e-8, na.rm=TRUE))
    na0 <- is.na(Y0[(difflag+1):length(Y0)])
    Y0[(difflag+1):length(Y0)][na0] <- lagy0[1:(length(Y0)-difflag)][na0]

    e <- resids[ids==idvar]
    Y <- Y0
    ## yt <- rep(0,length(x))
    ## for (t in 1:length(x)) {
    ##   yt[t] <- x[t] + Y %*% ar + e[t]
    ##   Y[2:length(Y)] <- Y[1:(length(Y)-1)]
    ##   Y[1] <- yt[t]
    ## }
    yf=as.vector(filter(x + e, ar, method="recursive", init=Y0))
    return(list(y=c(Y0[length(Y0):1],yf),
                date=c(date[1]-((maxlags):1),date)))
  }
}



meanandci <- function(yi, cfyi, date, p=0.9, difflag=7) {

  y <- yi
  cfy <- cfyi
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

  cumcases <- function(dc) filter(x=dc, filter=c(rep(0, difflag-1), 1),
                                  method="recursive", init=rep(0, difflag))

  cbind(data.frame(date=date),
        statframe(exp(yi),"y"), # weekly cases
        statframe(dy,"dy"), # growth
        statframe(exp(cfyi),"cf"), # cf weekly cases
        statframe(dcfy,"dcf"), # cf growth
        statframe((exp(cfyi)-exp(yi)),"p"), # change in weekly cases
        statframe(dcfy-dy,"dp"), # change in growth
        statframe(exp(cfyi)/exp(yi) - 1, "rel"), # relative weekly
        statframe(apply(exp(yi), 2, cumcases), "cum"), # cumulative cases
        statframe(apply(exp(cfyi), 2, cumcases), "cfcum"), # cf cumulative cases
        statframe(apply(exp(cfyi), 2, cumcases) -
                  apply(exp(yi), 2, cumcases), "dcum"), # change in cumulative cases
        statframe(apply(exp(cfyi), 2, cumcases)/
                  apply(exp(yi), 2, cumcases)- 1, "relcum") # relative cumulative
        )
}

nopstatesim <- function(st, nsim=200, simobs, simcf, data, var="logdc", dvar="dlogdc") {
  seed <- .Random.seed
  set.seed(seed)
  yi <- replicate(nsim, simobs(st, samplecoefs=TRUE)$y)
  set.seed(seed)
  cfyi <- replicate(nsim, simcf(st, samplecoefs=TRUE)$y)
  date <- simobs(st, samplecoefs=TRUE)$date
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
  xlims <- figdatelims
  pds <-
    c("Mandate.face.mask.use.by.employees.in.public.facing.businesses",
      "Date.closed.K.12.schools",
      "Stay.at.home..shelter.in.place",
      "Closed.movie.theaters",
      "Closed.restaurants.except.take.out",
      "Closed.non.essential.businesses")
  shortlbls <- c("Mandate masks",
                 "Close schools",
                 "Stay-at-home",
                 "Close move theaters",
                 "Close restaurants",
                 "Close non-essential businesses")
  if (is.null(cfdata)) {
    est <- nopstatesim(st, simobs=simobs, simcf=simcf, data=data)
  } else {
    est <- subset(cfdata, cfdata$state==st)
  }

  dt  <- c()
  labels <- c()
  for (p in 1:length(pds)) {
    lbl <- shortlbls[p]
    d <- as.Date(unclass(unique(data[data$state==st,pds[p]])))
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

  ylo <- min(est$clp/7)
  figp <- ggplot(data=est, aes(x=date, y=mp/7)) +
    geom_line(size=1.2) +
    geom_ribbon(aes(ymin=clp/7, ymax=chp/7), alpha=0.3) +
    ylab(sprintf("Change in %s per day", yvar)) + figtheme + colors() + colors_fill() +
    ggtitle(TeX(sprintf("Change in daily %s",yvar))) +
    xlim(xlims) #+
    #annotate("text",x=dt, y=rep(ylo,length(dt)),
    #         label=gsub("\\."," ", labels), size=4, angle=90, hjust=0)


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

  ylo <- min(est$cldp)
  figdp <- ggplot(data=est, aes(x=date, y=mdp)) +
    geom_line(size=1.2) +
    geom_ribbon(aes(ymin=cldp, ymax=chdp), alpha=0.3) +
    ylab(TeX(sprintf("Change in $\\Delta \\log \\Delta %s$",toupper(substring(yvar,1,1))))) +
    figtheme + colors() + colors_fill() +
    ggtitle(TeX(sprintf("Change in $\\Delta \\log \\Delta %s$",toupper(substring(yvar,1,1))))) +
    xlim(xlims) +
    annotate("text",x=dt, y=rep(ylo,length(dt)),
             label=gsub("\\."," ", labels), size=4, angle=90, hjust=0)

  figcum <- ggplot(data=est, aes(x=date, y=mcum)) +
    geom_line(aes(color="Observed"), size=1.5) +
    geom_line(aes(y=mcfcum,color="Counterfactual"), size=1.5) +
    geom_ribbon(aes(ymin=clcfcum, ymax=chcfcum, fill="Counterfacual"),
                alpha=0.3, show.legend=FALSE) +
    geom_ribbon(aes(ymin=cldcf, ymax=chdcf, fill=""),
                alpha=0.3, show.legend=FALSE) +
    figtheme + colors() + colors_fill() +
    ylab(TeX(sprintf("Cumulative %s$",yvar))) +
    labs(color="")  +
    theme(legend.position=c(0.8, 0.8)) +
    ggtitle(TeX(sprintf("Cumulative %s$",yvar))) +
    xlim(xlims)

  figdcum <- ggplot(data=est, aes(x=date, y=mdcum)) +
    geom_line(size=1.2) +
    geom_ribbon(aes(ymin=cldcum, ymax=chdcum), alpha=0.3) +
    ylab(sprintf("Change in cumulative %s", yvar)) + figtheme + colors() + colors_fill() +
    ggtitle(TeX(sprintf("Change in cumulative %s",yvar))) +
    xlim(xlims)

  figrcum <- ggplot(data=est, aes(x=date, y=mrelcum)) +
    geom_line(size=1.2) +
    geom_ribbon(aes(ymin=clrelcum, ymax=chrelcum), alpha=0.3) +
    ylab(sprintf("Relative change in cumulative %s", yvar)) + figtheme + colors() + colors_fill() +
    ggtitle(TeX(sprintf("Relative change in cumulative %s",yvar))) +
    xlim(xlims)

  figr <- ggplot(data=est, aes(x=date, y=mrel)) +
    geom_line(size=1.2) +
    geom_ribbon(aes(ymin=clrel, ymax=chrel), alpha=0.3) +
    ylab(sprintf("Relative change in daily %s", yvar)) + figtheme + colors() + colors_fill() +
    ggtitle(TeX(sprintf("Relative change in daily %s",yvar))) +
    xlim(xlims)

  return(list(figd=figd, figdp=figdp, figl=figl, figp=figp, figr=figr, figcum=figcum, figdcum=figdcum, figrcum=figrcum))
}

# Extract E[log(dc) |state, t, parameters] with parameters ~ N(m$coef, vcov(m))
getmeans <- function(sl, difflag=7) {
  date <- sl$edf$date[!is.na(sl$edf$my)]
  state <- sl$edf$state[!is.na(sl$edf$state)]
  stopifnot(length(unique(state))==1)
  if(length(dim(sl$y))==3) {
    trans <- function(x) apply(x, c(2,3), mean)
  } else {
    trans <- function(x) x
  }

  cumcases <- function(dc) filter(x=dc, filter=c(rep(0, difflag-1), 1),
                                  method="recursive", init=rep(0, difflag))

  y0 <- trans(exp(sl$y))
  long <- reshape2::melt(y0)
  names(long)[names(long)=="value"] <- "meany0"

  log0 <- trans(sl$y)
  long1 <- reshape2::melt(log0)
  names(long1)[names(long1)=="value"] <- "meanlog0"
  long <- merge(long, long1, by=c("Var1","Var2"))

  y1 <- trans(exp(sl$cf))
  long1 <- reshape2::melt(y1)
  names(long1)[names(long1)=="value"] <- "meany1"
  long <- merge(long,long1, by=c("Var1","Var2"))

  log1 <- trans(sl$cf)
  long1 <- reshape2::melt(log1)
  names(long1)[names(long1)=="value"] <- "meanlog1"
  long <- merge(long, long1, by=c("Var1","Var2"))

  cum0 <- trans(apply(exp(sl$y), 2, cumcases))
  long1 <- reshape2::melt(cum0)
  names(long1)[names(long1)=="value"] <- "cum0"
  long <- merge(long, long1, by=c("Var1","Var2"))

  cum1 <- trans(apply(exp(sl$cf), 2, cumcases))
  long1 <- reshape2::melt(cum1)
  names(long1)[names(long1)=="value"] <- "cum1"
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
  national <- alldf[ , .(diff=sum(meany1-meany0),
                         y0=sum(meany0),
                         dlog=mean(dlog1-dlog0),
                         dcum=sum(cum1-cum0),
                         cum0=sum(cum0)
                         ), by=.(date, sidx)]

  adf <- national[, .(mp=mean(diff),
                      clo=quantile(diff, 0.05),
                      chi=quantile(diff, 0.95),
                      rel=mean(diff/y0),
                      rlo=quantile(diff/y0, 0.05),
                      rhi=quantile(diff/y0, 0.95),
                      mlog=mean(dlog, na.rm=TRUE),
                      llo=quantile(dlog, 0.05, na.rm=TRUE),
                      lhi=quantile(dlog, 0.95, na.rm=TRUE),
                      crel=mean(dcum/cum0),
                      crlo=quantile(dcum/cum0, 0.05),
                      crhi=quantile(dcum/cum0, 0.95)
                      ),
                  keyby=.(date)]

  figdlog <- ggplot(cfdf, aes(x=date)) +
    #geom_text(aes(y=mdp, label=ST),alpha=0.2) +
    geom_point(aes(y=mdp),alpha=0.2, size=0.5) +
    xlim(figdatelims) +
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
    geom_point(aes(y=mp/7),alpha=0.2, size=0.5) +
    xlim(figdatelims) +
    geom_line(data=adf, aes(x=date, y=mp/7, color=""), size=2) +
    figtheme +
    geom_ribbon(data=adf, aes(ymin=clo/7, ymax=chi/7, fill=""),alpha=0.3) +
    colors() + theme(legend.position="none") +
    ylab(sprintf("%s per day", yvar)) +
    colors_fill() +
    ggtitle(sprintf("Effect of %s on %s",cfname, yvar))

  figr <- ggplot(adf, aes(x=date, y=rel)) +
    xlim(figdatelims) +
    geom_line(aes(color=""), size=2) + figtheme +
    geom_ribbon(data=adf, aes(ymin=rlo, ymax=rhi, fill=""),alpha=0.3) +
    colors() + theme(legend.position="none") +
    ylab(sprintf("relative increase in daily %s",yvar)) +
    colors_fill() +
    ggtitle(sprintf("Relative effect of %s",cfname))

  figcr <- ggplot(adf, aes(x=date, y=crel)) +
    xlim(figdatelims) +
    geom_line(aes(color=""), size=2) + figtheme +
    geom_ribbon(data=adf, aes(ymin=crlo, ymax=crhi, fill=""),alpha=0.3) +
    colors() + theme(legend.position="none") +
    ylab(sprintf("relative increase in cumulative %s",yvar)) +
    colors_fill() +
    ggtitle(sprintf("Relative effect of %s",cfname))

  return(list(g=figdlog, c=figc, r=figr, cr=figcr))
}
