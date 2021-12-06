regprep <- function(cs, L = 7) {
  cs$t <- as.numeric(cs$date)

  ## Social distancing measures
  newnames <- c("residential","transit","workplaces","retail","grocery")
  idx <- sapply(newnames,function(x) grep(paste(x,"_",sep=""), names(cs)))
  for(v in newnames) {
    cs[,v] <- panelma(cs[,idx[v]], cs$state, cs$date, len=L-1)
    #cs[,v] <- panellag(cs[,v], cs$state, cs$date, lag=L)
  }

  ## Other regressors
  cs$tests <- paneldiff(cs$totalTestResults, cs$state, cs$date, lag=L)
  cs$dlogtests <- dlogd(cs$totalTestResults, cs$state, cs$date, lag=L)

  df <- subset(cs, cs$date>=as.Date("2020-01-01"))
  df <- df[order(df$state, df$date),]

  #df$tests[df$testmissing] <- 0
  df$testrate  <- df$tests/df$Population.2018*1000
  df$testrate[df$testrate<0] <- NA

  dlogd <- function(x, id, t, lag=L, minval=0.0, b=0, g=0) {
    dx  <- paneldiff(x, id, t, lag=lag)
    dx[dx<minval] <- minval
    dlogdx <- log(dx + exp(g)*b) - panellag(log(dx+b),id,t,lag=lag)
    dlogdx[!is.finite(dlogdx)] <- NA
    return(dlogdx)
  }
  df$cases.jhu <- df$cases
  df$deaths.jhu <- df$deaths

  df$cases <- df$cases.nyt
  df$cases[is.na(df$cases)] <- df$cases.jhu[is.na(df$cases)]
  df$cases[is.na(df$cases)] <- df$positive[is.na(df$cases)]

  df$deaths <- df$deaths.nyt
  df$deaths[is.na(df$deaths)] <- df$deaths.jhu[is.na(df$deaths)]
  df$deaths[is.na(df$deaths)] <- df$deaths.ctp[is.na(df$deaths)]


  df$dcases <- paneldiff(df$cases,df$state, df$date, lag=L)
  df$ddeath <- paneldiff(df$deaths,df$state, df$date, lag=L)

  df$logdc <- log(sapply(df$dcases, function(x) max(x, exp(-1))))

  df$dlogdc <- dlogd(df$cases, df$state, df$date, lag=L,
                     minval=exp(-1))

  df$dlogdd <-  dlogd(df$deaths, df$state, df$date, lag=L,
                      minval=exp(-1))

  df$logdd <- log(sapply(df$ddeath, function(x) max(x, exp(-1))))

  df$dlogtests <- paneldiff(log(sapply(df$testrate, function(x)
    max(x,exp(-1)))), df$state, df$date, lag=L)
  df$testratedc <- df$ddeath/df$dcases*df$testrate
  df$testratedc[df$dcases==0] <- 0

  enddates <- c(NA,
                "End.stay.at.home.shelter.in.place",
                "Began.to.reopen.businesses.statewide",
                "Face.mask.mandate.end",
                "Business.only.face.mask.mandate.end"
                )

  pdates <- c("Closed.K.12.public.schools",
              "Stay.at.home.shelter.in.place",
              "Closed.other.non.essential.businesses",
              "Business.face.mask.mandate.start",
              "Business.face.mask.mandate.start"
              )

  df$Business.only.face.mask.mandate.end <-
    pmin(df$Face.mask.mandate.end, df$Public.face.mask.mandate.start,
         na.rm=TRUE)
  df$Business.only.face.mask.mandate.end[is.na(df$Business.face.mask.mandate.start)] <- NA

  stopifnot(length(enddates)==length(pdates))

  vars <- c("k12", "shelter", "nonessential", "maskbus","maskbusonly")
  for(i in 1:length(pdates)) {
    df[,paste("p",vars[i],sep="")] <-
      smoothedpolicy(df,pdates[i],enddatevar=enddates[i],type="ma",bw=6)
    df[,paste("d",vars[i],sep="")] <-
      smoothedpolicy(df,pdates[i],enddatevar=enddates[i],type="ma",bw=0)
  }
  xdates <- c("restaurants",
              "gyms",
              "movie.theaters",
              "bars",
              "casinos")
  for (v in xdates) {
    d <- smoothedpolicy(df, paste("Closed",v,sep="."),
                        paste("Reopened",v,sep="."), type="ma",bw=0)
    phase <- 2
    while (length(grep(paste(v,"\\.",phase,sep=""),names(df)))>0) {
      d <- d + smoothedpolicy(df, sprintf("Closed.%s.x%d",v,phase),
                              sprintf("Reopened.%s.x%d",v,phase),
                              type="ma",bw=0)
    }
    df[,paste("d",v,sep="")] <- d
    df[,paste("p",v,sep="")] <- panelma(d,df$state,df$date,len=6)
  }

  df$dmaskall <- smoothedpolicy(df,"Public.face.mask.mandate.start",
                             "Face.mask.mandate.end", type="ma", bw=0)
  df$dmaskall <- df$dmaskall + smoothedpolicy(df,"Public.face.mask.mandate.start.x2",
                                        "Face.mask.mandate.end.x2",
                                        type="ma", bw=0)
  df$dmaskall <- df$dmaskall + smoothedpolicy(df,"Face.mask.mandate.resumed.for.fully.vaccinated",
                                        "Face.mask.mandate.end.for.fully.vaccinated",
                                        type="ma", bw=0)
  df$pmaskall <- panelma(df$dmaskall, df$state, df$date, len=6)



  df$vote <- df$voteshare
  df$logvote <- log(df$vote)

  statevars <- c("log(Population.2018)",
                 "log(Square.Miles)",
                 "Percent.Unemployed..2018.",
                 "Percent.living.under.the.federal.poverty.line..2018.",
                 "Percent.at.risk.for.serious.illness.due.to.COVID","party")#, "logvote")
  bvars <- c("workplaces","retail","grocery","transit")

  for(i in 1:length(bvars)) df[,bvars[i]] <- df[,bvars[i]]/100



  nc <- aggregate(cases ~ date, function(x) sum(x, na.rm=TRUE), data=df)
  names(nc) <- c("date","cases.national")
  nc$state <- "US"
  nc$dcases.national <- paneldiff(nc$cases.national,nc$state, nc$date, lag=L)
  nc$logdc.national <- log(sapply(nc$dcases.national, function(x) max(x, exp(-1))))
  nc$dlogdc.national <- dlogd(nc$cases.national, nc$state, nc$date, lag=L,
                              minval=exp(-1))
  nc$state <- NULL
  ndf <- merge(df, nc, by="date", all=TRUE)

  nc <- aggregate(deaths ~ date, function(x) sum(x, na.rm=TRUE), data=ndf)
  names(nc) <- c("date","deaths.national")
  nc$state <- "US"
  nc$ddeaths.national <- paneldiff(nc$deaths.national,nc$state, nc$date, lag=L)
  nc$logdd.national <- log(sapply(nc$ddeaths.national, function(x) max(x, exp(-1))))
  nc$dlogdd.national <- dlogd(nc$deaths.national, nc$state, nc$date, lag=L,
                              minval=exp(-1))
  nc$state <- NULL

  ndf <- merge(ndf, nc, by="date", all=TRUE)
  #ndf <- pdata.frame(ndf, index=c("state","t"))
  df <- ndf[order(ndf$state, ndf$date),]

  df$z.mask <- df$z.mask/100
  df$mask_percent <- df$mask_percent/100

  df$party <- as.factor(df$party)
  df$pindex <- (df$pmovie.theaters+df$prestaurant+df$pnonessential)/3

  month <- data.table::month(df$date)
  week <- data.table::week(df$date)
  df$month = as.factor(month)
  df$week = as.factor(week)

  df <- pdata.frame(df, index=c("state","t"), stringsAsFactors=FALSE)
  return(df)
}
