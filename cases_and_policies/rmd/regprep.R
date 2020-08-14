cs$t <- as.numeric(cs$date)

warning("Using data only up to 2020-06-02 as in submitted version of paper. Modify lines 4-5 of regprep.R to change")
df <- pdata.frame(subset(cs, cs$date>=as.Date("2020-01-01") &
                                   cs$date<=as.Date("2020-06-02")),
                  # cs$date<=as.Date("2020-06-30")),
                  index=c("state","t"), stringsAsFactors=FALSE)

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
              "End.relax.stay.at.home.shelter.in.place",
              "Reopened.movie.theaters",
              "Reopen.restaurants",
              "Began.to.reopen.businesses.statewide",
              NA,
              NA,
              NA,
              NA,
              "Reopened.gyms")

pdates <- c("Date.closed.K.12.schools",
            "Stay.at.home..shelter.in.place",
            "Closed.movie.theaters",
            "Closed.restaurants.except.take.out",
            "Closed.non.essential.businesses",
            "Mandate.face.mask.use.by.all.individuals.in.public.spaces",
            "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
            "State.of.emergency",
            "Began.to.reopen.businesses.statewide",
            "Closed.gyms")

stopifnot(length(enddates)==length(pdates))

pvars <- c("pk12", "pshelter", "pmovie", "prestaurant", "pnonessential",
           "pmaskall","pmaskbus", "psoe", "preopenbus", "pgyms")
for(i in 1:length(pdates)) df[,pvars[i]] <-
                             smoothedpolicy(df,pdates[i],enddatevar=enddates[i],type="ma",bw=6)
df$pmaskany <- apply(df[,c("pmaskbus","pmaskall")], 1, max)
pols <- c("pmaskbus", "pk12","pshelter","pmovie","pgyms","prestaurant","pnonessential")


df$vote <- df$voteshare
df$logvote <- log(df$vote)

statevars <- c("log(Population.2018)",
               "log(Square.Miles)",
               "Percent.Unemployed..2018.",
               "Percent.living.under.the.federal.poverty.line..2018.",
               "Percent.at.risk.for.serious.illness.due.to.COVID","party")#, "logvote")
bvars <- c("workplaces","retail","grocery","transit")

for(i in 1:length(bvars)) df[,bvars[i]] <- df[,bvars[i]]/100 


#df$udistance <- panelma(df$daily_distance_diff,df$state, df$date, 6)
#df$uvisits <- panelma(df$daily_visitation_diff,df$state, df$date, 6)
#df$sghometime <- panelma(df$median_home_dwell_time, df$state, df$date, 6)
#df$sgpart <- panelma(df$part_time_work_behavior_devices/df$device_count, df$state, df$date, 6)
#df$sgfull <- panelma(df$full_time_work_behavior_devices/df$device_count, df$state, df$date, 6)

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
ndf <- pdata.frame(ndf, index=c("state","t"))
df <- ndf[order(ndf$state, ndf$date),]

df$z.mask <- df$z.mask/100
df$mask_percent <- df$mask_percent/100

df$party <- as.factor(df$party)

df$pindex <- (df$pshelter+df$pmovie+df$prestaurant+df$pnonessential)/4
#df$pindex <- (df$pshelter+df$prestaurant+df$pnonessential)/3

tilt = ifelse(df$date> as.Date("2020-05-01"),1,0 )
month <- month(df$date)
#month[month==6] <- 5
#month[month<3] <- 3
df$month = as.factor(month)
df$pmask.may <- df$pmaskbus*tilt
df$pmask.april <- df$pmaskbus*(1-tilt)
pols <- c("pmaskbus","pk12","pshelter","pmovie","prestaurant","pnonessential") 
#pols <- c("pmaskbus","pk12","pindex") 
