cs$t <- as.numeric(cs$date)

warning("Using data only up to 2020-06-03. Modify lines 4-5 of regprep.R to change")
df <- subset(cs, cs$date>=as.Date("2020-01-01") &
                                   cs$date<=as.Date("2020-06-03"))
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
df$pmaskbusonly <- smoothedpolicy(df,
                                  "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
                                  "Mandate.face.mask.use.by.all.individuals.in.public.spaces",
           type="ma", bw=6)
df$pmaskany <- apply(df[,c("pmaskbus","pmaskall")], 1, max)
pols <- c("pmaskbus","pmaskall", "pk12","pshelter","pmovie","pgyms","prestaurant","pnonessential")


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
df$pindex <- (df$pmovie+df$prestaurant+df$pnonessential)/3

tilt = ifelse(df$date> as.Date("2020-05-01"),1,0 )
month <- data.table::month(df$date)
week <- data.table::week(df$date)
#month[month==6] <- 5
#month[month<3] <- 3
df$month = as.factor(month)
df$week = as.factor(week)
df$pmask.may <- df$pmaskbus*tilt
df$pmask.april <- df$pmaskbus*(1-tilt)
pols <- c("pmaskbus","pk12","pshelter","pmovie","prestaurant","pnonessential")



## the following variables are used for DML
df$logpop <- log(df$Population.2018)
df$logsq <- log(df$Square.Miles)
df$repub <- as.factor(df$party)
df$party_dum <- ifelse(df$party == "republican", 0, 1)
df$month3 <- ifelse(df$month==3,0,1)
df$month4 <- ifelse(df$month==4,0,1)
df$month5 <- ifelse(df$month==5,0,1)
df$month6 <- ifelse(df$month==6,0,1)
L <- L.c
pols <- c("pmaskbus","pk12","pshelter","pmovie","prestaurant","pnonessential")
infovars <- list(c("dlogdc", "logdc"),
                 c("dlogdc", "logdc","dlogdc.national", "logdc.national"))
infovarsd <- list(c("dlogdd", "logdd"),
                  c("dlogdd", "logdd","dlogdd.national", "logdd.national"))
bvars <- c("workplaces","retail","grocery","transit")
vars <- c(infovars[[2]],infovarsd[[2]],pols,"pindex")
for(i in 1:length(vars)) {
  L<-L.c
  v <- sprintf("%s.L14",vars[i])
  df[,v] <- panellag(df[,vars[i]],df$state,df$date, lag=L)
  L<-L.d
  v <- sprintf("%s.L21",vars[i])
  df[,v] <- panellag(df[,vars[i]],df$state,df$date, lag=L)
}
for(i in 1:length(bvars)) {
  L<-L.c
  v <- sprintf("%s.L14",bvars[i])
  df[,v] <- panellag(df[,bvars[i]],df$state,df$date, lag=L+14)
  L<-L.d
  v <- sprintf("%s.L21",bvars[i])
  df[,v] <- panellag(df[,bvars[i]],df$state,df$date, lag=L+14)
}
svars <- c("logpop","logsq","Percent.Unemployed..2018.",
           "Percent.living.under.the.federal.poverty.line..2018.",
           "Percent.at.risk.for.serious.illness.due.to.COVID","party_dum")
mvars.c <- mvars.d <- mvars <- c("month4","month5","month6")
# mvars.c <- c("month4.c","month5.c","month6.c")
# mvars.d <- c("month4.d","month5.d","month6.d")
ijs <- expand.grid(1:length(svars), 1:length(mvars.c))
msvars.c <- vector(mode="character", length=lengths(ijs)[[1]])
msvars.d <- vector(mode="character", length=lengths(ijs)[[1]])
for (ij in (1:lengths(ijs)[[1]])){
  v <-  sprintf("%s.%s",svars[ijs[[1]][ij]],mvars.c[ijs[[2]][ij]])
  msvars.c[ij] <- v
  df[,v] <- df[,svars[ijs[[1]][ij]]]*df[,mvars.c[ijs[[2]][ij]]]
  v <-  sprintf("%s.%s",svars[ijs[[1]][ij]],mvars.d[ijs[[2]][ij]])
  msvars.d[ij] <- v
  df[,v] <- df[,svars[ijs[[1]][ij]]]*df[,mvars.d[ijs[[2]][ij]]]
}
bvars.c <- c("workplaces.L14","retail.L14","grocery.L14","transit.L14")
bvars.d <- c("workplaces.L21","retail.L21","grocery.L21","transit.L21")
wvars <- c(svars,mvars.c,msvars.c,"month3","mask_percent","logvote","dlogtests")
wvars.d <- c(svars,mvars.d,msvars.d,"month3","mask_percent","logvote")
xvars <- c(bvars.c,"dlogdc.L14","logdc.L14")
xvars.national <- c(bvars.c,"dlogdc.L14","logdc.L14","dlogdc.national.L14","logdc.national.L14")
xvars.d <- c(bvars.d,"dlogdd.L21","logdd.L21")
xvars.national.d <- c(bvars.d,"dlogdd.L21","logdd.L21","dlogdd.national.L21","logdd.national.L21")
pols.L14 <- c("pmaskbus.L14","pk12.L14","pshelter.L14","pindex.L14")
pols.L21 <- c("pmaskbus.L21","pk12.L21","pshelter.L21","pindex.L21")

df <- pdata.frame(df, index=c("state","t"), stringsAsFactors=FALSE)
