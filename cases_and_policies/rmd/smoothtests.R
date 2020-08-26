# We rearrange and linearly interpolate constant test counts, but do not
# apply any additional smoothing.

warning("Linearly interpolating and rearranging test counts and creating dlogtest varaible in
data frame df.")

rearrange <- function(x, id) {
  for (i in unique(id)) {
    xi <- x[id==i]
    xi[!is.na(xi)] <- sort(xi[!is.na(xi)])
    x[id==i] <- xi
  }
  return(x)
}


dropnotincreasing <- function(x, id, t) {
  const <- (x <= panellag(x, id, t))
  const[is.na(const)] <- FALSE
  x[const & x>0] <- NA
  return(x)
}

lininterp <- function(x, id, t) {
  for (i in unique(id)) {
    xi <- x[id==i]
    ti <- t[id==i]
    xi[is.na(xi)] <- approx(ti[!is.na(xi)], xi[!is.na(xi)], ti[is.na(xi)], rule=2)$y
    x[id==i] <- xi
  }
  return(x)
}


smoothtests <- function(x, id, t) {
  x[is.na(x)] <- 0
  return(lininterp(dropnotincreasing(rearrange(x,id),id,t),id,t))
}

df$neg.interp <- smoothtests(df$negative, df$state, df$date)
df$pos.interp <- df$cases #smoothtests(df$positive, df$state, df$date)
df$tot.interp <- df$neg.interp + df$pos.interp

L <- 7
df$tests <- paneldiff(df$tot.interp, df$state, df$date, lag=L)
df$testrate  <- df$tests/df$Population.2018*1000
#df$testrate[df$testrate<0] <- NA
df$dlogtests <- paneldiff(log(sapply(df$testrate, function(x)
  max(x,exp(-1)))), df$state, df$date, lag=L)
df$testratedc <- df$ddeath/df$dcases*df$testrate
df$testratedc[df$dcases==0] <- 0

df$tests.ma <- paneldiff(panelma(df$neg.interp, df$state, df$date, 7) + df$cases,
                         df$state, df$date, lag=L)
df$testrate.ma  <- df$tests.ma/df$Population.2018*1000
df$testrate.ma[df$testrate.ma<0] <- NA
df$dlogtests.ma <- paneldiff(log(sapply(df$testrate.ma, function(x)
  max(x,exp(-1)))), df$state, df$date, lag=L)
df$testratedc.ma <- df$ddeath/df$dcases*df$testrate.ma
df$testratedc.ma[df$dcases==0] <- 0

df$logtests <- log(sapply(df$testrate, function(x)
  max(x,exp(-1))))

df$lagtests <- panellag(df$dlogtests, df$state, df$date, lag=1)
df$dlogtests[is.na(df$dlogtests)] <- df$lagtests[is.na(df$dlogtests)]
