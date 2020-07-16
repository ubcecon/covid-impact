library(lfe)
library(plm)

rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
source(paste(rootdir,"tex/dataprep.R",sep="/"))

cs$t <- as.numeric(cs$date)
df <- pdata.frame(cs, index=c("state","t"), stringsAsFactors=FALSE)

behave <- c("residential","transit","workplaces","retail","grocery")
policy <- c("pmovie_7","pshelter_7","pk12_7","prestaurant_7","pnonessential_7")
cor(df[,c(behave,policy)], use="pairwise.complete.obs")

df$tau  <- df$tests/df$Population.2018*1000
df$taudc <- (df$ddeath.jhu/df$dcases.jhu)*df$tau
df$trend <- log(as.numeric(cs$date - as.Date("2020-01-29")))
df$Y <- df$dlogdc.jhu
df$Y[lag(df$cases,7)<10 ] <- NA
df$taudc <- df$ddeath.jhu/df$dcases.jhu*df$tau
df$taudc[df$cases<10] <- NA
# normalize taudc
df$taudc <- df$taudc*1000

# Table 1
yvar <- "Y"
xvars <- c("dlogtests",
           "lag(tau,14)",
           "lag(logdc.jhu,14)",
           "trend",
           "lag(Y,7)")
interactions <- list(c("lag(taudc,7)"),
                     c("log(Population.2018)",
                       "log(Square.Miles)",
                       "Percent.Unemployed..2018.",
                       "Percent.living.under.the.federal.poverty.line..2018.",
                       "Percent.at.risk.for.serious.illness.due.to.COVID"))
cluster  <- "state"
smoothedpolicy <- function(df, pdatevar, type=c("pnorm","ma"), bw=7) {
  if (type=="pnorm") {
    p <- as.numeric(pnorm(df$date, df[,pdatevar], bw))
    p[is.na(p)] <- 0.0
  } else if(type=="ma") {
    d <- df$date >= df[,pdatevar]
    d[is.na(d)] <- FALSE
    p <- panelma(d, df$state, df$date, len=bw)
  }
  return(p)
}

createfmla <- function(yvar, xvars, interactions, fe = "0", iv="0", cluster="state") {
  rhs <- paste(xvars, collapse=" + ")
  rhs <- paste(rhs, " + (", paste(interactions[[1]], collapse = " + "), ")*(",
               paste(interactions[[2]], collapse = " + "), ")")
  if (!is.null(fe)) {
    rhs <- paste(rhs," | ", fe, " | ", iv , " | ", paste(cluster, collapse=" + "))
  }
  return(list(as.formula(paste(yvar, "~", rhs, sep=" ")), rhs))
}

#df$pshelter <- smoothedpolicy(df, "Stay.at.home..shelter.in.place", "pnorm", 7)
summary(felm(createfmla("Y",c("lag(pmovie_7,14)", xvars), interactions)[[1]], data=df))
summary(plm(createfmla("Y",c("lag(pmovie_7,14)", xvars), interactions, NULL)[[1]], data=df, model="pooling"))


if (false) {

  df2 <- read.csv(paste(rootdir, "cases_and_policies/Stata/specification2_0507.csv", sep="/"),
                  stringsAsFactors=FALSE)


  #df2 <- df2[!is.na(df2$fips),]
  df2$date <- as.Date(df2$date_d, format="%d%b%Y") + 14
  vars2 <- c("fips","date",
             "y1","pmaskbus",
             "y1_lag","taudc","logdc_lag","logt","tau_lag","dlogtau",
             "logpop","logsqmiles","unemp","poverty","illness")
  tmp <- merge(cs, df2[!is.na(df2$fips),vars2], by=c("fips","date"), all.x=TRUE)
  tmp$t <- as.numeric(tmp$date)
  df <- pdata.frame(tmp, index=c("fips","t"))
  df$pmaskbus_7 <- smoothedpolicy(df,"Mandate.face.mask.use.by.employees.in.public.facing.businesses")
  df$Y <- df$dlogdc.jhu
  df$Y[lag(df$cases,7)<10] <- NA
  #df$lcases7 <- lag(df$cases, 7)
  #df$lcases14 <- lag(df$cases, 14)
  df$trend <- log(as.numeric(cs$date - as.Date("2020-01-29")))
  df$tau  <- df$tests/df$Population.2018
  df$taudc  <- df$taudc*1000
  #df$taudc <- (df$ddeath.jhu/df$dcases.jhu)*df$tau
  #df$taudc[df$cases<10] <- NA
  #df$taudc[df$ddeath.jhu<=0 | df$dcases.jhu<=0] <- NA

  #df$tau.x  <- df$tests/df$Population.2018
  #p <- is.na(df$Y) != is.na(df$y1)
  #View(df[p,c("state","date","cases","lcases7","lcases14","deaths","Y","y1")])

  #df$Y[is.na(df$y1)] <- NA

  basevars <- "dlogtests + lag(I(tests/Population.2018),14) + lag(logdc.jhu,7) + trend + lag(Y,7) +(log(Population.2018) + log(Square.Miles) + Percent.Unemployed..2018. + Percent.at.risk.for.serious.illness.due.to.COVID)*lag(taudc,7) | 0 | 0 | state"

  summary(felm(Y ~ pmaskbus +  dlogtests + lag(tau,14)
               + logdc_lag + logt + lag(Y,7) + #taudc*
               taudc*
                 #pop_taudc + sq_taudc + ue_taudc + ill_taudc +
                 #logpop + logsqmiles + unemp + poverty + illness )
               (log(Population.2018) +
               log(Square.Miles) +
               Percent.Unemployed..2018. +
               Percent.living.under.the.federal.poverty.line..2018. +
               Percent.at.risk.for.serious.illness.due.to.COVID)
               #| 0 | 0 | fips
          , data=df))

}
