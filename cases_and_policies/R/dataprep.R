## This will break if you didn't check out the git repo.
## If it breaks, just set rootdir to whatever you need
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
datafile <- paste(rootdir,"cases_and_policies/data/covidstates.Rda", sep="/")
cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
cat("Run ", paste(rootdir,"cases_and_policies/R/cases_and_policies.R", sep="/"), " to update data.\n")
base::load(paste(rootdir,"cases_and_policies/data/covidstates.Rda", sep="/"))
source(paste(rootdir,"cases_and_policies/R/utils.R",sep="/"))

cs <- covidstates
cs <- cs[cs$fips<60, ] # drop territories (Puerto Rico, Guam, etc)
cs <- cs[order(cs$state, cs$date),]

## Outcome variable
L <- 7
dlogd <- function(x, id, t, lag=L, minval=0.1) {
  dx  <- paneldiff(x, id, t, lag=lag)
  dx[dx<minval] <- minval
  dlogdx <- paneldiff(log(dx),id,t,lag=lag)
}

cs$dlogdc <- dlogd(cs$cases.nyt, cs$state, cs$date, lag=L)
cs$dlogdc.jhu <- dlogd(cs$cases, cs$state, cs$date, lag=L)
cs$dcases <- paneldiff(cs$cases.nyt,cs$state, cs$date, lag=L)
cs$dcases.jhu <- paneldiff(cs$cases,cs$state, cs$date, lag=L)

## Social distancing measures
newnames <- c("residential","transit","workplaces","retail","grocery")
idx <- sapply(newnames,function(x) grep(paste(x,"_",sep=""), names(cs)))
for(v in newnames) {
  cs[,v] <- panelma(cs[,idx[v]], cs$state, cs$date, len=L-1)
  #cs[,v] <- panellag(cs[,v], cs$state, cs$date, lag=L)
}

## Policy variables
bw <- 7
newnames <- c("movie","shelter","k12","restaurant","nonessential")
oldnames <- c("Closed.movie.theaters","Stay.at.home..shelter.in.place",
              "Date.closed.K.12.schools","Closed.restaurants.except.take.out",
              "Closed.non.essential.businesses")
for(i in 1:length(newnames)) {
  v <- sprintf("dummy_%s",newnames[i],bw)
  cs[,v]  <- cs$date >= cs[,oldnames[i]]
  cs[is.na(cs[,v]),v] <- 0.0
  v <- sprintf("p%s_%d",newnames[i],bw)
  cs[,v] <- as.numeric(pnorm(cs$date, cs[,oldnames[i]], bw))
  cs[is.na(cs[,v]),v] <- 0.0
}

## Other regressors
cs$tests <- paneldiff(cs$totalTestResults, cs$state, cs$date, lag=L)
cs$dlogtests <- dlogd(cs$totalTestResults, cs$state, cs$date, lag=L)

lognozero <- function(x, minval=0.1) {
  x[x<minval] <- minval
  return(log(x))
}
cs$logdc <- lognozero(cs$dcases)
cs$logdc.jhu <- lognozero(cs$dcases.jhu)

cs$ddeath <- paneldiff(cs$deaths.nyt, cs$state, cs$date, lag=L)
cs$ddeath.jhu <- paneldiff(cs$deaths, cs$state, cs$date, lag=L)
