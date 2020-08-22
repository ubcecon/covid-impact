library(rvest)
library(gsheet)

stub_ <- function() {}
thisPath <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1]
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
  } else if (Sys.getenv("RSTUDIO") == "1") {
    # RStudio
    dirname(rstudioapi::getSourceEditorContext()$path)
  } else if (is.null(attr(stub_, "srcref")) == FALSE) {
    # 'source'd via R console
    dirname(normalizePath(attr(attr(stub_, "srcref"), "srcfile")$filename))
  } else {
    "."
  }
}

gitrootdir <- function() {
  rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  if (is.na(rootdir)) {
    thisfilepath <- thisPath()
    rootdir <- normalizePath(paste(thisfilepath,"../..",sep="/"))
  }
  validationfile <- normalizePath(paste(rootdir,"cases_and_policies/",sep="/"))
  if (!file.exists(validationfile)) {
    stop("Could not determine location of git tree for covid project. Setting R's working directory to anywhere within the git repo should resolve this problem.")
  }
  return(rootdir)
}

rootdir <- gitrootdir()


# Data on policies from Raifman and collaborators
urls <- c("https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=993060716",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1357478819",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1894978869",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1269444822",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1489353670",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=654980200",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=69456887",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1921407906",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=299430094",
          #"https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=87338810",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1643322116",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=1846559547",
          "https://docs.google.com/spreadsheets/d/1zu9qEWI8PsOI_i8nI_S29HDGHlIp2lfVMsGxpQ5tvAQ/edit#gid=698331724")
poldf <- list()
for(i in 1:length(urls)) {
  poldf[[i]] <- data.frame(gsheet2tbl(urls[i]))[1:51,]
}
policies <- poldf[[1]]
for(i in 2:length(urls)) {
  policies <- merge(policies, poldf[[i]], by=c("State","State.Abbreviation","State.FIPS.Code"))
}

# remove footnoes

datecols <- c("State.of.emergency",
              "Date.closed.K.12.schools",
              "Closed.day.cares",
              "Date.banned.visitors.to.nursing.homes",
              "Stay.at.home..shelter.in.place",
              "Closed.non.essential.businesses",
              "Closed.restaurants.except.take.out",
              "Closed.gyms",
              "Closed.movie.theaters",
              "Stop.enforcement.of.evictions.overall.or.due.to.COVID.related.issues",
              "Stop.Initiation.of.Evictions.overall.or.due.to.COVID.related.issues" ,
              "Order.freezing.utility.shut.offs",
              "Froze.mortgage.payments",
              "Waived.one.week.waiting.period.for.unemployment.insurance",
              "End.relax.stay.at.home.shelter.in.place",
              "Began.to.reopen.businesses.statewide",
              "Mandate.face.mask.use.by.all.individuals.in.public.spaces",
              "Mandate.face.mask.use.by.employees.in.public.facing.businesses",
              "Reopen.restaurants",
              "Reopened.gyms",
              "Reopened.movie.theaters",
              "Resumed.elective.medical.procedures",
              "Renter.grace.period.or.use.of.security.deposit.to.pay.rent"
              )
for(col in datecols) {
  policies[,col]  <-  as.Date(policies[,col], format="%m/%d/%Y")
}

## remove commas and convert to numbers
numcols  <- c("Population.2018","Square.Miles","Number.Homeless..2019.",
              "Percent.Unemployed..2018.","Percent.living.under.the.federal.poverty.line..2018.",
              "Percent.at.risk.for.serious.illness.due.to.COVID", "All.cause.deaths.2018")
for (col in numcols) {
  if (!is.numeric(policies[,col]))
    policies[,col]<- as.numeric(gsub(",","", policies[,col]))
}

# policies with missings filled in by Victor, Hiro, & Paul
polfix <- data.frame(gsheet2tbl("https://docs.google.com/spreadsheets/d/1E6HRkgbdSnZ9ZxrneydU6q4hhOCCt9oTl_5fa3OFVZE/edit?usp=sharing"))
polfix <- polfix[1:51,1:6]
for(v in names(polfix)[2:6]) {
  polfix[,v] <- as.Date(polfix[,v], format="%m/%d/%Y")
  diffs  <-  polfix[,v] != policies[,v]
  diffs[is.na(diffs)] <- FALSE
  if (any(diffs)) {
    warning(sprintf("Policies and our fixed policies differ for %s",v))
    show(cbind(policies[diffs, c("State",v)], polfix[diffs,c("State",v)]))
  }
  adds <- is.na(policies[,v]) & !is.na(polfix[,v])
  if (any(adds)) {
    warning(sprintf("Our fixed policies change in %d values from NA to non-NA for %s",sum(adds),v))
  }
  policies[,v]  <- polfix[,v]
}

# Data on state cases, deaths, etc from JHU
jhucoviddata <- function() {
  confirmed = read.csv('https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv?raw=true', stringsAsFactors=FALSE)
  deaths = read.csv('https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv?raw=true', stringsAsFactors=FALSE)
  makelong <- function(df, varname) {
    names(df) <- gsub("\\.","-",names(df))
    names(df) <- gsub("^X","date.",names(df))
    day0 <- as.Date("2020-01-01")
    for (c in grep("date\\..+",names(df))) {
      d <- as.Date(gsub("date\\.","",names(df)[c]), format="%m-%d-%y") - day0
      names(df)[c] <- sprintf("count.%03d",d)
    }
    df <- reshape(df, varying=grep("count\\..+",names(df)), direction="long")
    df$date <- day0 + df$time
    names(df)[names(df)=="count"] = varname
    return(df)
  }
  deaths <- makelong(deaths, "deaths")
  confirmed <- makelong(confirmed, "cases")
  jhu <- merge(deaths, confirmed)
  return(jhu)
}
jhu.all <- jhucoviddata()
# Aggregate to state level
jhu <- aggregate(cases ~ Province_State*date, data=jhu.all, FUN=sum)
jhu <- merge(jhu, aggregate(deaths ~ Province_State*date, data=jhu.all, FUN=sum))
fips <- unique(jhu.all[,c("Province_State","FIPS")])
fips$fips <- fips$FIPS %/% 1000
fips <- fips[fips$fips<=70,]
fips <- fips[!is.na(fips$fips),]
fips <- unique(fips[,c("Province_State","fips")])
jhu <- merge(jhu, fips)
names(jhu)[names(jhu)=="Province_State"]  <- "state"

# Data on state cases, deaths, etc from NYTimes
nyt  <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", stringsAsFactors=FALSE)
nyt$date <- as.Date(nyt$date)


## Check for differences
df <- merge(jhu,nyt, by=c("state","fips","date"), all=TRUE)
# There are some differences in the counts. NYT has no data while states weren't reporting numbers, JHU puts zeros. After numbers appear, they are still occassionally different.
# Deaths differ on 0.25 portion of days with mean absolute difference of 1.9
# Cases differ on 0.59 portion of days wiht mean absolute difference of 36.9
mean(abs(df$deaths.x - df$deaths.y), na.rm=T)
# We will use JHU data for now, but ...

## Covid Tracking project data on tests and hospitalizations
ctp  <- read.csv("https://covidtracking.com/api/v1/states/daily.csv", stringsAsFactors=FALSE)
# Convert date to  correct format
year <- ctp$date %/% 1e4
month <- (ctp$date %% 1e4)  %/% 100
day  <-  (ctp$date %% 1e4) %% 100
date <- as.Date(sprintf("%d-%d-%d", year, month, day))
ctp$date <- date
names(ctp)[names(ctp)=="death"] <- "deaths.ctp"
names(ctp)[names(ctp)=="recovered"] <- "recovered.ctp"
names(ctp)[names(ctp)=="state"] <- "ST"



## Homebase data on businesses with hourly employees
loadhomebase <- function(url) {
  page <- read_html(url)
  rows <- html_nodes(page,"tr")
  first <- TRUE
  for (row in rows) {
    datecells <- html_nodes(row,"td.s5")
    if (length(datecells)>0) {
      dates <- html_text(datecells)
      dates <- paste("2020/",dates,sep="")
      dates <- as.Date(dates,"%Y/%m/%d")
    } else {
      cells <- html_text(html_nodes(row,"td"))
      if (length(cells)==0 || all(cells=="")) next
      else if (sum(cells!="")==1) header=cells[cells!=""]
      else {
        i <- which(cells!="")
        region <- cells[i[1]]
        values <- cells[i[-1]]
        if (length(values)!=length(dates)) stop()
        values <- as.numeric(gsub("%","",values))
        ndf <- data.frame(region=region, date=dates, vals=values, stringsAsFactors=FALSE)
        names(ndf)[names(ndf)=="vals"] <- header
        if (first) {
          hb <- ndf
          first=FALSE
        } else hb <- merge(hb, ndf, on=c("region","date"), all=TRUE)
      }
    }
  }
  return(hb)
}

urls <- c("https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pubhtml/sheet?headers=false&gid=1930671010", "https://docs.google.com/spreadsheets/u/0/d/e/2PACX-1vS6_JK5zktVQr6JwkYUPvzlwcw0YAawSVC7ldWZVfg9hvTjBxl2z4xWaWCrzb9JZ0Go07KhLgbzw5DW/pubhtml/sheet?headers=false&gid=1102464531")
varname <- c("percentchangehours","percentchangebusinesses")
hb <- data.frame(state=c(),date=c())
for (i in 1:length(urls)) {
  url <- urls[i]
  ndf <- loadhomebase(url)
  ndf <- ndf[,c("region","date","Break-down by state")]
  names(ndf) <- c("state","date",varname[i])
  ndf <- ndf[!is.na(ndf[,varname[i]]),]
  summary(ndf)
  if (i==1) hb <- ndf
  else hb <- merge(hb, ndf, on=c("state","date"),all=T)
}

## Google mobility reports
gmr <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", stringsAsFactors=FALSE)
gmr <- subset(gmr, country_region_code=="US")
gmr$date <- as.Date(gmr$date, "%Y-%m-%d")
gmr <- subset(gmr, sub_region_1 != "" & sub_region_2=="") # get state level data
names(gmr)[names(gmr)=="sub_region_1"] <- "state"
drops <- c("country_region_code","country_region","sub_region_2")
gmr <- gmr[, !(names(gmr) %in% drops)]


## Merge data together

# Merge by state name where we must
df <- jhu #
df <- df[df$fips>=1 & df$fips<60,] # drop territories
df <- merge(df, gmr, by=c("state","date"), all=TRUE)
df <- merge(df, hb[hb$state!="Puerto Rico", ], by=c("state","date"), all=TRUE)
stopifnot(sum(is.na(df$fips))==0)
stopifnot(sum(is.na(df$date))==0)
fs <- unique(df[,c("fips","state")])
df$state <- NULL
stopifnot(nrow(unique(df[,c("fips","date")]))==nrow(df))

## but better to merge by fips
names(nyt)[names(nyt)=="cases"]  <- "cases.nyt"
names(nyt)[names(nyt)=="deaths"]  <- "deaths.nyt"
df <- merge(df, nyt[nyt$fips<60,], by=c("fips","date"), all=TRUE)
df <- merge(df, ctp[ctp$fips<60,], by=c("fips","date"), all=TRUE)
df$state <- NULL
df <- merge(df, fs, by="fips", all.x=TRUE)
stopifnot(nrow(unique(df[,c("fips","date")]))==nrow(df))

# missing state names for territories
df$ST <- NULL
fst <- unique(ctp[ctp$fips<60, c("fips","ST")])
df <- merge(df, fst, by="fips", all.x=TRUE)
df <- merge(df, policies, by.x="state",by.y="State")
stopifnot(length(unique(df$state))==51)
stopifnot(length(unique(df$ST))==51)
stopifnot(length(unique(df$fips))==51)
stopifnot(nrow(unique(df[,c("state","ST","fips")]))==51)
stopifnot(nrow(unique(df[,c("fips","date","state","ST")]))==nrow(df))

temp <- read.csv(paste(rootdir,"cases_and_policies/data/Facemask_Mar26_April29.csv",sep="/"))
temp1 <- temp[c("state","fips","z.mask","mask_percent")] 

temp <- read.csv(paste(rootdir,"cases_and_policies/data/1976-2016-president.csv",sep="/"))
temp <- temp[ which(temp$year==2016 & temp$candidate=="Trump, Donald J." & temp$party=="republican" & 
                       temp$writein=="FALSE"), ]
temp$voteshare <- temp$candidatevotes/temp$totalvotes
temp$fips <- temp$state_fips
temp <- temp[c("state","fips","voteshare")]
temp1 <- merge(temp,temp1,by=c("state","fips")) 
 
# from https://github.com/CivilServiceUSA
temp <- read.csv(paste(rootdir,"cases_and_policies/data/us-governors.csv",sep="/"))
temp$state <- temp$state_name 
temp <- temp[c("state","party")]
temp <- merge(temp,temp1,by="state") 
df <- merge(df,temp,by=c("state","fips"))


covidstates <- df

dataguide <- data.frame(name=names(covidstates), description="", source="", stringsAsFactors=FALSE)
for(i in 1:nrow(dataguide)) {
  var <- dataguide$name[i]
  if (var %in% names(jhu)) dataguide$source[i] <- "JHU"
  else if (var %in% names(ctp)) dataguide$source[i] <- "Covid-Tracking Project"
  else if (var %in% names(gmr)) dataguide$source[i] <- "Google Mobility Reports"
  else if (var %in% names(policies)) dataguide$source[i] <- "Raifman et al"
  else if (var %in% names(hb)) dataguide$source[i] <- "Homebase"
  else if (var %in% names(nyt)) dataguide$source[i] <- "NYT"
}


save(covidstates, dataguide, file=paste(rootdir,"cases_and_policies/data/covidstates.Rda",sep="/"))
write.csv(covidstates, paste(rootdir, "cases_and_policies/data/covidstates.csv", sep="/"), row.names=FALSE)
