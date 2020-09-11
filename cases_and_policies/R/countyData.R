## Data on state cases, deaths, etc from NYTimes
nyt  <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", stringsAsFactors=FALSE)
nyt$date <- as.Date(nyt$date)


## County Mask Mandate info from Wright, Chawla, Chen, & Farmer
library(readxl)
if (!(file.exists("mm.xlsx")) {
  download.file("https://drive.google.com/uc?export=download&id=1qVIhPaBQ-apdDjOaKV2eA9SgZNkLMLAm",destfile="mm.xlsx")
} else {
  warning("mm.xlsx exists. Not redownloading mask mandate data.")
}

mask <- data.frame(read_excel("mm.xlsx", col_names=FALSE))
names(mask) <- c("state.fips", # 1
                 "state",      # 2
                 "fips",       # 3
                 "county",     # 4
                 "start_county", # 5
                 "end_county",   # 6
                 "scope_county", # 7
                 "source_county", #8
                 "note1_county", # 9
                 "note2_county", # 10
                 "code1", # 11  (this is some stata encoding of a date, I think)
                 "start_state", # 12
                 "end_state",  # 13
                 "scope_state", # 14
                 "source_state", # 15
                 "code2",
                 "code3",
                 "code4",
                 "start_mask") # min(start_county, start_state)

for (v in grep("start|end",names(mask))) {
  # let's enter dates in 3 different formats for some reason ...
  if (is.character(mask[,v])) {
    if (any(grepl("/",mask[,v])))
      mask[,v] <- as.Date(mask[,v],format="%m/%d/%y")
    else if (any(grepl("-",mask[,v])))
      mask[,v] <- as.Date(mask[,v],format="%m-%d-%y")
  } else
    mask[,v] <- as.Date(mask[,v])
}
stopifnot(all(pmin(mask$start_county, mask$start_state) == mask$start_mask, na.rm=TRUE))

## Corrections and additions to mask data

unique(mask$state[!is.na(mask$end_state)])
# only AL, CO, DC, KY, LA, MS, NC, & RI  have end dates. I checked on
# 2020-09-10, and all state orders were extended at least to the present.


unique(mask$state[is.na(mask$start_state)])
# Check against
# https://www.aarp.org/health/healthy-living/info-2020/states-mask-mandates-coronavirus.html
# Following mandates added more recently
mask[mask$state=="Mississippi","start_state"] <- as.Date("2020-08-04")
mask[mask$state=="Montana","start_state"] <- as.Date("2020-07-15") # https://covid19.mt.gov/Portals/223/Documents/Mask%20Directive%20FINAL.pdf?ver=2020-07-15-140109-633
mask[mask$state=="Vermont","start_state"] <- as.Date("2020-08-01")
mask[mask$state=="Wisconsin","start_state"] <- as.Date("2020-08-01")
# Texas mandated on July 2, but counties with < 20 cases can get
# exemption. County level dates appear to be accurate.
mask$start_mask <- pmin(mask$start_county, mask$start_state, na.rm=TRUE)

mask <- mask[,!(names(mask) %in% c("state","code1","code2","code3","code4"))]
################################################################################
df <- merge(mask, nyt, by=c("fips"), all=TRUE)


library(plm)
df$t <- as.numeric(df$date)
df <- df[order(df$fips, df$date),]
df <- pdata.frame(df,index=c("fips","t"), stringsAsFactors=FALSE)
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
source(paste(rootdir,"cases_and_policies/R/utils.R", sep="/"))
source(paste(rootdir,"cases_and_policies/rmd/varlabels.R", sep="/"))


dlogd <- function(x, id, t, lag=L, minval=0.0, b=0, g=0) {
  dx  <- paneldiff(x, id, t, lag=lag)
  dx[dx<minval] <- minval
  dlogdx <- log(dx + exp(g)*b) - panellag(log(dx+b),id,t,lag=lag)
  dlogdx[!is.finite(dlogdx)] <- NA
  return(dlogdx)
}
L  <- 7
df$dcases <- paneldiff(df$cases,df$fips, df$date, lag=L)
df$ddeath <- paneldiff(df$deaths,df$fips, df$date, lag=L)
df$logdc <- log(sapply(df$dcases, function(x) max(x, exp(-1))))

df$dlogdc <- dlogd(df$cases, df$fips, df$date, lag=L,
                   minval=exp(-1))

df$dlogdd <-  dlogd(df$deaths, df$fips, df$date, lag=L,
                    minval=exp(-1))

df$logdd <- log(sapply(df$ddeath, function(x) max(x, exp(-1))))
enddates <- c(NA)

pdates <- c("start_mask")

stopifnot(length(enddates)==length(pdates))

pvars <- c("pmask")
for(i in 1:length(pdates)) df[,pvars[i]] <-
                             smoothedpolicy(df,pdates[i],enddatevar=enddates[i],type="ma",bw=6, id="fips")

pfigure <- function(df, pol, stlab=1, outcome="dlogdc", L=14) {
  df$p <- panellag(df[,pol],df$fips, df$date, L)
  df$p <- ceiling(df$p)
  df$y <- df[,outcome]
  #df$week <- week(df$date)
  m <- lm(y ~ as.factor(date) + as.factor(date):p, data=df)
  adf <- rbind(data.frame(date=m$xlevels$`as.factor(date)`, p=0),
               data.frame(date=m$xlevels$`as.factor(date)`, p=1))
  #adf$week <- week(as.Date(adf$date))
  pred <- predict(m, newdata=adf, interval="confidence")
  adf$hat <- pred[,"fit"]
  adf$lo <- pred[,"lwr"]
  adf$hi <- pred[,"upr"]
  adf$date <- as.Date(adf$date)
  if (stlab==1) {
    pdf <- subset(df, df$p<=0.01)
    ldf <- subset(df, df$p>0.01)
  } else if (stlab==0) {
    pdf <- subset(df, df$p>=0.99)
    ldf <- subset(df, df$p<0.99)
  } else {
    pdf <- df
    ldf <- NULL #data.frame(date=NA,y=NA,ST=NA,p=NA)
  }
  clrs <- solarized_pal(2)(2)
  fig <- ggplot(df,aes(color=p)) +
    geom_point(data=pdf,aes(x=date, y=y, color=p),alpha=0.3, size=0.5) +
    xlim(c(as.Date("2020-04-10"), max(df$date))) +
    geom_line(data=adf, aes(x=date, y=hat, group=p), size=2) +
    figtheme + scale_color_gradient(low=clrs[1], high=clrs[2]) + #colors_grad(palette="Green-Gold") +
    theme(legend.position="none") +
    xlab("") +
    ylim(c(-0.65,0.4))
  if (!is.null(ldf))
    fig <- fig +
      geom_text(data=ldf,
                aes(x=date, y=y, label=ST, color=p), alpha=1.0, size=5)
  return(fig)
}
library(latex2exp)
fig <- pfigure(subset(df, df$date>="2020-03-01"), "pmask", stlab=-1,L=14) + ggtitle(TeX(relabel("dlogdc given pmask"))) + ylab(TeX(relabel("dlogdc")))

p <- "pmask"
#figdev(sprintf("%s/tex/tables_and_figures/%s-cases-%s.pdf", rootdir, p, L.c), width=4, height=3)
#print(fig)
#dev.off()
fig


