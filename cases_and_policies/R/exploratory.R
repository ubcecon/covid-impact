library(ggplot2)
library(ggthemes)
library(xtable)
library(gridExtra)

rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
datafile <- paste(rootdir,"cases_and_policies/data/covidstates.Rda", sep="/")
cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
cat("Run ", paste(rootdir,"cases_and_policies/R/cases_and_policies.R", sep="/"), " to update data.\n")
load(datafile)
source(paste(rootdir,"cases_and_policies/R/utils.R",sep="/"))
outdir <- paste(rootdir,"tex/tables_and_figures", sep="/")

plottheme <- theme_pander() + theme(plot.title=element_text(face="plain"))
#figtheme
colors <-  scale_color_solarized

cs <- covidstates
cs <- cs[cs$fips<60, ] # drop territories (Puerto Rico, Guam, etc)
# cs <- cs[cs$date<=as.Date("2020-06-03"), ] # drop territories (Puerto Rico, Guam, etc)
cs <- cs[order(cs$state, cs$date),]
 
quantileplot <- function(v, p=c(0.05, 0.25, 0.5, 0.75, 0.95), data=cs) {
  data$y <- data[,v]
  adf <- aggregate(y ~ date, data=data,
                   FUN=function(x) quantile(x, probs=p, na.rm=TRUE))
  p <- as.factor(p)
  s <- 1.2
  txt <- data.frame(date=max(adf$date), y=adf$y[nrow(adf$y),], p=p)
  names(adf)[1] <- "date"
  figm <- ggplot(adf) +
    geom_line(aes(x=date, y=adf$y[,1], color=p[1]), size=s) +
    geom_line(aes(x=date, y=adf$y[,2], color=p[2]), size=s) +
    geom_line(aes(x=date, y=adf$y[,3], color=p[3]), size=s) +
    geom_line(aes(x=date, y=adf$y[,4], color=p[4]), size=s) +
    geom_line(aes(x=date, y=adf$y[,5], color=p[5]), size=s) +
    geom_text(data=txt,aes(x=date,y=y,color=p, label=p,
                           hjust="left",vjust="center", size=2)) +
    plottheme +
    colors() + 
    #ggtitle(TeX(paste("Evolution of", varlabels[varlabels$Variable==v,"Label"]))) +
    geom_line(data=data, aes(x=date, y=y, group=state), alpha=0.2) +
    theme(legend.position="none") +
     labs(color='Quantile')  #+
    # xlim(startday, max(data$date))
}

################################################################################
## Case Measures
vars <- c("cases.nyt","cases","positive")
tbl <- cor(cs[,vars],use="complete")
rownames(tbl) <- colnames(tbl) <- c("NYT","JHU","CTP")
cat(print(xtable(tbl, digits=5, caption="Correlation of cumulative cases \\label{tab:casecor}")),
    file=paste(outdir, "casecor.tex", sep="/"))

tbl2 <- matrix(NA,3,3)
for(i in 1:3) {
  for(j in 1:3) {
    tbl2[i,j] <- mean(cs[,vars[i]]==cs[,vars[j]], na.rm=TRUE)
  }
}
rownames(tbl2) <- rownames(tbl)
colnames(tbl2) <- colnames(tbl2)
cat(print(xtable(tbl2, digits=2, caption="Portion of cumulative cases that are equal between data sets\\label{tab:casediff}")),
    file=paste(outdir, "casediff.tex", sep="/"))

cs$newcases.nyt <- paneldiff(cs$cases.nyt, cs$state, cs$date, lag=1)
cs$newcases.jhu <- paneldiff(cs$cases, cs$state, cs$date, lag=1)
cs$newcases.ctp <- paneldiff(cs$positive, cs$state, cs$date, lag=1)

fctp <- ggplot(cs, aes(x=date, y=newcases.ctp, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("CTP") + ylab("New cases") +
  theme(legend.position = "none")
fnyt <- ggplot(cs, aes(x=date, y=newcases.nyt, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("NYT") + ylab("New cases") +
  theme(legend.position = "none")
fjhu <- ggplot(cs, aes(x=date, y=newcases.jhu, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("JHU") + ylab("New cases") +
  theme(legend.position = "none")

pdf(paste(outdir,"newcases.pdf", sep="/"))
grid.arrange(fnyt,fjhu,fctp)
dev.off()

fnyt_daily <- quantileplot("newcases.nyt", data=cs) + ylab("Daily new cases") + ggtitle("NYT Daily New Cases") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))
 
fctp <- quantileplot("newcases.ctp", data=cs) + ylab("New cases") + ggtitle("CTP") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03")) # +  xlim(as.Date("2020-03-10"),Sys.Date())
fnyt <- quantileplot("newcases.nyt", data=cs) + ylab("New cases") + ggtitle("NYT") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))
fjhu <- quantileplot("newcases.jhu", data=cs) + ylab("New cases") + ggtitle("JHU") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))
pdf(paste(outdir,"newcases_q.pdf", sep="/")) 
grid.arrange(fnyt,fjhu,fctp)
dev.off()


X  <- cs[,c("newcases.nyt","newcases.jhu","newcases.ctp")]
X[X<0] <- NA
X[X==0] <- 0.1
cor(log(X), use="complete")
tbl  <- colSums(cs[,c("newcases.nyt","newcases.jhu","newcases.ctp")]<0, na.rm=TRUE)
tbl <- cor(log(X), use="complete")
tbl <- rbind(tbl, diag(cov(log(X), use="complete")))
colnames(tbl) <- c("NYT","JHU","CTP")
rownames(tbl) <- c("NYT","JHU","CTP","Variance")
cat(print(xtable(tbl, digits=2, caption="Correlation and variance of log daily new cases\\label{tab:newcasecor}")),
    file=paste(outdir, "newcasecor.tex", sep="/"))


cs$newcases.nyt <- paneldiff(cs$cases.nyt, cs$state, cs$date, lag=7)
cs$newcases.jhu <- paneldiff(cs$cases, cs$state, cs$date, lag=7)
cs$newcases.ctp <- paneldiff(cs$positive, cs$state, cs$date, lag=7)
X  <- cs[,c("newcases.nyt","newcases.jhu","newcases.ctp")]
X[X<0] <- NA
X[X==0] <- 0.1
cor(log(X), use="complete")
tbl  <- colSums(cs[,c("newcases.nyt","newcases.jhu","newcases.ctp")]<0, na.rm=TRUE)
tbl <- cor(log(X), use="complete")
tbl <- rbind(tbl, diag(cov(log(X), use="complete")))
colnames(tbl) <- c("NYT","JHU","CTP")
rownames(tbl) <- c("NYT","JHU","CTP","Variance")
cat(print(xtable(tbl, digits=2, caption="Correlation and variance of log weekly new cases\\label{tab:weekcasecor}")),
    file=paste(outdir, "weekcasecor.tex", sep="/"))

fctp <- ggplot(cs, aes(x=date, y=newcases.ctp, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("CTP") + ylab("New cases") +
  theme(legend.position = "none")
fnyt <- ggplot(cs, aes(x=date, y=newcases.nyt, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("NYT") + ylab("New cases") +
  theme(legend.position = "none")
fjhu <- ggplot(cs, aes(x=date, y=newcases.jhu, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("JHU") + ylab("New cases") +
  theme(legend.position = "none")

pdf(paste(outdir,"weekcases.pdf", sep="/"))
grid.arrange(fnyt,fjhu,fctp)
dev.off()
 

fnyt_weekly <- quantileplot("newcases.nyt", data=cs) + ylab("Weekly new cases") + ggtitle("NYT Weekly Cases") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))


fctp <- quantileplot("newcases.ctp", data=cs) + ylab("New cases") + ggtitle("CTP") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03")) # +  xlim(as.Date("2020-03-10"),Sys.Date())
fnyt <- quantileplot("newcases.nyt", data=cs) + ylab("New cases") + ggtitle("NYT") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))
fjhu <- quantileplot("newcases.jhu", data=cs) + ylab("New cases") + ggtitle("JHU") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))
pdf(paste(outdir,"weekcases_q.pdf", sep="/")) 
grid.arrange(fnyt,fjhu,fctp)
dev.off()


################################################################################
## Deaths
cs$newdeaths.nyt <- paneldiff(cs$deaths.nyt, cs$state, cs$date, lag=7)
cs$newdeaths.jhu <- paneldiff(cs$deaths, cs$state, cs$date, lag=7)
cs$newdeaths.ctp <- paneldiff(cs$deaths.ctp, cs$state, cs$date, lag=7)
X  <- cs[,c("newdeaths.nyt","newdeaths.jhu","newdeaths.ctp")]
cor(X, use="complete")
tbl  <- colSums(cs[,c("newdeaths.nyt","newdeaths.jhu","newdeaths.ctp")]<0, na.rm=TRUE)
tbl <- cor(X, use="complete")
tbl <- rbind(tbl, diag(cov(X, use="complete")))
colnames(tbl) <- c("NYT","JHU","CTP")
rownames(tbl) <- c("NYT","JHU","CTP","Variance")
cat(print(xtable(tbl, digits=2, caption="Correlation and variance of weekly deaths\\label{tab:weekdeathcor}")),
    file=paste(outdir, "weekdeathcor.tex", sep="/"))

fctp <- ggplot(cs, aes(x=date, y=newdeaths.ctp, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("CTP") + ylab("Deaths") +
  theme(legend.position = "none")
fnyt <- ggplot(cs, aes(x=date, y=newdeaths.nyt, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("NYT") + ylab("Deaths") +
  theme(legend.position = "none")
fjhu <- ggplot(cs, aes(x=date, y=newdeaths.jhu, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("JHU") + ylab("Deaths") +
  theme(legend.position = "none")

pdf(paste(outdir,"weekdeaths.pdf", sep="/"))
grid.arrange(fnyt,fjhu,fctp)
dev.off()

fnyt_death_weekly <- quantileplot("newdeaths.nyt", data=cs) + ylab("Weekly Deaths") + ggtitle("NYT Weekly Deaths") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))

  
fctp <- quantileplot("newdeaths.ctp", data=cs) + ylab("Deaths") + ggtitle("CTP") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03")) # +  xlim(as.Date("2020-03-10"),Sys.Date())
fnyt <- quantileplot("newdeaths.nyt", data=cs) + ylab("Deaths") + ggtitle("NYT") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))
fjhu <- quantileplot("newdeaths.jhu", data=cs) + ylab("Deaths") + ggtitle("JHU") + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))
pdf(paste(outdir,"weekdeaths_q.pdf", sep="/")) 
grid.arrange(fnyt,fjhu,fctp)
dev.off()


## Tests
figc <- ggplot(cs, aes(x=date, y=totalTestResults, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("Total Cumulative Tests") + ylab("Tests") +
  theme(legend.position = "none")
cs$newtests <- paneldiff(cs$totalTestResults, cs$state, cs$date, lag=7)
fign <- ggplot(cs, aes(x=date, y=newtests, colour=state)) + geom_line(alpha=1) +
  xlim(as.Date("2020-03-10"),Sys.Date()) + scale_y_log10() + plottheme +
  ggtitle("New Tests in the Past Week") + ylab("Tests") +
  theme(legend.position = "none")

pdf(paste(outdir,"test.pdf", sep="/"))
grid.arrange(figc,fign)
dev.off()
 
figc <- quantileplot("totalTestResults", data=cs) + ggtitle("Total Cumulative Tests") + ylab("Tests")  + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03")) # +  xlim(as.Date("2020-03-10"),Sys.Date())
fign <- quantileplot("newtests", data=cs) + ggtitle("New Tests in the Past Week") + ylab("Tests")  + scale_y_log10() + xlim(as.Date("2020-03-10"),as.Date("2020-06-03"))
 pdf(paste(outdir,"test_q.pdf", sep="/"))  
 grid.arrange(figc,fign)
dev.off()



pdf(paste(outdir,"casesdeaths_q.pdf", sep="/")) 
grid.arrange(fnyt_daily,fnyt_weekly,fnyt_death_weekly,fign,  ncol=2, nrow=2)
dev.off()


##########################################################################################
## Policies
rvars <- dataguide[dataguide$source=="Raifman et al", "name"]
pols  <- unique(cs[,c("state","fips","ST",rvars)])
di <- sapply(cs[,rvars], function(x) inherits(x,"Date"))
pvars <- rvars[di]

tbl <- cbind(sapply(pols[,pvars], function(x) sum(!is.na(x)), USE.NAMES=FALSE),
             sapply(pols[,pvars], function(x) min(x, na.rm=TRUE), USE.NAMES=FALSE),
             sapply(pols[,pvars], function(x) median(x, na.rm=TRUE), USE.NAMES=FALSE),
             sapply(pols[,pvars], function(x) max(x, na.rm=TRUE), USE.NAMES=FALSE))
tbl[,2:4] <- as.character(as.Date(as.numeric(tbl[,2:4]), origin=as.Date("1970-01-01", format="%Y-%m-%d")))
rownames(tbl) <- gsub("\\."," ",pvars)
colnames(tbl) <- c("N","Min","Median","Max")
cat(print(xtable(tbl, digits=0, caption="State Policies \\label{tab:policies}")),
    file=paste(outdir, "policies.tex", sep="/"))

pvars <- c(#"State.of.emergency",
          "Date.closed.K.12.schools",
          "Stay.at.home..shelter.in.place",
          "Closed.movie.theaters",
          "Closed.restaurants.except.take.out",
          "Closed.non.essential.businesses",
          "Mandate.face.mask.use.by.employees.in.public.facing.businesses")

tbl <- cbind(sapply(pols[,pvars], function(x) sum(!is.na(x)), USE.NAMES=FALSE),
             sapply(pols[,pvars], function(x) min(x, na.rm=TRUE), USE.NAMES=FALSE),
             sapply(pols[,pvars], function(x) median(x, na.rm=TRUE), USE.NAMES=FALSE),
             sapply(pols[,pvars], function(x) max(x, na.rm=TRUE), USE.NAMES=FALSE))
tbl[,2:4] <- as.character(as.Date(as.numeric(tbl[,2:4]), origin=as.Date("1970-01-01", format="%Y-%m-%d")))
rownames(tbl) <- gsub("\\."," ",pvars)
#rownames(tbl)[7] <- "Mandate face mask use by employees"
rownames(tbl)[6] <- "Mandate face mask use by employees"
colnames(tbl) <- c("N","Min","Median","Max")
cat(print(xtable(tbl, digits=0, caption="State Policies \\label{tab:policies_inreg}")),
    file=paste(outdir, "policies_inregs.tex", sep="/"))
