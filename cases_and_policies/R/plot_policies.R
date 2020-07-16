library(ggplot2)
library(gridExtra)
load(file="../data/covidstates.Rda")
## Some summary plots to check
df  <- covidstates
plotstatepolicies  <- function(st) {
  sdf  <- subset(df, state==st)
  #sdf$cases  <- pmax(sdf$cases, 1) # to avoid log(0)
  pols <- c("State.of.emergency",
            "Date.closed.K.12.schools",
            "Closed.day.cares",
            "Date.banned.visitors.to.nursing.homes",
            "Stay.at.home..shelter.in.place",
            "Closed.non.essential.businesses",
            "Closed.restaurants.except.take.out",
            "Closed.gyms",
            "Closed.movie.theaters",
            "Order.freezing.utility.shut.offs",
            "Froze.mortgage.payments"
            )
  x  <- c()
  labels <- c()
  for (lbl in pols) {
    d <- unique(sdf[,lbl])
    i <- which(x==d)
    if (length(i)==0) {
      if (length(x)==0)
        x <- d
      else
        x <- c(x,d)
      labels <- c(labels,lbl)
    } else {
      labels[i]  <- paste(labels[i],lbl,sep=", ")
    }
  }
  labels <- labels[!is.na(x)]
  x <- x[!is.na(x)]
  y <- rep(1, length(x))
  fig <- ggplot(sdf,aes(x=date))  +  theme_minimal() +
    geom_line(aes(y=cases)) +
    geom_line(aes(y=deaths), colour="red") +
    xlim(as.Date("2020-03-01"),max(sdf$date)) +
    ylab("count") +
    scale_y_continuous(trans="log10") +
    ggtitle(sprintf("Cases, deaths, and policies in %s",st))

  fig + annotate("text", x=x,y=y,label=gsub("\\."," ",labels),
             size=2, angle=90, hjust=0)

}

fig <- list()
for (st in c("Washington","New York","Florida","Alabama","Wyoming","California"))
  fig[[st]] <- plotstatepolicies(st)
pdf("state_policies.pdf", width=14,height=28)
grid.arrange(grobs=fig, nrow=length(fig))
dev.off()
