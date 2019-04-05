################################################################################
## Writing Streak
## Every week when you write
## > 300 words = bronze star (write an abstract)
## > 1000 words = silver star (write a paper section)
## > 2000 words = gold star (write more than one paper section)
################################################################################

setwd("/home/marcov/Dropbox/Backup/Work/git/ESWP")

## 52 weeks of the past year
today <- format(Sys.time(), "%Y-%m-%d")
thisweek <- as.numeric(format(Sys.time(), "%V"))

## center weeks on this week
wks <- seq(1,52) 
weeks <- c(((wks[((thisweek+1):52)])),wks[-((thisweek+1):52)])


## a year back
d <- as.POSIXlt(as.Date(today))
d$year <- d$year-1
d <- as.Date(d)

#layout.show(60)

## get streak data
stk <- read.csv("./StreakWeeklyWords.csv")
stkDates <- as.Date(stk$Day,format="%d-%m-%Y")

stk <- stk[stkDates>d,]
stkDates <- stkDates[stkDates>d]
stkWeeks <- as.numeric(format(stkDates,format="%V"))

hits <- star <- numeric(52)
hits[weeks%in%stkWeeks] <- stk$words>300
star[weeks%in%stkWeeks][stk$words>300] <- "goldenrod4"
star[weeks%in%stkWeeks][stk$words>1000] <- "grey87"
star[weeks%in%stkWeeks][stk$words>1000] <- "gold2"

 
png("./streakFig.png",width=800,height=600)

layout(matrix(1:60,nrow=6,ncol=10,byrow=TRUE))


par(mar=c(0,0,1,0))
for(i in 1:60){

    if(i%in%wks){

        plot(0,0,ylim=c(-1,1),xlim=c(-1,1),fg="white",xaxt="n",yaxt="n",
             xlab="",ylab="",main=weeks[i],col="white")

        if(hits[i]){

            symbols(0,0,fg="gold",bg=star[i]
                   ,stars=rbind(rep(c(2,1,1),5)),add=TRUE,
                    inches=.25)
        }
    }

}

dev.off()
