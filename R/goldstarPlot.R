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

for(i in stkWeeks){
    tmp <- sum(stk$words[stkWeeks==i])

    if(tmp>300){
        hits[weeks==i] <- TRUE
        star[weeks==i] <- "goldenrod4"
        
    if(tmp>1000){
        star[weeks==i]<- "grey87"
    }
        
    if(tmp>2000){
        star[weeks==i] <- "gold2"
    }
    }
}

   
png("./streakFig.png",width=900,height=600)

layout(matrix(1:60,nrow=6,ncol=10,byrow=TRUE))


par(mar=c(0,0,1,0),xpd=FALSE)
for(i in 1:60){

    if(i%in%wks){

        
        plot(0,0,ylim=c(-1,1),xlim=c(-1,1),fg="white",xaxt="n",yaxt="n",
             xlab="",ylab="",main=paste0("Week ",weeks[i]),col="white")

        rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],
             col=ifelse(weeks[i]==thisweek,"lightcyan","white"))
        
        if(hits[i]){

            symbols(0,0,fg=ifelse(star[i]=="gold2","grey60","gold4"),bg=star[i]
                   ,stars=rbind(rep(c(2,1,1),5)),add=TRUE,
                    inches=.35)
        } else {


            points(0,0,col="red",pch=4,cex=5)

        }
    } else {

             plot(0,0,ylim=c(-1,1),xlim=c(-1,1),fg="white",xaxt="n",yaxt="n",
                  xlab="",ylab="",main="",col="white")


    
    if(i==55) {

        par(xpd=NA)

        legend("top",col=c("goldenrod4","grey87","gold2"),
                    pch=16,legend=c("> 300 words","> 1000 words","> 2000 words")
                   ,bty="n",cex=2.5) }

    }
}
        par(xpd=TRUE)

dev.off()
