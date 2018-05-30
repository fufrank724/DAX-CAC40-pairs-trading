#Pair Trading
#Date Process
CAC40 <- read.csv('C:/Users/90812/Desktop/90812/德法指數期貨配對交易策略/data/CAC40(After-Hour).csv',header = TRUE, sep = ",")
DAX <- read.csv('C:/Users/90812/Desktop/90812/德法指數期貨配對交易策略/data/DAX(After-Hour).csv',header = TRUE, sep = ",")
#CAC40[,1] <- as.Date(as.character(CAC40[,1]), "%Y%m%d")
#DAX[,1] <- as.Date(as.character(DAX[,1]), "%Y%m%d")
#write.table(CAC40, file = "CAC40(After-Hour).CSV", sep = ",")
#write.table(DAX, file = "DAX(After-Hour).CSV", sep = ",")

#Compute Spread and Price_Ratio (Based on CAC40 Time)
CAC40 <- CAC40[,c(1,2,6,8)]
DAX <- DAX[,c(1,2,6,8)]
S <- merge(CAC40,DAX,by = 'POSIXct', sort = FALSE)
S$Spread <- S[,7] - S[,4]
S$Price_Ratio <- S[,4]/S[,7]
S <- S[,c(-5,-6)]
names(S)[2]<-paste("Date")
names(S)[3]<-paste("Time")
names(S)[4]<-paste("CAC40")
names(S)[5]<-paste("DAX")
write.table(S, file = "Spread(After-Hour).CSV", sep = ",")

#Plot Spread and Price_Ratio
library(TTR)
S_d <- read.csv('C:/Users/90812/Desktop/90812/德法指數期貨配對交易策略/data/Spread(Day).csv',header = TRUE, sep = ",")
#S_a <- read.csv('C:/Users/90812/Desktop/90812/德法指數期貨配對交易策略/data/Spread(After-Hour).csv',header = TRUE, sep = ",")

#cor.test(S_d[,4],S_d[,5])
#cor.test(S_a[,4],S_a[,5]) #Both found CAC40 and DAX are really High cor

S_d$Date <- as.character(S_d$Date)
S_d$Time <- as.character(S_d$Time)
S_d$POSIXct <- paste(S_d$Date,S_d$Time)
S_d$POSIXct = as.POSIXct(strptime(S_d$POSIXct, "%Y/%m/%d %H:%M:%S"))

S_d$Year <- as.numeric(format(S_d[,1], format="%Y"))
S_d$Month <- as.numeric(format(S_d[,1], format="%m"))
S_d$Day <- as.numeric(format(S_d[,1], format="%d"))
for(i in c(2016,2017)){
  for(j in 1:12){
    for(k in 1:31){
      sub <- subset(S_d,Year == i)
      sub <- subset(sub,Month == j)
      sub <- subset(sub,Day == k)
      if(nrow(sub)!=0){
        sub$MA5 <- SMA(sub$Spread,n = 5)
        sub$MA15 <- SMA(sub$Spread,n = 15)
        sub$MA50 <- SMA(sub$Spread,n = 50)
        name <- sub$Year[1]*10000 + sub$Month[1]*100 + sub$Day[1]
        png(filename=paste(name, ".png", sep = ""),width = 800)
        plot(sub$POSIXct,sub$Spread,type="l",xaxt="n",col="black",main = sub$Date[1],lwd=2)
        lines(sub$POSIXct,sub$MA5,xaxt="n",col="firebrick1",lwd=2)
        lines(sub$POSIXct,sub$MA15,xaxt="n",col="goldenrod1",lwd=2)
        lines(sub$POSIXct,sub$MA50,xaxt="n",col="forestgreen",lwd=2)
        axis.POSIXct(1,at=sub$POSIXct)
        #plot(sub$POSIXct,sub$Price_Ratio,type="l",xaxt="n",col="blue")
        #axis.POSIXct(1,at=sub$POSIXct)
        dev.off()
      }
    }
  }
}

#attach(mtcars)
#par(mfrow=c(2,1)) 
#plot(S_d$POSIXct,S_d$Spread,type="l",xaxt="n",col="red")
#axis.POSIXct(1,at=S_d$POSIXct)
#plot(S_d$POSIXct,S_d$Price_Ratio,type="l",xaxt="n",col="blue")

