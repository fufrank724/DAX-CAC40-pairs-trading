#Spread Process
library(TTR)
S_d <- read.csv('C:/Users/90812/Desktop/90812/德法指數期貨配對交易策略/data/Spread(Day).csv',header = TRUE, sep = ",")
#S_a <- read.csv('C:/Users/90812/Desktop/90812/德法指數期貨配對交易策略/data/Spread(After-Hour).csv',header = TRUE, sep = ",")
S_d <- subset( S_d, select = -c( 7 ))
S_d$spread_change15 <- ROC(S_d$Spread, n = 15, type = c('discrete')) #Compute Spread change% ,n can be resetted
S_d$spread_change45 <- ROC(S_d$Spread, n = 45, type = c('discrete'))

S_d$Date <- as.character(S_d$Date)
S_d$Time <- as.character(S_d$Time)
S_d$POSIXct <- paste(S_d$Date,S_d$Time)
S_d$POSIXct = as.POSIXct(strptime(S_d$POSIXct, "%Y/%m/%d %H:%M:%S"))

S_d$Year <- as.numeric(format(S_d[,1], format="%Y"))
S_d$Month <- as.numeric(format(S_d[,1], format="%m"))
S_d$Day <- as.numeric(format(S_d[,1], format="%d"))

#plot(S_d$POSIXct,S_d$spread_change15,type="l",xaxt="n",yaxt="n",col="blue",main = "spread_change15") #Plot Spread_change15
#axis.POSIXct(1,at=S_d$POSIXct)
#axis(2,at=seq(-0.1,0.05,0.01))
#abline(h=0,col="red")

#plot(S_d$POSIXct,S_d$spread_change45,type="l",xaxt="n",yaxt="n",col="blue",main = "spread_change45") #Plot Spread_change45
#axis.POSIXct(1,at=S_d$POSIXct)
#axis(2,at=seq(-0.1,0.05,0.01))
#abline(h=0,col="red")

#summary(S_d$spread_change15) #Summary SpreadChange overall
#summary(S_d$spread_change45)

#find where is SpreadChange's local maxima and local minima ,and observe the wave followed
#S_d$SLMin <- runMin(S_d$spread_change15, n = 120)
#S_d$SLMax <- runMax(S_d$spread_change15, n = 120)

#回測參數
num_dax = 1 #等市值部位價差 以DAX40為準
num_cac40 <- 0 #配合DAX40做口數調整
point_dax = 25 #點數價值
point_cac40 = 10
cost_dax = 1 #交易成本
cost_cac40 = 1


#回測紀錄
result <- data.frame(start_time = character(),start_price = numeric(),end_time = character(),end_price = numeric(),ls = numeric())

S_d <- S_d[-c(1:74),] #刪除NULL值

for(i in c(2016,2017)){
  for(j in 1:12){
    for(k in 1:31){

      sub <- subset(S_d,Year == i)
      sub <- subset(sub,Month == j)
      sub <- subset(sub,Day == k)

      if(nrow(sub)!=0){ #作圖(德法價差)

        sub$MA5 <- SMA(sub$Spread,n = 5) #價差均線
        sub$MA15 <- SMA(sub$Spread,n = 15)
        sub$MA50 <- SMA(sub$Spread,n = 50)

        sub$Spread_dn <- BBands(sub$Spread,n = 30,sd = 2)[,1] #布林通道
        sub$Spread_mavg <- BBands(sub$Spread,n = 30,sd = 2)[,2]
        sub$Spread_up <- BBands(sub$Spread,n = 30,sd = 2)[,3]

        sub$SLMin <- runMin(sub$spread_change15, n = 60) #速度變化量極值
        sub$SLMax <- runMax(sub$spread_change15, n = 60)
        
        localmax <- subset(sub,spread_change15 == SLMax)
        localmin <- subset(sub,spread_change15 == SLMin)
        
        # name <- sub$Year[1]*10000 + sub$Month[1]*100 + sub$Day[1]
        # png(filename=paste(name, ".png", sep = ""),width = 800)
        # plot(sub$POSIXct,sub$Spread,type="l",xaxt="n",col="black",main = sub$Date[1],lwd=2)
        # lines(sub$POSIXct,sub$Spread_dn,xaxt="n",col="cornflowerblue",lwd=2)
        # lines(sub$POSIXct,sub$Spread_mavg,xaxt="n",col="brown1",lwd=2)
        # lines(sub$POSIXct,sub$Spread_up,xaxt="n",col="cornflowerblue",lwd=2)
        # axis.POSIXct(1,at=sub$POSIXct)
        # points(localmax$POSIXct,localmax$Spread, col="violet",pch = 19)
        # points(localmin$POSIXct,localmin$Spread, col="darkgoldenrod1",pch = 19)
        # dev.off()
      }
      
      sub <- na.omit(sub) #刪除NA
      ls <- 0 #買賣方向
      init_price <- NA
      
      if(nrow(sub)!=0){
        for(m in 1:nrow(sub)){ #回測
          
          if(ls == 0 && (sub$Spread[m] < sub$Spread_dn[m]) && (sub$spread_change15[m] == sub$SLMin[m])){ #多頭訊號
            
            init_price <- sub$Spread[m] + rnorm(n = 1,mean = 0,sd = 0.30)
            st <- sub$POSIXct[m]
            ls <- 1
            
          }
          
          if(ls == 0 && (sub$Spread[m] > sub$Spread_up[m]) && (sub$spread_change15[m] == sub$SLMax[m])){ #空頭訊號
            
            init_price <- sub$Spread[m] + rnorm(n = 1,mean = 0,sd = 0.30)
            st <- sub$POSIXct[m]
            ls <- -1
            
          }
          
          if(is.na(init_price) != TRUE && ls != 0){
            
            payoff <- ls*(sub$Spread[m] - init_price) #即時損益
            
            if(identical(sub[m,],tail(sub,n=1)) == TRUE){ #當沖平倉
              
              term_price <- sub$Spread[m] + rnorm(n = 1,mean = 0,sd = 0.30)
              result <- rbind(result,data.frame(start_time = st,start_price = init_price,end_time = sub$POSIXct[m],end_price = term_price,ls = ls))
              ls <- 0
              payoff <- 0
              init_price <- NA
              
            }
            
            if(ls == 1){
              
              if(sub$Spread[m] >= sub$Spread_up[m]){ #出場訊號
                
                term_price <- sub$Spread[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                result <- rbind(result,data.frame(start_time = st,start_price = init_price,end_time = sub$POSIXct[m],end_price = term_price,ls = ls))
                ls <- 0
                payoff <- 0
                init_price <- NA
                
              }else if(payoff <= -10){ #停損
                
                term_price <- sub$Spread[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                result <- rbind(result,data.frame(start_time = st,start_price = init_price,end_time = sub$POSIXct[m],end_price = term_price,ls = ls))
                ls <- 0
                payoff <- 0
                init_price <- NA
                
              }
            }else if(ls == -1){
              
              if(sub$Spread[m] <= sub$Spread_dn[m]){ #出場訊號
                
                term_price <- sub$Spread[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                result <- rbind(result,data.frame(start_time = st,start_price = init_price,end_time = sub$POSIXct[m],end_price = term_price,ls = ls))
                ls <- 0
                payoff <- 0
                init_price <- NA
                
              }else if(payoff <= -10){ #停損
                
                term_price <- sub$Spread[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                result <- rbind(result,data.frame(start_time = st,start_price = init_price,end_time = sub$POSIXct[m],end_price = term_price,ls = ls))
                ls <- 0
                payoff <- 0
                init_price <- NA
                
              }
            }
          }
        }
      }
      
    }
  }
}

result$Payoff <- (result$end_price - result$start_price)*result$ls

#逐筆交易紀錄
print('整體累積收益')
sum(result$Payoff)
print('單筆平均收益')
mean(result$Payoff)
print('總交易筆數')
nrow(result)
print('交易勝率')
nrow(result[result$Payoff>0,])/nrow(result)

#收益曲線
plot(as.Date(result$start_time),result$Payoff,type = 'l',main = '單筆點數收益')
plot(as.Date(result$start_time),cumsum(result$Payoff),type = 'l',main = '累積點數收益')


#筆記
#計算合約目前市值再決定匹配口數
#以利報告呈現
