#Spread Process
library(TTR)
library(zoo)
S_d <- read.csv('C:/Users/90812/Desktop/90812/德法指數期貨配對交易策略/data/Spread(Day).csv',header = TRUE, sep = ",")
#S_a <- read.csv('C:/Users/90812/Desktop/90812/德法指數期貨配對交易策略/data/Spread(After-Hour).csv',header = TRUE, sep = ",")
S_d <- subset( S_d, select = -c( 7 ))
S_d$Spread <- S_d$CAC40 * 2.5 - S_d$DAX
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
result <- data.frame(start_time = character(),start_price_DAX = numeric(),start_price_CAC40 = numeric(),end_time = character(),end_price_DAX = numeric(),end_price_CAC40 = numeric(),ls = numeric(),n1 = numeric(),n2 = numeric())

S_d <- na.omit(S_d)

for(i in c(2016,2017)){
  for(j in 1:12){
    for(k in 1:31){
      
      sub <- subset(S_d,Year == i)
      sub <- subset(sub,Month == j)
      sub <- subset(sub,Day == k)
      
      if(nrow(sub)!=0){ #作圖(德法價差)
        
        #sub$MA5 <- SMA(sub$Spread,n = 5) #價差均線
        #sub$MA15 <- SMA(sub$Spread,n = 15)
        #sub$MA50 <- SMA(sub$Spread,n = 50)
        
        sub$Spread_dn <- BBands(sub$Spread,n = 30,sd = 2)[,1] #布林通道
        sub$Spread_mavg <- BBands(sub$Spread,n = 30,sd = 2)[,2]
        sub$Spread_up <- BBands(sub$Spread,n = 30,sd = 2)[,3]
        
        sub$SLMin <- runMin(sub$spread_change15, n = 60) #速度變化量極值
        sub$SLMax <- runMax(sub$spread_change15, n = 60)
        
        #localmax <- subset(sub,spread_change15 == SLMax)
        #localmin <- subset(sub,spread_change15 == SLMin)
        
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
      init_price_dax <- NA
      init_price_cac40 <- NA
      
      if(nrow(sub)!=0){
        for(m in 1:nrow(sub)){ #回測
          
          if(ls == 0 && (sub$Spread[m] < sub$Spread_dn[m]) && (sub$spread_change15[m] == sub$SLMin[m])){ #多頭訊號
            
            init_price_dax <- sub$DAX[m] + rnorm(n = 1,mean = 0,sd = 0.30)
            init_price_cac40 <- sub$CAC40[m] + rnorm(n = 1,mean = 0,sd = 0.30)
            st <- sub$POSIXct[m]
            num_cac40 <- point_dax*num_dax*init_price_dax/point_cac40/init_price_cac40
            ls <- 1
            
          }
          
          if(ls == 0 && (sub$Spread[m] > sub$Spread_up[m]) && (sub$spread_change15[m] == sub$SLMax[m])){ #空頭訊號
            
            init_price_dax <- sub$DAX[m] + rnorm(n = 1,mean = 0,sd = 0.30)
            init_price_cac40 <- sub$CAC40[m] + rnorm(n = 1,mean = 0,sd = 0.30)
            st <- sub$POSIXct[m]
            num_cac40 <- point_dax*num_dax*init_price_dax/point_cac40/init_price_cac40
            ls <- -1
            
          }
          
          if(is.na(init_price_dax) != TRUE && is.na(init_price_cac40) != TRUE && ls != 0){
            
            payoff <- ls*(point_cac40*num_cac40*(sub$CAC40[m] - init_price_cac40) - point_dax*num_dax*(sub$DAX[m] - init_price_dax)) - 2*cost_dax - 2*cost_cac40 #即時損益
            
            if(identical(sub[m,],tail(sub,n=1)) == TRUE){ #當沖平倉
              
              term_price_dax <- sub$DAX[m] + rnorm(n = 1,mean = 0,sd = 0.30)
              term_price_cac40 <- sub$CAC40[m] + rnorm(n = 1,mean = 0,sd = 0.30)
              result <- rbind(result,data.frame(start_time = st,start_price_DAX = init_price_dax,start_price_CAC40 = init_price_cac40,end_time = sub$POSIXct[m],end_price_DAX = term_price_dax,end_price_CAC40 = term_price_cac40,ls = ls,n1 = num_dax,n2 = num_cac40))
              ls <- 0
              payoff <- 0
              num_cac40 <- 0
              init_price_dax <- NA
              init_price_cac40 <- NA
              
            }
            
            if(ls == 1){
              
              if(sub$Spread[m] >= sub$Spread_up[m]){ #出場訊號
                
                term_price_dax <- sub$DAX[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                term_price_cac40 <- sub$CAC40[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                result <- rbind(result,data.frame(start_time = st,start_price_DAX = init_price_dax,start_price_CAC40 = init_price_cac40,end_time = sub$POSIXct[m],end_price_DAX = term_price_dax,end_price_CAC40 = term_price_cac40,ls = ls,n1 = num_dax,n2 = num_cac40))
                ls <- 0
                payoff <- 0
                num_cac40 <- 0
                init_price_dax <- NA
                init_price_cac40 <- NA
                
              }else if(payoff <= -30.0){ #停損
                
                term_price_dax <- sub$DAX[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                term_price_cac40 <- sub$CAC40[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                result <- rbind(result,data.frame(start_time = st,start_price_DAX = init_price_dax,start_price_CAC40 = init_price_cac40,end_time = sub$POSIXct[m],end_price_DAX = term_price_dax,end_price_CAC40 = term_price_cac40,ls = ls,n1 = num_dax,n2 = num_cac40))
                ls <- 0
                payoff <- 0
                num_cac40 <- 0
                init_price_dax <- NA
                init_price_cac40 <- NA
                
              }
            }else if(ls == -1){
              
              if(sub$Spread[m] <= sub$Spread_dn[m]){ #出場訊號
                
                term_price_dax <- sub$DAX[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                term_price_cac40 <- sub$CAC40[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                result <- rbind(result,data.frame(start_time = st,start_price_DAX = init_price_dax,start_price_CAC40 = init_price_cac40,end_time = sub$POSIXct[m],end_price_DAX = term_price_dax,end_price_CAC40 = term_price_cac40,ls = ls,n1 = num_dax,n2 = num_cac40))
                ls <- 0
                payoff <- 0
                num_cac40 <- 0
                init_price_dax <- NA
                init_price_cac40 <- NA
                
              }else if(payoff <= -30.0){ #停損
                
                term_price_dax <- sub$DAX[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                term_price_cac40 <- sub$CAC40[m] + rnorm(n = 1,mean = 0,sd = 0.30)
                result <- rbind(result,data.frame(start_time = st,start_price_DAX = init_price_dax,start_price_CAC40 = init_price_cac40,end_time = sub$POSIXct[m],end_price_DAX = term_price_dax,end_price_CAC40 = term_price_cac40,ls = ls,n1 = num_dax,n2 = num_cac40))
                ls <- 0
                payoff <- 0
                num_cac40 <- 0
                init_price_dax <- NA
                init_price_cac40 <- NA
                
              }
            }
          }
        }
      }
      
    }
  }
}

result$Payoff <- result$ls*(point_cac40*result$n2*(result$end_price_CAC40 - result$start_price_CAC40) - point_dax*result$n1*(result$end_price_DAX - result$start_price_DAX)) - 2*cost_dax - 2*cost_cac40

performance <- function(a){
  
  print('累積淨利(歐元)')
  print(sum(a$Payoff))
  
  print('單筆平均淨利(歐元)')
  print(mean(a$Payoff))
  
  print('總交易筆數(次)')
  print(nrow(a))
  
  print('交易勝率(%)')
  print(nrow(a[a$Payoff>0,])/nrow(a))
  
  a$DD = rep(0, length(a$Payoff))
  topprofit = rep(a$Payoff[1], length(a$Payoff))
  for (m in 2:length(a$Payoff)){
    if (sum(a$Payoff[1:m]) > topprofit[m-1]){   
      topprofit[m:length(a$Payoff)]=sum(a$Payoff[1:m])  
    }
    a$DD[m]=sum(a$Payoff[1:m])-topprofit[m] 
  }
  print('最大策略虧損')
  print(min(a$DD))
  
  print('最大平倉虧損(歐元)')
  print(min(a$Payoff))
  
  print('平均獲利/平均虧損')
  print(mean(a[a$Payoff>0,]$Payoff)/abs(mean(a[a$Payoff<0,]$Payoff)))
  
  plot(as.Date(a$start_time),a$Payoff,xlab = '時間',ylab = '淨利',type = 'h',col = 'firebrick4',main = '逐筆淨利(歐元)')
  abline(h=0,col="forestgreen")
  
  plot(as.Date(a$start_time),cumsum(a$Payoff),xlab = '時間',ylab = '淨利',type = 'l',col = 'firebrick4',main = '累積淨利(歐元)')
  lines(as.Date(a$start_time),a$DD,type = 'l',col = 'dodgerblue4')
  abline(h=0,col="forestgreen")
  
  a$Year <- as.numeric(format(a$start_time, format="%Y"))
  a$Month <- as.numeric(format(a$start_time, format="%m"))
  monthly_return <- data.frame(YYYYMM = character(),return = numeric())
  
  for(i in a$Year[1]:a$Year[length(a$Year)]){
    for(j in 1:12){
      s <- a[(a$Year == i),]
      s <- s[(s$Month == j),]
      YM <- as.yearmon(s$start_time)
      if(nrow(s) != 0){
        monthly_return  <- rbind(monthly_return,data.frame(YYYYMM = YM,return = sum(s$Payoff)))
      }
    }
  }
  plot(monthly_return$YYYYMM,monthly_return$return,xlab = '時間',ylab = '淨利',type = 'h',col = 'firebrick4',lwd = 5,main = '每月獲利和虧損')
  abline(h=0,col="forestgreen")
  
}

performance(result)

#順勢
u <- result[result$ls == 1,]
performance(u)

#逆勢
d <- result[result$ls == -1,]
performance(d)
