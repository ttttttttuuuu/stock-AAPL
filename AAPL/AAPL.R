###AAPL疫情期間的股價分析

library(quantmod)
#取得股票
getSymbols("AAPL")
#畫圖-疫情期間
AAPL_COVID19<-AAPL["2020-03-01::2020-06-16",]
chartSeries(AAPL_COVID19)

#moving average
ma5<-runMean(AAPL_COVID19[,4],n=5)
ma20<-runMean(AAPL_COVID19[,4],n=20)
ma60<-runMean(AAPL_COVID19[,4],n=60)
addTA(ma5,on=1,col="blue")
addTA(ma20,on=1,col="red")
addTA(ma60,on=1,col="green")

#BBands
chartSeries(AAPL_COVID19)
addBBands()

###AAPL疫情期間的股價預測
#AR
getSymbols("AAPL",from = "2007-01-03",to = "2020-03-01")
myStock <- AAPL
#上市以來蘋果的收盤價
myStock <- as.data.frame(myStock)
myStock$Date <- rownames(myStock)
myStock$Date <- as.Date(myStock$Date )
ggplot(data = myStock,aes(x=Date, y=AAPL.Close))+ geom_line()

vec <-  c()
for(p in c(1:20)){
  mod <- ar.ols(myStock$AAPL.Close,order = p)
  #AIC
  aka <- log(sum(mod$resid^2, na.rm = T)/mod$n.used)+ 2*(p+1)/mod$n.used
  print(aka)
  #BIC
  sch <- log(sum(mod$resid^2, na.rm = T)/mod$n.used)+ (p+1)*log(mod$n.used)/mod$n.used 
  print(sch)
  vec <- c(vec, as.integer(p), aka, sch)
}


info<-data.frame(matrix(vec, nrow=20, ncol = 3, byrow = T))
colnames(info) <- c('Lagged Period','AIC','BIC')  
matplot(info[,c(2:3)], type = c("l"), pch=1,col=1:2)  
legend("topleft", legend = c("AIC","BIC"),col=1:2,pch=1)
model <- ar.ols(myStock$AAPL.Close,order=which.min(info$AIC))
date_seq = seq(as.Date('2020-03-01'),as.Date('2020-03-01')+89, by = 1)
dat <- data.frame(Date=as.Date(date_seq),
                  AAPL.Close = as.numeric(predict(mod, n.ahead = 90)$pred),
                  group = 'pred',
                  stringsAsFactors = F)

AAPL_dat<- myStock[,c('Date','AAPL.Close')]
AAPL_dat$group = 'orginal'
AAPL_dat <- rbind(dat, AAPL_dat)

ggplot(data = AAPL_dat)+
  geom_line(aes(x=Date,y=AAPL.Close, colour=group),size = 1.2)+
  labs(title='AAPL AR Model Prediction')

###探討公司財報與營收對於股價之影響
library(quantmod)
getSymbols("AAPL")
myStock <- AAPL
myStock= as.data.frame(to.quarterly(AAPL))
rownames(myStock) = gsub(" Q1","-01-01",rownames(myStock))
rownames(myStock) = gsub(" Q2","-04-01",rownames(myStock))
rownames(myStock) = gsub(" Q3","-07-01",rownames(myStock))
rownames(myStock) = gsub(" Q4","-10-01",rownames(myStock))
date <- as.Date(rownames(myStock))
myStock <- xts(myStock, date)

require(TTR)
#install.packages("randomForest")
myTTR <- function(data){
  names(data) <- sapply(X=names(data), FUN=function(x) strsplit(x, split=".", fixed=TRUE)[[1]][2])
  #HLC : Object that is coercible to xts or matrix and contains High-Low-Close prices
  myATR <- ATR(HLC(data))$atr #average true range 
  mySMI <- SMI(HLC(data))$SMI #The SMI relates the close to the midpoint of the high/low range
  myADX <- ADX(HLC(data))$ADX #Directional Movement Index
  myAroon <- aroon(HLC(data)[,-3])$oscillator #AROON指標
  myBBands <- BBands(HLC(data))$pctB #Bollinger Bands are a way to compare a security’s volatility and price levels over a period of time.
  myChaikin <- Delt(chaikinVolatility(HLC(data)[,-3]))[,1] #A object of the same class as HLC and volume or a vector (if try.xts fails) containing the accumulation / distribution values.
  #The Close Location Value (CLV) relates the day’s close to its trading range.
  myCLV <- EMA(CLV(HLC(data)))[,1] #Exponential moving average.
  myMACD <- MACD(data[,"Close"])[,2] #signal
  myMFI <- MFI(HLC(data), data[,"Volume"])
  mySAR <- SAR(data[,c("High", "Close")])[,1]
  
  result <- cbind(myATR, mySMI, myADX, myAroon, myBBands, myChaikin, myCLV, myMACD, myMFI, mySAR)
  colnames(result) <- cbind("myATR", "mySMI", "myADX", "myAroon", "myBBands", "myChaikin", "myCLV", "myMACD", "myMFI", "mySAR") 
  return(result) 
  View(result)
}

mystockTTR<-as.data.frame(myTTR(myStock))
mystockTTR$Date <- rownames(mystockTTR)

library(jsonlite)
appleUrl<-"C:/Users/user/Downloads/248,241,750,243,11503,251,11504,250,1788,264,265,1707,238,239,240,242"
apple<-fromJSON(appleUrl)

a1<-data.frame(apple$data$`c:248`$s[1])
a2<-data.frame(apple$data$`c:248`$s[2])
a2<-merge(a1,a2,by="X1",all=T)

TotalRevenue<-data.frame(Date=a2$X1,
                         TotalRevenue=a2$X2.x,
                         AnnualGrowthRate=a2$X2.y)

mystockTTR<-merge(mystockTTR,TotalRevenue, by = "Date", all = T)

a2<-data.frame(apple$data$`c:1788`$s)
a2_1<-a2[,1:22]
a2_1<-a2_1[-c(2:4),]
a2_1<-t(a2_1)
a2_2<-a2[,23:44]
a_final<-as.data.frame(t(a2_2))
colnames(a_final) <- c("GrossMargin","OperatingRate","NetInterestRate","EarningsPerShare")
rownames(a_final) <- c(1:22)
a_final$Date<-a2_1
a_final<-data.frame(a_final)
mystockTTR<-merge(mystockTTR,a_final, by = "Date", all = T)

myStock<-as.data.frame(myStock)
myStock$Date<-rownames(myStock)
mystockTTR<-merge(mystockTTR,myStock, by = "Date", all = T)

test<-mystockTTR[34:54,]
test$TotalRevenue<-as.double(test$TotalRevenue)
test$GrossMargin<-as.double(test$GrossMargin)
test$OperatingRate<-as.double(test$OperatingRate)
test$NetInterestRate<-as.double(test$NetInterestRate)
test$EarningsPerShare<-as.double(test$EarningsPerShare)


AAPL_TotalRevenue<-ggplot(test, aes(x = Date, y = TotalRevenue, group = 1)) + geom_line() +theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
AAPL_TotalRevenue
AAPL_GrossMargin<-ggplot(test, aes(x = Date, y = GrossMargin, group = 1)) + geom_line() +theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
AAPL_GrossMargin
AAPL_OperatingRate<-ggplot(test, aes(x = Date, y = OperatingRate, group = 1)) + geom_line() +theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
AAPL_OperatingRate
AAPL_EarningsPerShare<-ggplot(test, aes(x = Date, y = EarningsPerShare, group = 1)) + geom_line() +theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
AAPL_EarningsPerShare

###################
b3<-lm(`AAPL.Close`~GrossMargin+OperatingRate+NetInterestRate+EarningsPerShare, data =test, na.rm=F)
summary(b3)



###training_stock
###訓練-股價預測模型
##step1模型建立

library(quantmod)
library(ggplot2)
getSymbols("AAPL")
myStock <- AAPL
#定義功能以衡量績效

colnames(myStock) = paste("myStock",c("Open","High","Low","Close","Volume","Adjusted"), sep=".")
p <- 0.025
k <- 10
T.index <- function(data,p,k){
  require(quantmod)
  hlc = HLC(data)
  P <- rowMeans(hlc)
  V <- matrix(NA, ncol=k, nrow=NROW(P))
  for (i in 1:k) {
    V[,i] <- Next(Delt(P,k=i),k=i)
  }
  T <- apply(V,1,function(x) sum(x,na.rm=TRUE))
  T <- xts(x=T, order.by=time(data))
  return(T)
}
#預測指標

myTTR <- function(data){
  require(TTR)
  install.packages("randomForest")
  #require(quantmod)
  names(data) <- sapply(X=names(data), FUN=function(x) strsplit(x, split=".", fixed=TRUE)[[1]][2]) # change [2] 
  myATR <- ATR(HLC(data))$atr
  mySMI <- SMI(HLC(data))$SMI
  myADX <- ADX(HLC(data))$ADX
  myAroon <- aroon(HLC(data)[,-3])$oscillator
  myBBands <- BBands(HLC(data))$pctB
  myChaikin <- Delt(chaikinVolatility(HLC(data)[,-3]))[,1]
  myCLV <- EMA(CLV(HLC(data)))[,1]
  myMACD <- MACD(data[,"Close"])[,2]
  myMFI <- MFI(HLC(data), data[,"Volume"])
  mySAR <- SAR(data[,c("High", "Close")])[,1]
  
  result <- cbind(myATR, mySMI, myADX, myAroon, myBBands, myChaikin, myCLV, myMACD, myMFI, mySAR)
  colnames(result) <- cbind("myATR", "mySMI", "myADX", "myAroon", "myBBands", "myChaikin", "myCLV", "myMACD", "myMFI", "mySAR") 
  return(result) 
  View(result)
}
#模型設置

rm(myTTR.data, model.data, train.data, test.data)
myTTR.data <- myTTR(myStock)[,c("myATR", "myADX", "myMACD", "mySAR", "mySMI")]
model.data <- specifyModel(formula=T.index(myStock, p=0.025, k=10) ~ myTTR.data + runMean(Cl(myStock)) )
train.data <- as.data.frame(modelData(model.data, 
                                      data.window=c(start(myStock), as.Date("2020-03-01")) )) #Books Error was here
test.data <- as.data.frame(modelData(model.data, 
                                     data.window=c(as.Date("2020-03-02"), end(myStock)) ))

colnames(train.data) <- c("T", "myATR", "myADX", "myMACD", "mySAR", "mySMI", "runMean")
colnames(test.data) <- c("T", "myATR", "myADX", "myMACD", "mySAR", "mySMI", "runMean")
form <- as.formula("T~.")
#擬合模型

svm.model <- svm(form, train.data, cost=100)
svm.predict <- predict(svm.model, na.omit(test.data))


#將得到數據轉為買入/持有/賣出訊號

# Transform from T.Index to Buy/Hold/Sell Signal
T2Signal <- function(x, a1=-0.01, a2=-a1){
  result <- ifelse(x<a1, "Sell", ifelse(x>a2, "Buy", "Hold"))
  result <- factor(result, levels=c("Buy", "Hold", "Sell"))
  return(result)
}

accuracy2 <- function(prediction, true){
  t <- table(prediction, true)
  result <- (t["Sell", "Sell"] + t["Buy", "Buy"]) / (t["Sell", "Sell"] + t["Buy", "Buy"] + t["Sell", "Buy"] + t["Buy", "Sell"])
  return(result)
}
##step2 開始測試


## [1] "AAPL"
getSymbols("AAPL")
myStock <- AAPL
colnames(myStock) = paste("myStock", c("Open","High","Low","Close","Volume","Adjusted"), sep=".")
#指標依據

myTTR.data <- myTTR(myStock)[,c("myATR", "myADX", "myMACD", "mySAR", "mySMI")]
model.data <- specifyModel(formula=T.index(myStock, p=0.025, k=10) ~ myTTR.data + runMean(Cl(myStock)) )
#從上市-到2020-0301為訓練模型

train.data <- as.data.frame(modelData(model.data, data.window=c(start(myStock), as.Date("2020-03-01"))))

#從2020-03-02到現在為測試模型–期間正是美國疫情爆發期

test.data <- as.data.frame(modelData(model.data, data.window=c(as.Date("2020-03-01"), end(myStock))))
#分組模擬

colnames(train.data) <- c("T", "myATR", "myADX", "myMACD", "mySAR", "mySMI", "runMean")
colnames(test.data) <- c("T", "myATR", "myADX", "myMACD", "mySAR", "mySMI", "runMean")
form <- as.formula("T~.")

rm(svm.model, svm.predict, signal.pred, signal.true)
svm.model <- svm(form, train.data, cost=100)
svm.predict <- predict(svm.model, na.omit(test.data))
test.data$predict<-svm.predict

signal.pred <- T2Signal(x=svm.predict, a1=-0.095, a2=0.095)
test.data$signal<-signal.pred
signal.true <- T2Signal(x=na.omit(test.data)$T, a1=-0.095, a2=0.095)

#決策樹
if (!require('rpart')){
  install.packages("rpart"); library(rpart)
}
DT<-rpart(signal~myATR+myADX+myMACD+mySAR+mySMI+runMean,
          data=test.data) #訓練組 Training set

DT
if (!require('rpart.plot')){
  install.packages("rpart.plot"); 
  library(rpart.plot)
}
prp(DT)

tail(test.data)

table(signal.pred, signal.true)

#相關係數

accuracy2(signal.pred, signal.true)
# [1] 0.7307692

#DAY
AAPL_COVID19<-as.matrix(to.daily(AAPL_COVID19))
length(rownames(AAPL_COVID19))

#每筆交易損益的向量紀錄
numeric(length(AAPL_COVID19))


#產生紀錄每筆交易損益的向量，並附上日期
profit=setNames(numeric(length(rownames(AAPL_COVID19))),rownames(AAPL_COVID19))
#每日收盤-開盤
profit
im<-1
im<-as.numeric(im)
for(im in rownames(AAPL_COVID19))
{  
  profit[im]=AAPL_COVID19[im,4]-AAPL_COVID19[im,1]
}
profit

#總損益
sum(profit)
#累計損益
cumsum(profit)

change<-data.frame(change=profit)
AAPL_COVID19$每日變化<-change$change

#每次交易的損益
plot1<-plot(profit,type="l",col="red",lwd=2)
abline(h=0,col="green")

#累積損益
plot2<-plot(cumsum(profit),type="l",col="red",lwd=2)
abline(h=0,col="green")

##績效分析
###贏的次數
length(profit[profit>0])

#勝率
length(profit[profit>0])/length(profit[profit!=0])

#平均賺
mean(profit[profit>0])

#平均賠
mean(profit[profit<0])

#賺賠比=平均賺/平均賠
mean(profit[profit>0])/abs(mean(profit[profit<0]))

#扣掉交易成本
#股票手續費
#‧成交金額*0.1425% 
#‧買賣各一次
#‧小數點以下無條件捨去，不足20元以20元計


for (m in rownames(AAPL_COVID19)[-1]) {
  fee=AAPL_COVID19[m,4]*0.001425
  profit[m]=AAPL_COVID19[m,4]-AAPL_COVID19[m,1]-fee
  
}
cbind(AAPL_COVID19[,c(1,4)],profit)   ##check

sum(profit)
cumsum(profit)
plot3<-plot(cumsum(profit),type="l",col="red",lwd=2)

plot4<-plot(profit,type="l",col="red",lwd=2)


