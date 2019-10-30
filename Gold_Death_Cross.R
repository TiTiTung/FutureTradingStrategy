library("quantmod")
stock="0050.tw"
from="2010-01-01"
to="2014-12-31"

#***************************************************
#  Tdata$price: 股價資料
#  Tdata$bs: 買賣訊號，1為買進、2為賣出、0為無訊號
#  Tdata$cash: 目前現金部分
#  Tdata$weight: 目前股票部位
#  Tdata$wealth: 目前資產總市值
#***************************************************
x=getSymbols(stock, auto.assign=FALSE,from=from,to=to) 
x=x[x[,5]>0]
Tdata=x[,4]
colnames(Tdata) <- c("price")
Tdata$bs=seq(0,0,length=nrow(x))
Tdata$cash=seq(0,0,length=nrow(x))
Tdata$weight=seq(0,0,length=nrow(x))
Tdata$wealth=seq(0,0,length=nrow(x))




snum=5
lnum=20
ma_s<-SMA(Tdata$price,n=snum)  
ma_l<-SMA(Tdata$price,n=lnum)
for (i in (lnum+1):nrow(Tdata))
{
  if((ma_s[i-1]<ma_l[i-1])&&(ma_s[i]>ma_l[i]))
  {
    Tdata$bs[i]=1
  }
  else if((ma_s[i-1]>ma_l[i-1])&&(ma_s[i]<ma_l[i]))
  {
    Tdata$bs[i]=2
  }
}



Tdata$cash[1]=100
Tdata$wealth[1]=Tdata$cash[1]+Tdata$weight[1]*Tdata$price[1]
for (i in 2:nrow(Tdata))
{
  if((Tdata$bs[i]==1)&&(Tdata$cash[i-1]>0)) #  Tdata$cash[i-1]>0   前一天要有錢
    #  as.numeric   數值資料(轉成數字)
  {
    Tdata$weight[i]=as.numeric(Tdata$cash[i-1])/as.numeric(Tdata$price[i])
    Tdata$cash[i]=0
  } else if ((Tdata$bs[i]==2)&&(Tdata$weight[i-1]>0))
  {
    Tdata$cash[i]=as.numeric(Tdata$price[i])*as.numeric(Tdata$weight[i-1])
    Tdata$weight[i]=0     #  今天的cash等於今天的價格成上昨日的部位，所以部位是0
  }else
  {
    Tdata$cash[i]=Tdata$cash[i-1]
    Tdata$weight[i]=Tdata$weight[i-1]
  }
  Tdata$wealth[i]=Tdata$cash[i]+Tdata$weight[i]*Tdata$price[i]
}

plot(Tdata$wealth)
