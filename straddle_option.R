# ==========================================================================
# sessionInfo()
# Sys.getlocale()
# 應該要長這樣：[1] "en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/C"
# https://shihs.github.io/blog/r/2018/10/04/R-RStudtio編碼問題/
# ==========================================================================
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

source("combine_option_dat.R")
setwd("~/Documents/FutureTradingStrategy")

#==========================================================================
# Build a Future Trading Strategy  # TI-TI TUNG 
# 載入套件, 只有 magrittr 才能夠使用 %<>% 
#==========================================================================
library(magrittr)
library(ggplot2)
library(stringr)
library(tidyverse)
library(highcharter)
library(readr)
library(lubridate)
library(zoo)
library(timeDate)
library("quantmod")

#==========================================================================
# 跨式策略開始
# 選定年份
#==========================================================================
opt_year = 2015

stock = "^TWII"
from =  str_c(opt_year, "-01-01")
to =  str_c(opt_year, "-12-31")

twii <- getSymbols(stock, auto.assign=FALSE,from=from,to=to) 
  
twii_oppen <- twii$TWII.Open %>% 
  round(-2)

TXO_year_dat <- read_csv(str_c("TXO", opt_year, ".csv"), locale = locale(encoding = "big5"))
TXO_year_dat$date %<>% as.Date()




twii_oppen <- twii$TWII.Open %>% 
  round(-2)
buy_and_sell <- TXO_year_dat[0,]
lost_contract_all <- TXO_year_dat[0,]
lost_contract_LTD_all <- TXO_year_dat[0,]
sell <- TXO_year_dat[1,]
i = 1

target_price <- TXO_year_dat

change_contract = 0

buy_price_all <- vector("integer", 0)
buy_price_all_2 <- vector("integer", 0)
sell_price_all <- vector("integer", 0)

while (i != nrow(target_price)) {

  break_test_call_put_exit = 0
  while (break_test_call_put_exit == 0) {  
      twii_oppen = twii_oppen[twii_oppen$TWII.Open %>% date() > sell$date,]
      
      test_call_put <- TXO_year_dat %>% 
        filter(date >= twii_oppen$TWII.Open[1] %>% date()) %>% 
        filter(strike == twii_oppen$TWII.Open[[1]])
      
      # 大盤價平作為option的買進點, option的價格不存在確實奇怪
      # 減去只有當日只有call or put的情況
      n = 0
      for (i in 1:nrow(test_call_put)){
        print(i)
        if ((test_call_put$date[i] != test_call_put$date[i+1] |
            test_call_put$c_or_p[i] == test_call_put$c_or_p[i+1])) {  # 到期月份不能夠是當月到期
      
          n = n + 1
          sell$date = test_call_put$date[n+1]
        }  else {
          
          twii_oppen = twii_oppen[twii_oppen$TWII.Open %>% date() >= sell$date,]
          
          test_call_put <- TXO_year_dat %>% 
            filter(date >= twii_oppen$TWII.Open[1] %>% date()) %>% 
            filter(strike == twii_oppen$TWII.Open[[1]])
          
          target_price = test_call_put
          break_test_call_put_exit = 1
          break
          
        }
      }
  }
  
  # ==========================================================================
  # 計算報酬率
  # ==========================================================================
  break_piont = 0
  for (i in 1:nrow(target_price)){
    
    # 季結算日要結算換新單
    if (target_price$date[i] %>% month() == 
        target_price$LTD[1] %>% str_sub(-2, -1) %>% as.numeric() &
        target_price$date[i] %>% day() >= 15 &
        target_price$date[i] %>% day() <= 21 &
        target_price$weekdays[i] == "Wednesday" 
    ) {
      
      change_contract = change_contract + 1
      
      lost_contract = target_price[1:2,]
      lost_contract_all = rbind(lost_contract_all, lost_contract)
      
      lost_contract_LTD = target_price[i,]
      lost_contract_LTD_all = rbind(lost_contract_LTD_all, lost_contract_LTD)
      
      break_piont = 1
      sell = target_price[i,]
      
    } else {
      
        if(target_price$Final_p[i] > (target_price$open[1] + target_price$open[2]) &
           (target_price$c_or_p[1] == target_price$c_or_p[i] & 
            target_price$LTD[1] == target_price$LTD[i]) &
           ((target_price$LTD[i] %>% str_sub(-2, -1) %>% as.numeric() - 
             target_price$date[i] %>% month()) != 0))
        {
          break_piont = 1
          print(target_price[1,])
          print(target_price[i,])
          
          
          # 賣出價格應為當日結算價以高出成本後, 第二天相同履約價格的open price買進, 
          # 不可設計成當日結算價以高出成本後, 當天就買進
          sell_price_filter <- target_price %>% 
            filter(date > target_price$date[i] & 
                     LTD == target_price$LTD[i] & 
                     strike == target_price$strike[i] & 
                     c_or_p == target_price$c_or_p[i])
          
          buy_put = target_price[2,]
          buy_call = target_price[1,]
          trading_point = target_price[i,]
          sell = sell_price_filter[1,]
          buy_and_sell = rbind(buy_and_sell, buy_put, buy_call, trading_point, sell)
            
            buy_price = buy_call[1,5]
            sell_price = sell[,5]
            
            buy_price_all %<>% append(buy_price) %>% unlist()
            buy_price_all_2 %<>% append(buy_put[1,5]) %>% unlist()
            sell_price_all %<>% append(sell_price) %>% unlist()
          
          break
        } else if (target_price$Final_p[i] > (target_price$open[1] + target_price$open[2]) &
                   (target_price$c_or_p[2] == target_price$c_or_p[i] & 
                    target_price$LTD[2] == target_price$LTD[i]) & 
                   ((target_price$LTD[i] %>% str_sub(-2, -1) %>% as.numeric() - 
                     target_price$date[i] %>% month()) != 0))
        {
          break_piont = 1
          print(target_price[2,])
          print(target_price[i,])
          
          
          # 賣出價格應為當日結算價以高出成本後, 第二天相同履約價格的open price買進, 
          # 不可設計成當日結算價以高出成本後, 當天就買進
          sell_price_filter <- target_price %>% 
            filter(date > target_price$date[i] & 
                     LTD == target_price$LTD[i] & 
                     strike == target_price$strike[i] & 
                     c_or_p == target_price$c_or_p[i])
          
          buy_put = target_price[2,]
          buy_call = target_price[1,]
          trading_point = target_price[i,]
          sell = sell_price_filter[1,]
          buy_and_sell = rbind(buy_and_sell, buy_put, buy_call, trading_point, sell)
          
            buy_price = buy_put[1,5]
            sell_price = sell[,5]
            
            buy_price_all %<>% append(buy_price) %>% unlist()
            buy_price_all_2 %<>% append(buy_call[1,5]) %>% unlist()
            sell_price_all %<>% append(sell_price) %>% unlist()
          
          break
          
        }
      
    }
    
    
    if (break_piont ==1) {
      break
    }
  }
}

# 查看資料中的四個季度 (3, 6, ,9 ,12)
lost_contract_LTD_all

# 查看買進與賣出點

# 每一次的買賣分為4個row, 前兩個為買進的call跟put, 
# 第三個為結算價已經高過成本的時點, 第四個row為當
# 成本底平時, 的下一天的開盤價, 該價格即為賣出時點
buy_and_sell 


# 為選擇權契約到期時, 皆未達到賣出目標的時點
lost_contract_all 






TXO2014_win <- buy_and_sell
TXO2014_lost <- lost_contract_all

TXO2015_win <- buy_and_sell
TXO2015_lost <- lost_contract_all

TXO2016_win <- buy_and_sell
TXO2016_lost <- lost_contract_all

TXO2017_win <- buy_and_sell
TXO2017_lost <- lost_contract_all

TXO2018_win <- buy_and_sell
TXO2018_lost <- lost_contract_all

TXO_win_5_year <- rbind(TXO2014_win, TXO2015_win, TXO2016_win, TXO2017_win, TXO2018_win)
TXO_lost_5_year <- rbind(TXO2014_lost, TXO2015_lost, TXO2016_lost, TXO2017_lost, TXO2018_lost)

TXO_win_5_year$strike %<>% as.integer()
write_csv(TXO_win_5_year, "TXO_win_5_year.csv")
write_csv(TXO_lost_5_year, "TXO_lost_5_year.csv")
