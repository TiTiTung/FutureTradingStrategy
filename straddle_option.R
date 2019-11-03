setwd("~/Documents/FutureTradingStrategy")

write_csv(option, "TXO2015.csv")
setwd("opt_2017")
setwd("opt_2018")
TXO2017 <- combine_option_year_dat_2017(2017)
TXO2016 <- normal_period_combine_option(2016)
TXO2015 <- season_combine_option(2015)
TXO2018 = read_csv("TXO2015.csv")

library("quantmod")
opt_year = 2018
stock = "^TWII"
from =  str_c(opt_year, "-01-01")
to =  str_c(opt_year, "-12-31")

twii <- getSymbols(stock, auto.assign=FALSE,from=from,to=to) 
  
twii_oppen <- twii$TWII.Open %>% 
  round(-2)

TXO_year_dat <- read_csv(str_c("TXO", opt_year, ".csv"), locale = locale(encoding = "big5"))





twii_oppen <- twii$TWII.Open %>% 
  round(-2)
buy_and_sell <- TXO_year_dat[0,]
lose_contract_all <- TXO_year_dat[0,]
lose_contract_LTD_all <- TXO_year_dat[0,]
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
      
      lose_contract = target_price[1:2,]
      lose_contract_all = rbind(lose_contract_all, lose_contract)
      
      lose_contract_LTD = target_price[i,]
      lose_contract_LTD_all = rbind(lose_contract_LTD_all, lose_contract_LTD)
      
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
          
          buy_call = target_price[1,]
          buy_put = target_price[2,]
          sell = target_price[i,]
          buy_and_sell = rbind(buy_and_sell, buy_call, buy_put, sell)
          
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
          
          buy_put = target_price[2,]
          buy_call = target_price[1,]
          sell = target_price[i,]
          buy_and_sell = rbind(buy_and_sell, buy_put, buy_call, sell)
          
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

lose_contract_LTD_all

buy_and_sell 

lose_contract_all 
lose_contract_all$open[-(1:2)]

target_price

change_contract 

buy_price_all
buy_price_all_2
sell_price_all

(sum(sell_price_all) - sum(buy_price_all+buy_price_all_2)) / sum(buy_price_all+buy_price_all_2) * 100
    
sum(buy_price_all+buy_price_all_2) /sum(sell_price_all, lose_contract_all$open[-(1:2)])*100
