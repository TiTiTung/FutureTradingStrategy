library("quantmod")
stock="^TWII"
from="2018-01-01"
to="2018-12-31"

twii <- getSymbols(stock, auto.assign=FALSE,from=from,to=to) 
  
twii_oppen <- twii$TWII.Open %>% 
  round(-2)


straddle_Final_p <- TXO2018 %>% 
  filter(date >= twii_oppen$TWII.Open[1] %>% date()) %>% 
  filter(strike == twii_oppen$TWII.Open[[1]])





twii_oppen <- twii$TWII.Open %>% 
  round(-2)
buy_and_sell <- TXO2018[0,]
lose_contract_all <- TXO2018[0,]

sell <- TXO2018[1,]
i = 1

target_price <- TXO2018

change_contract = 0

buy_price_all <- vector("integer", 0)
buy_price_all_2 <- vector("integer", 0)
sell_price_all <- vector("integer", 0)

while (i != nrow(target_price)) {
  
  twii_oppen = twii_oppen[twii_oppen$TWII.Open %>% date() > sell$date,]
  
  target_price <- TXO2018 %>% 
    filter(date >= twii_oppen$TWII.Open[1] %>% date()) %>% 
    filter(strike == twii_oppen$TWII.Open[[1]])
  
  # call and put (all leat trading price) the same
  while (target_price$c_or_p[1] == target_price$c_or_p[2] | 
         target_price$LTD[1] != target_price$LTD[2] ) {
    for (i in 1:nrow(target_price)){
      if ((target_price$LTD[i] %>% str_sub(-2, -1) %>% as.numeric() - 
           target_price$date[i] %>% month()) != 0) {  # 到期月份不能夠是當月到期
        
        target_price = target_price[-(1:2),]
        
      } else if (target_price$c_or_p[i] == target_price$c_or_p[i+1] | 
                 target_price$LTD[i] != target_price$LTD[i+1] ){
        
        target_price = target_price[-(1),]
        
        
      } else {
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
      
      break_piont = 1
      sell = target_price[i,]
      sell$date = sell$date+1
      
    } else {
      
      for (col in c(5, 7, 6, 8)){
        print(i)
        if(target_price$Final_p[i] > (target_price$open[1] + target_price$open[2]) &
           (target_price$c_or_p[1] == target_price$c_or_p[i] & 
            target_price$LTD[1] == target_price$LTD[i]) &
           ((target_price$LTD[i] %>% str_sub(-2, -1) %>% as.numeric() - 
             target_price$date[i] %>% month()) != 0))
        {
          break_piont = 1
          print(c(i,col))
          print(target_price[1,])
          print(target_price[i,])
          
          buy_call = target_price[1,]
          buy_put = target_price[2,]
          sell = target_price[i,]
          buy_and_sell = rbind(buy_and_sell, buy_call, buy_put, sell)
          
          buy_price = buy_call[1,5]
          sell_price = sell[,col]
          
          buy_price_all %<>% append(buy_price) %>% unlist()
          buy_price_all_2 %<>% append(buy_put[1,5]) %>% unlist()
          sell_price_all %<>% append(sell_price) %>% unlist()
          
          break
        } else if (target_price$Final_p[i] > (target_price$open[1] + target_price$open[2]) &
                   (target_price$c_or_p[2] == target_price$c_or_p[i] & 
                    target_price$LTD[2] == target_price$LTD[i]) & 
                   ((target_price$LTD[i] %>% str_sub(-2, -1) %>% as.numeric() - 
                     target_price$date[i] %>% month()) == 0))
        {
          break_piont = 1
          print(c(i,col))
          print(target_price[2,])
          print(target_price[i,])
          
          buy_put = target_price[2,]
          buy_call = target_price[1,]
          sell = target_price[i,]
          buy_and_sell = rbind(buy_and_sell, buy_put, buy_call, sell)
          
          buy_price = buy_put[1,5]
          sell_price = sell[,col]
          
          buy_price_all %<>% append(buy_price) %>% unlist()
          buy_price_all_2 %<>% append(buy_call[1,5]) %>% unlist()
          sell_price_all %<>% append(sell_price) %>% unlist()
          
          break
          
        }
        
      }
    }
    
    
    if (break_piont ==1) {
      break
    }
  }
}



buy_and_sell 

lose_contract_all 
lose_contract_all$open[-2]

target_price

change_contract 

buy_price_all
buy_price_all_2
sell_price_all


sum(buy_price_all[-c(3,7)]+buy_price_all_2[-c(3,7)]) /sum(sell_price_all[-c(3,7)], 
                                                          lose_contract_all[-2,5])*100
