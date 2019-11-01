TXO2018 <- combine_option_year_dat(2018)

#==========================================================================
# 本策略為跨式策略, 需同時買進相同履約價的 call 跟 put
# 因此只有call或是只有put的履約價將刪除
#==========================================================================
call_put_group_by <- TXO2018 %>% 
  group_by(date, LTD, strike) %>% 
  summarise(only_one = n()) %>% 
  filter(only_one == 2)

call_put_intersection <- TXO2018 %>% 
  filter(str_c(date,LTD,strike) %in% 
         str_c(call_put_group_by$date,
               call_put_group_by$LTD,
               call_put_group_by$strike))%>% 
  group_by(date, LTD, strike) %>% 
  summarise(sum = sum(OI)) %>% 
  group_by(date, LTD) %>% 
  arrange(date, LTD, desc(sum))


target_price = TXO2018 %>% 
  filter(strike == call_put_intersection$strike[1])


















buy_and_sell <- TXO2018[0,]
lose_contract_all <- TXO2018[0,]

sell <- TXO2018[1,]
i = 1

target_price <- TXO2018

change_contract = 0

buy_price_all <- vector("integer", 0)
sell_price_all <- vector("integer", 0)

while (i != nrow(target_price)) {
  

# ==========================================================================
# 抓賣出點那天交易量最大值, 作為明天買進點
# ==========================================================================

call_put_group_by <- TXO2018 %>% # "OI > 10"是因為會出現單邊量太小問題
  filter(date == sell$date & OI > 100) %>% # 賣出點那天的日期
  group_by(date, LTD, strike) %>% 
  summarise(only_one = n()) %>% 
  filter(only_one == 2)

call_put_intersection <- TXO2018 %>% 
  filter(str_c(date,LTD,strike) %in% 
           str_c(call_put_group_by$date,
                 call_put_group_by$LTD,
                 call_put_group_by$strike))%>% 
  group_by(date, LTD, strike) %>% 
  summarise(sum = sum(OI)) %>% 
  group_by(date, LTD) %>% 
  arrange(date, LTD, desc(sum))


target_price = TXO2018 %>% 
  filter(strike == call_put_intersection$strike[1] & date > sell$date)

while (target_price$c_or_p[1] == target_price$c_or_p[2] | 
       target_price$LTD[1] != target_price$LTD[2] ) {
    for (i in 1:nrow(target_price)){
      if ((target_price$LTD[i] %>% str_sub(-2, -1) %>% as.numeric() - 
           target_price$date[i] %>% month()) == 0) {
        
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
            if(
              (((target_price[i,col] - ((target_price$open[1]+target_price$open[2]))) / 
                (target_price$open[1]+target_price$open[2])) >= 1.2) &
              (target_price$c_or_p[1] == target_price$c_or_p[i] & 
               target_price$LTD[1] == target_price$LTD[i])
              )
              {
              break_piont = 1
              print(c(i,col))
              print(target_price[2,])
              print(target_price[i,])
              
              buy = target_price[1,]
              sell = target_price[i,]
              buy_and_sell = rbind(buy_and_sell, buy, sell)
              
              buy_price = buy[1,5]
              sell_price = sell[,col]
              
              buy_price_all %<>% append(buy_price) %>% unlist()
              sell_price_all %<>% append(sell_price) %>% unlist()
              
              break
            } else if (
              (((target_price[i,col] - (target_price$open[1]+target_price$open[2])) / 
                (target_price$open[1]+target_price$open[2])) >= 1.2) &
              (target_price$c_or_p[2] == target_price$c_or_p[i] & 
               target_price$LTD[2] == target_price$LTD[i])
              )
              {
              break_piont = 1
              print(c(i,col))
              print(target_price[2,])
              print(target_price[i,])
              
              buy = target_price[2,]
              sell = target_price[i,]
              buy_and_sell = rbind(buy_and_sell, buy, sell)
              
              buy_price = buy[1,5]
              sell_price = sell[,col]
              
              buy_price_all %<>% append(buy_price) %>% unlist()
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


target_price

change_contract 

buy_price_all
sell_price_all


sum(sell_price_all-buy_price_all) /sum(buy_price_all)*100
