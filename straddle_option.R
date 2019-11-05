# =============================================================
# sessionInfo()
# Sys.getlocale()
# 應該要長這樣：
# [1] "en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/C"
# https://shihs.github.io/blog/r/2018/10/04/R-RStudtio編碼問題/
# =============================================================
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

source("combine_option_dat.R")
setwd("~/Documents/FutureTradingStrategy")

# ======================================================
# Build a Future Trading Strategy  # TI-TI TUNG 
# 載入套件, 只有 magrittr 才能夠使用 %<>% 
# ======================================================
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

# ======================================================
# ======================================================
#                     跨式策略開始
# ======================================================
# ======================================================
# 目前策略設定為, 在選定年份後 (opt_year = 2018), 即在該年的
# 第一個日期依據現貨大盤, 買進台指期選擇權的價平價格。依此類
# 推, 進場後, 獲利出場或合約到期出場的隔一天, 即依據價平價格
# 買進台指選擇權。

{ # 基本設定 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊
# ======================================================
# 選定 年份 與 獲利出場標準
# ======================================================
opt_year = 2017
win_ratio = 0.2
stock = "^TWII"

# ======================================================
#                     導入資料
# ======================================================  
# 利用getSymbols抓取該年份的大盤價格
twii <- getSymbols(stock, auto.assign = FALSE, 
                   from = str_c(opt_year, "-01-01"),
                   to=str_c(opt_year, "-12-31")) 
# 因為選擇權的履約價最小變動價格為100, 
# 因此將大盤價格社捨五入到百位數, round(-2)
twii_oppen <- twii$TWII.Open %>% round(-2)

# 載入選定年份的台指選擇權的資料
TXO_year_dat <- read_csv(str_c("TXO", opt_year, ".csv"), 
                         locale = locale(encoding = "big5"))
TXO_year_dat$date %<>% as.Date()


# 2017-06-22的大盤價格為10400, 但是選擇權只有call有這個價格
# 目前先以手動更改履約價為10300為最近其存在call跟put的履約價
# twii_oppen["2017-06-22",] = 10300

# ======================================================
#                       變數初始化
# ======================================================
# 儲存買進點與賣出點(在合約到期前, 單邊獲利達win_ratio)
buy_and_sell <- TXO_year_dat[0,]
# 儲存買進點, 但在合約到期前皆為獲利達到標準 (win_ratio)
lost_contract_all <- TXO_year_dat[0,]
# 儲存四個季月到期日的call跟put資料
lost_contract_LTD_all <- TXO_year_dat[0,]

# 由於最後單邊獲利的單日資料將儲存在 "sell" 中, 再用
# twii_oppen$TWII.Open %>% date() > sell$date 來取原本資料
# 的子集, 並以子級的第一筆資料作為買進價, 因此須先將 "sell" 
# 設為TXO_year_dat[1,], 即為該年度第一筆交易資料, 讓我們可以買進
sell <- TXO_year_dat[1,]

# 之後的在進行買賣時, 使用 while (i != nrow(target_price)){}
# 由於我們會不斷取子集, 使得target_price不段變化, 直到取到
# target_price最後一筆時, 迴圈才會停下來, 故先將i設為1, 迴圈才能
# 開始運作
i = 1

# 取子集時, 會不斷以大盤價平價格重建target_price, 再以target_price
# 作為買進賣出點的搜尋依據, 因此不能直接重建TXO_year_dat, 而已先將
# target_price <- TXO_year_dat, 才能不段從遠史資料中取子集出來
# 如下：
# test_call_put <- TXO_year_dat %>% 
#   filter(date >= twii_oppen$TWII.Open[1] %>% date()) %>% 
#   filter(strike == twii_oppen$TWII.Open[[1]])
target_price <- TXO_year_dat
} # 基本設定結束 ＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊






# ======================================================
#                     跨式策略開始
# ======================================================
while (i != nrow(target_price)) {

  # 為了讓while迴圈break出來所設置的初始值
  break_test_call_put_exit = 0
  # ======================================================
  #                     去除單邊的迴圈
  # ======================================================
  while (break_test_call_put_exit == 0) { 
      twii_oppen = twii_oppen[twii_oppen$TWII.Open %>% date() > sell$date,]
      
      test_call_put <- TXO_year_dat %>% 
        filter(date >= twii_oppen$TWII.Open[1] %>% date()) %>% 
        filter(strike == twii_oppen$TWII.Open[[1]])
      
      # 大盤價平作為option的買進點, option的價格不存在大盤的當日價格確實奇怪
      # 去除當日只有call or put的單邊價格情況
      n = 0
      for (i in 1:nrow(test_call_put)){
        # 如果第一筆與第二筆的日期不一樣, 或是方向一樣, 代表當日可能有有單邊價鉻, 
        # 因此將不斷向下個日期搜尋, 直到存在雙邊價格, 才儲存資料作為買進價格
        if ((test_call_put$date[i] != test_call_put$date[i+1] |
            test_call_put$c_or_p[i] == test_call_put$c_or_p[i+1])) {  
          # 到期月份不能夠是當月到期
      
          n = n + 1
          sell$date = test_call_put$date[n+1]
        }  else {
          # 直到出現雙邊價格時 (test_call_put$date[n+1]), 再從原始資料TXO_year_dat
          # 終取子集, 再使target_price = test_call_put, 正式開始尋找買賣點
          twii_oppen = twii_oppen[twii_oppen$TWII.Open %>% date() >= sell$date,]
          
          test_call_put <- TXO_year_dat %>% 
            filter(date >= twii_oppen$TWII.Open[1] %>% date()) %>% 
            filter(strike == twii_oppen$TWII.Open[[1]])
          
          target_price = test_call_put
          
          # 找到雙邊價格的開始日期後, 即可離開迴圈
          break_test_call_put_exit = 1
          break
        }
      }
  }  #  去除單邊的迴圈結束
  # ===================================================================
  #                             無法解決個案
  # ===================================================================
  # 2017-06-22的大盤價格為10400, 但是選擇權只有call有這個價格但是大盤價
  # 格為10400之情況下, 依據迴圈往下推的日期中, 也都是單邊價鉻, 因此在相
  # 同旅在目前先以手動更改履約價為10300,其為近期存在call跟put的履約價
  # twii_oppen["2017-06-22",] = 10300
  # ===================================================================
  
  
  
  # ======================================================
  # 計算報酬率
  # ======================================================
  break_piont = 0
  for (i in 1:nrow(target_price)){
    
    # 季結算日要結算換新單
    # 計算如果買進至到期結算都未獲利的結算日期, 以及當時買進持有的雙邊資料
    if (target_price$date[i] %>% month() == 
        target_price$LTD[1] %>% str_sub(-2, -1) %>% as.numeric() &
        target_price$date[i] %>% day() >= 15 &
        target_price$date[i] %>% day() <= 21 &
        target_price$weekdays[i] == "Wednesday" 
    ) { # 季結算日無法獲利的判斷if
      
      lost_contract = target_price[1:2,]
      lost_contract_all = rbind(lost_contract_all, lost_contract)
      
      # 季結算日日期
      lost_contract_LTD_all = rbind(lost_contract_LTD_all, target_price[i,], target_price[i+1,])
      
      #合約到期, 跳出該迴圈, 繼續跑一下一筆買進單
      break_piont = 1
      
      #合約到期, 重新調整日期
      sell = target_price[i,]
      
    } else { # 季結算日前獲利的判斷if
        
      # ======================================================
      #                    買進 call 獲利的if()
      # ======================================================
        if((target_price$Final_p[i] - (target_price$open[1] + target_price$open[2])) /
           (target_price$open[1] + target_price$open[2]) > win_ratio & # win_ratio報酬率
           (target_price$c_or_p[1] == target_price$c_or_p[i] & 
            target_price$LTD[1] == target_price$LTD[i]) &
           ((target_price$LTD[i] %>% str_sub(-2, -1) %>% as.numeric() - 
             target_price$date[i] %>% month()) != 0))
        {
          break_piont = 1
          buy_put = target_price[2,]
          buy_call = target_price[1,]
          sell = target_price[i,]
          # target_price[(i-1):(i+1),]是為了在賣出獲利的call時, 也不需讓put同時出場
          # 由於無法準確判斷相同和約翰日期的put是在call之前還是之後, 先以這樣處理在手動刪除
          buy_and_sell = rbind(buy_and_sell, buy_put, buy_call, target_price[(i-1):(i+1),])
          break
      # ======================================================
      #                    買進 put 獲利的if()
      # ======================================================
        } else if ((target_price$Final_p[i] - (target_price$open[1] + target_price$open[2])) /
                   (target_price$open[1] + target_price$open[2]) > win_ratio  & # win_ratio報酬率
                   (target_price$c_or_p[2] == target_price$c_or_p[i] & 
                    target_price$LTD[2] == target_price$LTD[i]) & 
                   ((target_price$LTD[i] %>% str_sub(-2, -1) %>% as.numeric() - 
                     target_price$date[i] %>% month()) != 0))
        {
          break_piont = 1
          
          buy_put = target_price[2,]
          buy_call = target_price[1,]
          sell = target_price[i,]
          # target_price[(i-1):(i+1),]是為了在賣出獲利的ㄔㄧㄣ時, 也不需讓call同時出場
          # 由於無法準確判斷相同和約翰日期的call是在put之前還是之後, 先以這樣處理在手動刪除
          buy_and_sell = rbind(buy_and_sell, buy_put, buy_call, target_price[(i-1):(i+1),])
          break
        }
      
    }# 季結算日前獲利的判斷if
    
    
    if (break_piont ==1) {
      break
    }
  }
  
}





# ======================================================
#                 資料合併與儲存
# ======================================================


# buy_and_sell = buy_and_sell[-c(3),]

# 查看買進與賣出點
# 每一次的買賣分為4個row, 前兩個為買進的call跟put, 
# 第三個為結算價已經高過成本的時點, 第四個row為當
# 成本底平時, 的下一天的開盤價, 該價格即為賣出時點
(TXO2017_win <- buy_and_sell)
# 查看資料中的四個季度 (3, 6, ,9 ,12)
(TXO2017_lost <- lost_contract_all)
# 為選擇權契約到期時, 皆未達到賣出目標的時點
(TXO2017_season <- lost_contract_LTD_all)




TXO_win_5_year <- 
  rbind(TXO2014_win, TXO2015_win, TXO2016_win, TXO2017_win, TXO2018_win)

TXO_lost_5_year <- 
  rbind(TXO2014_lost, TXO2015_lost, TXO2016_lost, TXO2017_lost, TXO2018_lost)

TXO_season_LTD_5_year <- 
  rbind(TXO2014_season, TXO2015_season, TXO2016_season, TXO2017_season, TXO2018_season)

TXO_win_5_year$strike %<>% as.integer()
TXO_lost_5_year$strike %<>% as.integer()
TXO_season_LTD_5_year$strike %<>% as.integer()

write_csv(TXO_win_5_year, "TXO_win_ratio_0.2.csv")
write_csv(TXO_lost_5_year, "TXO_lost_ratio_0.2.csv")
write_csv(TXO_season_LTD_5_year, "TXO_season_LTD_5_year.csv")
