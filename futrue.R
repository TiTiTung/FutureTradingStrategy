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
setwd("opt_2018")

# ==========================================================================
# sessionInfo()
# Sys.getlocale()
# 應該要長這樣：[1] "en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/C"
# https://shihs.github.io/blog/r/2018/10/04/R-RStudtio編碼問題/
# ==========================================================================
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

# ==========================================================================
# column_name是原始資料所有的column, strategy_column是建構策略時所需的column
# ==========================================================================
all_column_name <- c("date", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
                 , "close", "volume", "Final_p", "OI", "H1", "H2", "H3", "H4", "H5", "period")

TXO_column <- c("date", "LTD", "strike", "c_or_p", "open", "high", "low"
            , "close", "volume", "OI")

LTD_season <- c("2018-03", "2018-06", "2018-09", "2018-12")
# ==========================================================================
# https://www.taifex.com.tw/cht/3/dlOptDailyMarketView
# 從期交所載下來的年度日資料行情 (每個月一個檔案)
# 因此需要 "每個月" 分別讀近來再進行彙整
# ==========================================================================
option_2018_01 <- read_csv("2018_opt_01.csv", locale = locale(encoding = "big5"))

colnames(option_2018_01) <- all_column_name

# 把中文的買賣全轉成call and put
option_2018_01$c_or_p %<>% 
  factor(levels = c("買權", "賣權"), c("call", "put")) 

# 原本的最後交易日的資料型態是integer (像201801), 但我們想轉成日期型態
option_2018_01$LTD %<>% 
  as.character() %>% 
  str_c("01") %>% 
  ymd() %>% 
  format("%Y-%m")


# ==========================================================================
# 把不是台指option的其他契約刪掉
# 只留下一般交易時段 (不使用盤後交易期間)
# remove Last trading day = "NA"
# 取出需要的column
# ==========================================================================
option_2018_01 %<>%
  filter(contract == "TXO" & period == "一般" & open != "-" & 
           LTD != "NA" & (LTD %in% LTD_season)) %>% 
  select(TXO_column)

# 因為原本的開高低收價格有缺失值 (用這個表示"-")
# 因此整個column是 "chr" , 現在用as.numeric轉為數值
option_2018_01[,5:8] %<>% apply(2,as.numeric) %>% as.tibble()

# ==========================================================================
# 判斷留倉量
# ==========================================================================





# ==========================================================================
# 計算報酬率
# ==========================================================================
ken = 0
for (i in 1:2627){
  for (col in c(5, 7, 6, 8)){
    
    if((((option_2018_01[i,col] - option_2018_01$open[27]) / option_2018_01$open[27]) > 1.2) &(
      option_2018_01$strike[27] == option_2018_01$strike[i]
    )){
      ken = ken + 1
      print(c(i,col))
      print(option_2018_01[27,])
      print(option_2018_01[i,])
      stop("賣出!")
    } else {
      ken = ken + 0
    }
    
  }
  
  
}

# ==========================================================================











x <- option_2018_01
x %>% 
  filter(date == "2018-01-03") %>% 
  arrange(desc(OI))
  
  
x %>% 
  group_by(date, LTD, c_or_p) %>% 
  summarise(maxOI = max(OI))

z <- option_2018_01 %>% 
  filter(date %in% x$date, OI %in% x$maxOI)


a=option_2018_01 %>% 
  filter(strike == z$strike[2])

aa=option_2018_01 %>% 
  filter(LTD == "2018-03-01", strike == z$strike[2], c_or_p == "call")


nrow(aa)




