# ==========================================================================
# sessionInfo()
# Sys.getlocale()
# 應該要長這樣：[1] "en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/C"
# https://shihs.github.io/blog/r/2018/10/04/R-RStudtio編碼問題/
# ==========================================================================
Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

source("combine_option_dat.R")
setwd("opt_2018")
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

# ==========================================================================
# column_name是原始資料所有的column, strategy_column是建構策略時所需的column
# ==========================================================================
all_column_name <- c("date", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
                 , "close", "volume", "Final_p", "OI", "H1", "H2", "H3", "H4", "H5")

TXO_column <- c("date", "LTD", "strike", "c_or_p", "open", "high", "low"
            , "close", "Final_p", "volume", "OI")
opt_year = 2015
LTD_season <- c(str_c(opt_year,"-03"), str_c(opt_year,"-06"), 
                str_c(opt_year,"-09"), str_c(opt_year,"-12"))

# ==========================================================================
# https://www.taifex.com.tw/cht/3/dlOptDailyMarketView
# 從期交所載下來的年度日資料行情 (每個月一個檔案)
# 因此需要 "每個月" 分別讀近來再進行彙整
# ==========================================================================
option_2018_01 <- read_csv("2015_3_opt.csv", locale = locale(encoding = "big5"))

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
  filter(contract == "TXO"  & open != "-" & 
           LTD != "NA" & (LTD %in% LTD_season)) %>% 
  select(TXO_column)%>% 
  mutate(weekdays = weekdays(date %>% as.Date()))

# 因為原本的開高低收價格有缺失值 (用這個表示"-")
# 因此整個column是 "chr" , 現在用as.numeric轉為數值
option_2018_01[,5:8] %<>% apply(2,as.numeric) %>% as.tibble()

# ==========================================================================
# 判斷留倉量
# ==========================================================================

option = rbind(option, option_2018_01)



TXO_win_5_year <- read_csv("TXO_win_5_year.csv", locale = locale(encoding = "big5")) 
TXO_lost_5_year <- read_csv("TXO_lost_5_year.csv", locale = locale(encoding = "big5")) 

filter_year <- 2017
TXO_win <- TXO_win_5_year %>% 
  filter(date %>% year() == filter_year)
TXO_lost <- TXO_lost_5_year %>% 
  filter(date %>% year() == filter_year)

str_c(filter_year, " : ", (n = nrow(TXO_win))/4)
(nrow(TXO_lost))/2

-sum(TXO_win[seq(1,n,4),]$open+TXO_win[seq(2,n,4),]$open)+ sum(TXO_win[seq(4,n,4),]$open)

-sum(TXO_lost$open)



#==========================================================================
# 跨式策略開始
# 選定年份
#==========================================================================
opt_year = 2018

stock = "^TWII"
from =  str_c(opt_year, "-01-01")
to =  str_c(opt_year, "-12-31")

twii <- getSymbols(stock, auto.assign=FALSE,from=from,to=to) 

plot(twii)



