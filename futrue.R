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
column_name <- c("data", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
                 , "close", "volume", "Final_p", "OI", "H1", "H2", "H3", "H4", "H5", "period")

strategy_column <- c("data", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
            , "close", "volume", "Final_p", "OI", "period")

# ==========================================================================
# https://www.taifex.com.tw/cht/3/dlOptDailyMarketView
# 從期交所載下來的年度日資料行情 (每個月一個檔案)
# 因此需要 "每個月" 分別讀近來再進行彙整
# ==========================================================================
option_2018_01 <- read_csv("2018_opt_01.csv", locale = locale(encoding = "big5"))

colnames(option_2018_01) <- column_name

# ==========================================================================
# 把不是台指option的其他契約刪掉
# 只留下一般交易時段 (不使用盤後交易期間)
# remove Last trading day = "NA"
# 取出需要的column
# ==========================================================================
option_2018_01 %<>%
  filter(contract == "TXO" & period == "一般" & LTD != "NA") %>% 
  select(strategy_column)

option_2018_01$c_or_p %<>% factor(levels = c("買權", "賣權"), c("call", "put")) 
                                    




