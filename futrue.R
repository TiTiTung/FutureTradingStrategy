#==========================================================================
# TI-TI TUNG 
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
# https://www.taifex.com.tw/cht/3/dlOptDailyMarketView
# 從期交所載下來的年度日資料行情 (每個月一個檔案)
# 
# ==========================================================================
option_2018_01 <- read_csv("2018_opt_01.csv", locale = locale(encoding = "big5"))

# 
column_name <- c("data", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
, "close", "volume", "Final_p", "OI", "H1", "H2", "H3", "H4", "H5", "period")

column <- c("data", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
                 , "close", "volume", "Final_p", "OI", "period")


colnames(option_2018_01) <- column_name

option_2018_01 %<>%
  filter(contract == "TXO" & period == "一般" & LTD != "NA") %>% 
  select(column)

option_2018_01$c_or_p %<>% factor(levels = c("買權", "賣權"), c("call", "put")) 
                                    




