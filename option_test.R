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
setwd("TXO_5_year_dat")

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



TXO_win_5_year <- read_csv("TXO_win_ratio_0.2.csv", locale = locale(encoding = "big5")) 
TXO_lost_5_year <- read_csv("TXO_lost_ratio_0.2.csv", locale = locale(encoding = "big5")) 
TXO_season_LTD_5_year <- read_csv("TXO_season_LTD_5_year.csv", locale = locale(encoding = "big5")) 
{
filter_year <- 2014
TXO_win <- TXO_win_5_year %>% filter(date %>% year() == filter_year)
TXO_lost <- TXO_lost_5_year %>% filter(date %>% year() == filter_year)
TXO_season <- TXO_season_LTD_5_year %>% filter(date %>% year() == filter_year)

str_c(filter_year, " : ", (n = nrow(TXO_win))/4)
(nrow(TXO_lost))/2

# 贏的金額
cost_win = sum(TXO_win[seq(1,n,4),]$open+TXO_win[seq(2,n,4),]$open) 
revenue = sum(TXO_win[seq(4,n,4),]$Final_p+TXO_win[seq(3,n,4),]$Final_p)
# 輸的金額
cost_lost = -(sum(TXO_season$open) - sum(TXO_lost$open)) 
# 總成本
total_cost = sum(cost_win, cost_lost) 
}


str_c(filter_year, " : ", (n = (nrow(TXO_win))/4))
# 勝率
n/((nrow(TXO_lost))/2+n)
# 贏的報酬率
(revenue - cost_win) / cost_win * 100
# 總的報酬率
(revenue - total_cost) / total_cost * 100





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



