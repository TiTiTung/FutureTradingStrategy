setwd("TXO_5_year_dat")
# ======================================================
# 合併個年選擇權的最後結算價
# ======================================================
option_final_price <- read_csv("opt2018_final_price.csv", locale = locale(encoding = "big5")) 
opt_column <- c("Final_settlement_date", "contract_delivery_month", "code", "code_chinese", "Final_settlement_price")
colnames(option_final_price) = opt_column


TXO_season_LTD_5_year <- read_csv("TXO_season_LTD_5_year.csv", locale = locale(encoding = "big5")) 
filter_year <- 2018
TXO_season <- TXO_season_LTD_5_year %>% filter(date %>% year() == filter_year)

y=option_final_price %>% 
  select( -code_chinese) %>% 
  filter(code == "TXO", Final_settlement_date %in% TXO_season$date)

x=rbind(x,y)
x

write_csv(x, "opt_Final_settlement_price.csv")

# ======================================================
# 結合選擇權跟大盤的價格
# ======================================================


opt_year = 2018
stock = "^TWII"
# ======================================================
#                     導入資料
# ======================================================  
# 利用getSymbols抓取該年份的大盤價格
twii <- getSymbols(stock, auto.assign = FALSE, 
                   from = str_c(opt_year, "-01-01"),
                   to=str_c(opt_year, "-12-31")) 

filter <- date(twii) %in% option_final_price$Final_settlement_date
y=twii[filter,]
x=rbind(x,y)
x 


option_final_price = option_final_price %>% arrange(Final_settlement_date)
option_final_price$TWII_adj= x$TWII.Adjusted %>% as.integer()
write_csv(option_final_price, "Opt_and_TWII_Final_settlement_price.csv")