


combine_option_year_dat <- function(opt_year) {
  
  all_column_name <- c("date", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
                       , "close", "volume", "Final_p", "OI", "H1", "H2", "H3", "H4", "H5", "period")
  
  TXO_column <- c("date", "LTD", "strike", "c_or_p", "open", "high", "low"
                  , "close", "Final_p", "volume", "OI")
  
  LTD_season <- c(str_c(opt_year,"-03"), str_c(opt_year,"-06"), 
                  str_c(opt_year,"-09"), str_c(opt_year,"-12"))
  
  option1 =read_csv(str_c(opt_year, "_opt_01", ".csv"), locale = locale(encoding = "big5"))
  colnames(option1) <- all_column_name
  
  # 把中文的買賣全轉成call and put
  option1$c_or_p %<>% 
    factor(levels = c("買權", "賣權"), c("call", "put")) 
  
  # 原本的最後交易日的資料型態是integer (像201801), 但我們想轉成日期型態
  option1$LTD %<>% 
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
  option1 %<>%
    filter(contract == "TXO" & period == "一般" & open != "-" & 
             LTD != "NA" & (LTD %in% LTD_season)) %>% 
    select(TXO_column)%>% 
    mutate(weekdays = weekdays(date %>% as.Date()))
  
  
  for (opt_month in c("02", "03", "04", "05", "06", "07",
                      "08", "09", "10", "11", "12")) {
    


      
      option <- 
        read_csv(str_c(opt_year, "_opt_", opt_month, ".csv"), locale = locale(encoding = "big5"))
      colnames(option) <- all_column_name
      
      # 把中文的買賣全轉成call and put
      option$c_or_p %<>% 
        factor(levels = c("買權", "賣權"), c("call", "put")) 
      
      # 原本的最後交易日的資料型態是integer (像201801), 但我們想轉成日期型態
      option$LTD %<>% 
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
      option %<>%
        filter(contract == "TXO" & period == "一般" & open != "-" & 
                 LTD != "NA" & (LTD %in% LTD_season)) %>% 
        select(TXO_column) %>% 
        mutate(weekdays = weekdays(date %>% as.Date()))
      
      
      
      option1 <- rbind(option1, option)

    
    
  }
  
  option1[,5:8] %<>% apply(2,as.numeric) %>% as.tibble()
  option1$date %<>% as.Date()
  return(option1)

}

combine_option_year_dat_2017 <- function(opt_year) {
  
  all_column_name <- c("date", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
                       , "close", "volume", "Final_p", "OI", "H1", "H2", "H3", "H4", "H5", "period")
  
  TXO_column <- c("date", "LTD", "strike", "c_or_p", "open", "high", "low"
                  , "close", "Final_p", "volume", "OI")
  
  LTD_season <- c(str_c(opt_year,"-03"), str_c(opt_year,"-06"), 
                  str_c(opt_year,"-09"), str_c(opt_year,"-12"))
  
  option1 =read_csv(str_c(opt_year, "_opt_1", ".csv"), locale = locale(encoding = "big5"))
  colnames(option1) <- all_column_name
  
  # 把中文的買賣全轉成call and put
  option1$c_or_p %<>% 
    factor(levels = c("買權", "賣權"), c("call", "put")) 
  
  # 原本的最後交易日的資料型態是integer (像201801), 但我們想轉成日期型態
  option1$LTD %<>% 
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
  option1 %<>%
    filter(contract == "TXO" & period == "一般" & open != "-" & 
             LTD != "NA" & (LTD %in% LTD_season)) %>% 
    select(TXO_column)%>% 
    mutate(weekdays = weekdays(date %>% as.Date()))
  
  
  for (opt_month in c("2", "3", "4", "5", "6_1", "6_2", "7_1", "7_2",
                      "8_1", "8_2", "9_1", "9_2", "10_1", "10_2", "11_1", "11_2", "12_1", "12_2")) {
    
    
    
    
    option <- 
      read_csv(str_c(opt_year, "_opt_", opt_month, ".csv"), locale = locale(encoding = "big5"))
    colnames(option) <- all_column_name
    
    # 把中文的買賣全轉成call and put
    option$c_or_p %<>% 
      factor(levels = c("買權", "賣權"), c("call", "put")) 
    
    # 原本的最後交易日的資料型態是integer (像201801), 但我們想轉成日期型態
    option$LTD %<>% 
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
    option %<>%
      filter(contract == "TXO" & period == "一般" & open != "-" & 
               LTD != "NA" & (LTD %in% LTD_season)) %>% 
      select(TXO_column) %>% 
      mutate(weekdays = weekdays(date %>% as.Date()))
    
    
    
    option1 <- rbind(option1, option)
    
    
    
  }
  
  option1[,5:8] %<>% apply(2,as.numeric) %>% as.tibble()
  option1$date %<>% as.Date()
  return(option1)
  
}

normal_period_combine_option <- function(opt_year) {
  
  all_column_name <- c("date", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
                       , "close", "volume", "Final_p", "OI", "H1", "H2", "H3", "H4", "H5")
  
  TXO_column <- c("date", "LTD", "strike", "c_or_p", "open", "high", "low"
                  , "close", "Final_p", "volume", "OI")
  
  LTD_season <- c(str_c(opt_year,"-03"), str_c(opt_year,"-06"), 
                  str_c(opt_year,"-09"), str_c(opt_year,"-12"))
  
  option1 =read_csv(str_c(opt_year, "_opt_01", ".csv"), locale = locale(encoding = "big5"))
  colnames(option1) <- all_column_name
  
  # 把中文的買賣全轉成call and put
  option1$c_or_p %<>% 
    factor(levels = c("買權", "賣權"), c("call", "put")) 
  
  # 原本的最後交易日的資料型態是integer (像201801), 但我們想轉成日期型態
  option1$LTD %<>% 
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
  option1 %<>%
    filter(contract == "TXO"  & open != "-" & 
             LTD != "NA" & (LTD %in% LTD_season)) %>% 
    select(TXO_column)%>% 
    mutate(weekdays = weekdays(date %>% as.Date()))
  
  
  for (opt_month in c("02", "03", "04", "05", "06", "07",
                      "08", "09", "10", "11", "12")) {
    
    
    
    
    option <- 
      read_csv(str_c(opt_year, "_opt_", opt_month, ".csv"), locale = locale(encoding = "big5"))
    colnames(option) <- all_column_name
    
    # 把中文的買賣全轉成call and put
    option$c_or_p %<>% 
      factor(levels = c("買權", "賣權"), c("call", "put")) 
    
    # 原本的最後交易日的資料型態是integer (像201801), 但我們想轉成日期型態
    option$LTD %<>% 
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
    option %<>%
      filter(contract == "TXO"  & open != "-" & 
               LTD != "NA" & (LTD %in% LTD_season)) %>% 
      select(TXO_column) %>% 
      mutate(weekdays = weekdays(date %>% as.Date()))
    
    
    
    option1 <- rbind(option1, option)
    
    
    
  }
  
  option1[,5:8] %<>% apply(2,as.numeric) %>% as.tibble()
  option1$date %<>% as.Date()
  return(option1)
  
}


season_combine_option <- function(opt_year) {
  
  all_column_name <- c("date", "contract", "LTD", "strike", "c_or_p", "open", "high", "low"
                       , "close", "volume", "Final_p", "OI", "H1", "H2", "H3", "H4", "H5")
  
  TXO_column <- c("date", "LTD", "strike", "c_or_p", "open", "high", "low"
                  , "close", "Final_p", "volume", "OI")
  
  LTD_season <- c(str_c(opt_year,"-03"), str_c(opt_year,"-06"), 
                  str_c(opt_year,"-09"), str_c(opt_year,"-12"))
  
  option1 =read_csv(str_c(opt_year, "_1_opt", ".csv"), locale = locale(encoding = "big5"))
  colnames(option1) <- all_column_name
  
  # 把中文的買賣全轉成call and put
  option1$c_or_p %<>% 
    factor(levels = c("買權", "賣權"), c("call", "put")) 
  
  # 原本的最後交易日的資料型態是integer (像201801), 但我們想轉成日期型態
  option1$LTD %<>% 
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
  option1 %<>%
    filter(contract == "TXO"  & open != "-" & 
             LTD != "NA" & (LTD %in% LTD_season)) %>% 
    select(TXO_column)%>% 
    mutate(weekdays = weekdays(date %>% as.Date()))
  
  
  for (opt_month in c("2", "3", "4")) {
    
    
    
    
    option <- 
      read_csv(str_c(opt_year, "_", opt_month,"_opt", ".csv"), locale = locale(encoding = "big5"))
    colnames(option) <- all_column_name
    
    # 把中文的買賣全轉成call and put
    option$c_or_p %<>% 
      factor(levels = c("買權", "賣權"), c("call", "put")) 
    
    # 原本的最後交易日的資料型態是integer (像201801), 但我們想轉成日期型態
    option$LTD %<>% 
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
    option %<>%
      filter(contract == "TXO"  & open != "-" & 
               LTD != "NA" & (LTD %in% LTD_season)) %>% 
      select(TXO_column) %>% 
      mutate(weekdays = weekdays(date %>% as.Date()))
    
    
    
    option1 <- rbind(option1, option)
    
    
    
  }
  
  option1[,5:8] %<>% apply(2,as.numeric) %>% as.tibble()
  option1$date %<>% as.Date()
  return(option1)
  
}

