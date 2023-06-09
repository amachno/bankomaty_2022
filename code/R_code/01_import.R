library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(RcppRoll)

setwd("D:/AGH/bankomaty_2022/data/modelling_data")

data_raw <- read_csv("atm_data.csv") %>%
  rename(date=Values)
data_special_days <- read_csv("specialDays.csv")

data_special_days <- 
  data_special_days %>%
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
  arrange(date)
names(data_special_days)[2:12] <- sprintf("sd%02d", 0:10)

## add lag and lead variables
for (i in 1:10){
  data_special_days[[sprintf("lag_sd%02d", i)]] = lag(data_special_days[[sprintf("sd%02d", i)]])
  data_special_days[[sprintf("lead_sd%02d", i)]] = lead(data_special_days[[sprintf("sd%02d", i)]])
}

data_raw <- 
  data_raw %>%
  left_join(data_special_days)

data_list <- list()

nums_atm <- 1:81
atm_ids <- sprintf("%02d", nums_atm)

for (i in nums_atm){
  data_tmp <- 
    data_raw %>% 
    dplyr::select(date, dplyr::ends_with(atm_ids[i]), sd01:sd10, lag_sd01:lag_sd10, lead_sd01:lead_sd10) %>%
    dplyr::arrange(date)
  
  names(data_tmp)[2:3] <- c("ATM_W", "ATM_N")
  
  data_tmp <- 
    data_tmp %>%
    mutate(lag_1w_w = roll_sum(x = ATM_W, 7, align = "right", fill = NA), 
           lag_1w_n = roll_sum(x = ATM_N, 7, align = "right", fill = NA),
           lag_1d_w = lag(ATM_W),
           lag_1d_n = lag(ATM_N),
           lag_1w_w = lag(lag_1w_w),
           lag_1w_n = lag(lag_1w_n),
           weekday = format(date, format = "%a"),
           monthday = format(date, format = "%d") %>% 
             as.numeric(),
           ID = i
           ) %>% 
    na.omit()
  data_list[[atm_ids[i]]] <- data_tmp
}

save(data_list, file = "preprocessed_data.Rdt")
