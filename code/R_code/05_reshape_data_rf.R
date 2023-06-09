AI
library(tidyr)

setwd("D:/AGH/bankomaty/results")

dates <- c("2019-04-26", "2020-04-26", 
           "2019-08-01", "2020-08-01", 
           "2019-10-11", "2020-10-11", 
           "2019-12-15", "2020-12-15", 
           "2020-01-18", "2021-01-18"
)

for(i in 1:length(dates)){
  dt = dates[i]
  df <- read_csv(file = paste0("prognozy_rf", dt, ".csv"))
  atms_no <- length(unique(df$ID))
  
  error_W <- 
    df %>% select(date, ID, ATM_W, pred_W) %>%
    mutate(ID = substr(ID, 7, 8),
           pred_W = exp(pred_W),
           date = as.character(date),
           ATM_W = ifelse(ATM_W <= 0, NA, ATM_W)) %>%
    group_by(ID) %>%
    summarize(MAPE = mean(abs(ATM_W - pred_W) / ATM_W, na.rm = T), 
              SMAPE = mean(2 * abs(ATM_W - pred_W) / (ATM_W + pred_W), na.rm = T)) 
  
  df %>% select(date, ID, ATM_W, pred_W) %>%
    mutate(ID = substr(ID, 7, 8),
           pred_W = exp(pred_W),
           date = as.character(date)) %>% 
    pivot_wider(
      names_from = ID,
      values_from = c(ATM_W, pred_W)
    ) %>% 
    select(1, c(1, atms_no + 1) + rep(1:atms_no, each = 2)) %>% 
    rbind(c("MAPE", rep(error_W$MAPE, each=2))) %>%
    rbind(c("SMAPE", rep(error_W$SMAPE, each=2))) %>%
    write_csv(file = paste0("prognozy_rf_W_", dt, ".csv"))
  
  error_N <- 
    df %>% select(date, ID, ATM_N, pred_N) %>%
    mutate(ID = substr(ID, 7, 8),
           pred_N = exp(pred_N),
           date = as.character(date),
           ATM_N = ifelse(ATM_N <= 0, NA, ATM_N)) %>%
    group_by(ID) %>%
    summarize(MAPE = mean(abs(ATM_N - pred_N) / ATM_N, na.rm = T), 
              SMAPE = mean(2 * abs(ATM_N - pred_N) / (ATM_N + pred_N), na.rm = T)) 
  
  df %>% select(date, ID, ATM_N, pred_N) %>%
    mutate(ID = substr(ID, 7, 8),
           pred_N = exp(pred_N),
           date = as.character(date)) %>% 
    pivot_wider(
      names_from = ID,
      values_from = c(ATM_N, pred_N)
    ) %>% 
    select(1, c(1, atms_no + 1) + rep(1:atms_no, each = 2)) %>%
    rbind(c("MAPE", rep(error_N$MAPE, each=2))) %>%
    rbind(c("SMAPE", rep(error_N$SMAPE, each=2))) %>%
    write_csv(file = paste0("prognozy_rf_N_", dt, ".csv"))
}

