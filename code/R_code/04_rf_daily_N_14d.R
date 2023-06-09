setwd("D:/AGH/bankomaty/data")
load("preprocessed_data.Rdt")

df <- list.rbind(data_list) %>% 
  mutate(ID = paste0("atm_", sprintf("sd%02d", ID)) %>% as.factor(),
         weekday = weekday %>% as.factor())


dates <- c("2019-04-26", "2020-04-26", 
           "2019-08-01", "2020-08-01", 
           "2019-10-11", "2020-10-11", 
           "2019-12-15", "2020-12-15", 
           "2020-01-18", "2021-01-18"
)

trctrl <- trainControl(method = "cv", 
                       number = 5,  
                       allowParallel = TRUE,
                       verbose = 2)



test_df_list <- lapply(dates, function(dt){
  df %>%
    filter(date >= as.Date(dt),
           date < as.Date(dt) + 14) %>%
    na.omit() %>%
    filter(ATM_N > 0)
})

### numbers
train_df_N_list <- lapply(dates, function(dt){
  df %>%
    filter(date < as.Date(dt),
           date >= as.Date(dt) - 365) %>%
    filter(ATM_N > 0) %>% 
    select(-date, 
           -ATM_W) %>%
    na.omit()
})

mtry <- ncol(train_df_N_list[[1]]) / 3
tune_grid <- expand.grid(.mtry=mtry)

rf_act_covid_N_list <- 
  lapply(1:length(dates), 
         function(i){
           then <- Sys.time() 
           print(length(dates) - i)
           print(dates[i])
           res <- train(log(ATM_N)~., 
                        data=train_df_N_list[[i]], 
                        method='rf', 
                        metric='MAE', 
                        tuneGrid=tune_grid, 
                        trControl=trctrl,
                        tuneLength = 10)
           print(res)
           print(difftime(Sys.time(), then))
           return(res)
         })

### values
train_df_W_list <- lapply(dates, function(dt){
  df %>%
    filter(date < as.Date(dt),
           date >= as.Date(dt) - 365) %>%
    filter(ATM_N > 0) %>% 
    select(-date, 
           -ATM_N) %>%
    na.omit()
})

rf_act_covid_W_list <- 
  lapply(1:length(dates), 
         function(i){
           then <- Sys.time() 
           print(length(dates) - i)
           print(dates[i])
           res <- train(log(ATM_W)~., 
                        data=train_df_W_list[[i]], 
                        method='rf', 
                        metric='MAE', 
                        tuneGrid=tune_grid, 
                        trControl=trctrl,
                        tuneLength = 10)
           print(res)
           print(difftime(Sys.time(), then))
           return(res)
         })
######################
###predictions########
######################

prediction_list <- 
  lapply(1:length(dates), 
         function(i){
           df_tmp <- df %>%
             mutate(pred_W = ATM_W,
                    pred_N = ATM_N)
           for(j in 0:13){
             df_tmp$pred_N[df_tmp$date == (as.Date(dates[i]) + j)] <- 
               predict(rf_act_covid_N_list[[i]], 
                       df_tmp[df_tmp$date == (as.Date(dates[i]) + j), ]
               )
             df_tmp$pred_W[df_tmp$date == as.Date(dates[i]) + j] <- 
               predict(rf_act_covid_W_list[[i]], 
                       df_tmp[df_tmp$date == (as.Date(dates[i]) + j), ]
               )
             df_tmp %>%
               mutate(lag_1w_w = roll_sum(x = ATM_W, 7, align = "right", fill = NA), 
                      lag_1w_n = roll_sum(x = ATM_N, 7, align = "right", fill = NA),
                      lag_1d_w = lag(ATM_W),
                      lag_1d_n = lag(ATM_N),
                      lag_1w_w = lag(lag_1w_w),
                      lag_1w_n = lag(lag_1w_n)
               )
           }
           return(df_tmp %>% filter(date >= as.Date(dates[i]),
                                    date < as.Date(dates[i]) + 14)
           )
         }
  )

setwd("D:/AGH/bankomaty/results")
for(i in 1:length(dates)){
  dt = min(prediction_list[[i]]$date)
  write_csv(prediction_list[[i]] %>% 
              select(date,
                     ID,
                     ATM_W,
                     ATM_N,
                     pred_N,
                     pred_W), 
            file = paste0("prognozy_rf", dt, ".csv"))
}
