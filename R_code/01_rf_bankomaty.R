library(dplyr)
library(tidyr)
library(ggplot2)
library(rlist)
library(randomForest)
library(mlbench)
library(caret)
library(e1071)

setwd("D:/AGH/bankomaty/data")

load("preprocessed_data.Rdt")

# df <- list.rbind(data_list) %>% 
#   mutate(ID = paste0("atm_", sprintf("sd%02d", ID)),
#          dummy = 1) %>%
#   spread(key = ID, value = dummy, fill = 0) 

df <- list.rbind(data_list) %>% 
  mutate(ID = paste0("atm_", sprintf("sd%02d", ID)) %>% as.factor(),
         weekday = weekday %>% as.factor())

############train before COVID
control <- trainControl(method='repeatedcv', 
                        number=5, 
                        repeats=3)
set.seed(20220116)
df_precovid <- df %>%
  filter(date < '2020-04-01' %>% as.Date()) %>%
  select(-date, - ATM_N) 

df_covid <- df %>%
  filter(date >= '2020-04-01' %>% as.Date()) %>%
  select(-date, - ATM_N) 
### actual
then <- Sys.time()
mtry <- sqrt(ncol(df_precovid))
tunegrid <- expand.grid(.mtry=mtry)
rf_act_covid <- train(ATM_W~., 
                data=df_precovid, 
                method='rf', 
                metric='RMSE', 
                tuneGrid=tunegrid, 
                trControl=control)
print(rf_act_covid)
print(difftime(Sys.time(), then))
### increment
# then <- Sys.time()
# mtry <- sqrt(ncol(df_precovid))
# tunegrid <- expand.grid(.mtry=mtry)
# rf_inc_covid <- train(ATM_W~., 
#                 data=df_precovid %>% 
#                   mutate(ATM_W = ATM_W - lag_1d_w), 
#                 method='rf', 
#                 metric='RMSE', 
#                 tuneGrid=tunegrid, 
#                 trControl=control)
# print(rf_inc_covid)
# print(difftime(Sys.time(), then))
########tests
#then <- Sys.time()
#tmp_rf <- train(ATM_W~., data = df_tmp, method = "rf")
#print(difftime(Sys.time(), then))

#tmp_rf$modelInfo

### RMSE covid
### actual
(df_covid$ATM_W - predict(rf_act_covid, df_covid))^2 %>% mean %>% sqrt
### increment
# (df_covid$ATM_W - df_covid$lag_1d_w - predict(rf_inc_covid, df_covid))^2 %>% mean %>% sqrt

### MAE covid
### actual
(df_covid$ATM_W - predict(rf_act_covid, df_covid)) %>% abs %>% mean
### increment
# (df_covid$ATM_W - df_covid$lag_1d_w - predict(rf_inc_covid, df_covid)) %>% abs %>% mean
