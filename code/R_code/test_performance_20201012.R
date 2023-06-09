library(dplyr)
library(tidyr)
library(ggplot2)
library(rlist)
library(randomForest)
library(mlbench)
library(caret)
library(e1071)
library(readr)

setwd("D:/AGH/bankomaty/data")

df_week <- read_csv("df_week.csv")

# data split
set.seed(20220116)
df_train <- df_week %>%
  filter(date > '2020-04-01' %>% as.Date()) %>%
  filter(date < '2020-10-12' %>% as.Date()) %>%
  select(-date, 
         -ATM_N_sum,
         -week,
         -count,
         -ATM_N_sd, 
         -ATM_W_sd) %>%
  na.omit()

df_test <- df_week %>%
  filter(date == '2020-10-12' %>% as.Date()) %>% 
   # filter(ID != "atm_sd77",
   #        ID != "atm_sd04") %>%
  select(-date, 
         -ATM_N_sum,
         -week,
         -count,
         -ATM_N_sd, 
         -ATM_W_sd)  %>%
  na.omit()


# RF model fit
control <- trainControl(method='repeatedcv', 
                        number=5, 
                        repeats=3, 
                        search = 'random')

### actual value
then <- Sys.time()
mtry <- ncol(df_train)/3
tunegrid <- expand.grid(.mtry = mtry) 
rf_act_covid <- train(ATM_W_sum~., 
                      data=df_train, 
                      method='rf', 
                      metric='RMSE', 
                      tuneGrid=tunegrid, 
                      trControl=control,
                      nodesize=3)
print(rf_act_covid)
print(difftime(Sys.time(), then))

(df_train$ATM_W_sum - predict(rf_act_covid, df_train))^2 %>% mean %>% sqrt

#MAPE oos
((df_test$ATM_W_sum - predict(rf_act_covid, df_test)) / df_test$ATM_W_sum) %>% mean
#SMAPE oos
((df_test$ATM_W_sum - predict(rf_act_covid, df_test)) / (df_test$ATM_W_sum/2 + predict(rf_act_covid, df_test)/2)) %>% mean

### increment value

then <- Sys.time()
mtry <- sqrt(ncol(df_train))
tunegrid <- expand.grid(.mtry=mtry)
rf_inc_covid <- train(ATM_W_sum~., 
                      data=df_train %>% 
                        mutate(ATM_W_sum = ATM_W_sum - lag_ATM_W_sum), 
                      method='rf', 
                      metric='RMSE', 
                      tuneGrid=tunegrid, 
                      trControl=control)
print(rf_inc_covid)
print(difftime(Sys.time(), then))

(df_train$ATM_W_sum - df_train$lag_ATM_W_sum - predict(rf_inc_covid, df_train))^2 %>% mean %>% sqrt

#MAPE oos
((df_test$ATM_W_sum  - df_test$lag_ATM_W_sum - predict(rf_inc_covid, df_test)) / df_test$ATM_W_sum) %>% mean
#SMAPE oos
((df_test$ATM_W_sum - df_test$lag_ATM_W_sum - predict(rf_inc_covid, df_test)) / (df_test$ATM_W_sum/2 + predict(rf_inc_covid, df_test)/2)) %>% mean

