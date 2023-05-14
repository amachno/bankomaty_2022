library(dplyr)
library(tidyr)
library(ggplot2)
library(rlist)
library(xgboost)
library(mlbench)
library(caret)
library(e1071)
library(readr)
library(rpart)

setwd("D:/AGH/bankomaty/data")
load("preprocessed_data.Rdt")

df <- list.rbind(data_list) %>% 
  mutate(ID = paste0("atm_", sprintf("sd%02d", ID)) %>% as.factor(),
         weekday = weekday %>% as.factor())



################
## test model  #
#############  #
# xgboost model fit

df_train <- df %>%
  filter(date < '2019-04-26' %>% as.Date()) %>%
  filter(ATM_N > 0) %>% 
  select(-date, 
         -ATM_N) %>%
  na.omit()


df_test <- df %>%
  filter(date >= '2019-04-26' %>% as.Date(),
         date < '2019-05-10' %>% as.Date()) %>%
  filter(ATM_N > 0) %>% 
  select(-date, 
         -ATM_N) %>%
  na.omit()

trctrl <- trainControl(method = "cv", 
                       number = 5,  
                       allowParallel = TRUE,
                       verbose = 2)

mtry <- ncol(df_train) / 3
tunegrid <- expand.grid(.mtry=mtry)
### actual value
then <- Sys.time() 
rf_act_covid_W <- train(log(ATM_W)~., 
                         data=df_train, 
                         method='rf', 
                         metric='MAE', 
                         tuneGrid=tunegrid, 
                         trControl=trctrl,
                         tuneLength = 10)
print(rf_act_covid_W)
print(difftime(Sys.time(), then))
#RMSE
(df_train$ATM_W - exp(predict(rf_act_covid_W, df_train)))^2 %>% mean %>% sqrt

#MAE
(df_train$ATM_W - exp(predict(rf_act_covid_W, df_train))) %>% abs %>% mean 

#MAPE oos
((df_test$ATM_W - exp(predict(rf_act_covid_W, df_test))) / df_test$ATM_W) %>% abs %>% mean
#SMAPE oos
((df_test$ATM_W - exp(predict(rf_act_covid_W, df_test))) / (df_test$ATM_W/2 + exp(predict(rf_act_covid_W, df_test))/2)) %>% abs %>% mean




#############################
####### number
#############################

### actual value

df_train <- df %>%
  filter(date < '2019-04-26' %>% as.Date()) %>%
  filter(ATM_N > 0) %>% 
  select(-date, 
         -ATM_W) %>%
  na.omit()


df_test <- df %>%
  filter(date >= '2019-04-26' %>% as.Date(),
         date < '2019-05-10' %>% as.Date()) %>%
  filter(ATM_N > 0) %>% 
  select(-date, 
         -ATM_W) %>%
  na.omit()
then <- Sys.time() 
rf_act_covid_N <- train(log(ATM_N)~., 
                        data=df_train, 
                        method='rf', 
                        metric='MAE', 
                        tuneGrid=tunegrid, 
                        trControl=trctrl,
                        tuneLength = 10)
print(rf_act_covid_N)
print(difftime(Sys.time(), then))
#RMSE
(df_train$ATM_N - exp(predict(rf_act_covid_N, df_train)))^2 %>% mean %>% sqrt

#MAE
(df_train$ATM_N - exp(predict(rf_act_covid_N, df_train))) %>% abs %>% mean 

#MAPE oos
((df_test$ATM_N - exp(predict(rf_act_covid_N, df_test))) / df_test$ATM_N) %>% abs %>% mean
#SMAPE oos
((df_test$ATM_N - exp(predict(rf_act_covid_N, df_test))) / (df_test$ATM_N/2 + exp(predict(rf_act_covid_N, df_test))/2)) %>% abs %>% mean

save(rf_act_covid_N, rf_act_covid_W, file = rf.Rdt)