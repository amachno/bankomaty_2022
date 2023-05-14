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

tune_grid <- expand.grid(nrounds = 100,
                         max_depth = c(10,15),
                         eta = c(0.1, 0.1),
                         gamma = c(0.01, 0.005),
                         colsample_bytree = seq(0.4, 0.8, length.out = 3),
                         min_child_weight = c(0, .5, 1),
                         subsample = c(0.7, 1)
)
### actual value
then <- Sys.time() 
xgb_act_covid_W <- train(log(ATM_W)~., 
                       data=df_train, 
                       method='xgbTree', 
                       metric='MAE', 
                       tuneGrid=tune_grid, 
                       trControl=trctrl,
                       tuneLength = 10)
print(xgb_act_covid)
print(difftime(Sys.time(), then))
save(xgb_act_covid, file = "xgb_act_covid.Rdt")
#RMSE
(df_train$ATM_W - exp(predict(xgb_act_covid, df_train)))^2 %>% mean %>% sqrt

#MAE
(df_train$ATM_W - exp(predict(xgb_act_covid, df_train))) %>% abs %>% mean 

#MAPE oos
((df_test$ATM_W - exp(predict(xgb_act_covid, df_test))) / df_test$ATM_W) %>% abs %>% mean
#SMAPE oos
((df_test$ATM_W - exp(predict(xgb_act_covid, df_test))) / (df_test$ATM_W/2 + exp(predict(xgb_act_covid, df_test))/2)) %>% abs %>% mean
