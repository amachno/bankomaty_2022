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

df_week <- read_csv("df_week.csv")

# data split
set.seed(20220116)
df_train <- df_week %>%
  filter(date > '2020-04-01' %>% as.Date()) %>%
  filter(date < '2020-10-12' %>% as.Date()) %>%
  select(-date, 
         -ATM_W_sum,
         -week,
         -count,
         -ATM_N_sd, 
         -ATM_W_sd) %>%
  na.omit() %>%
  filter(ATM_N_sum > 0)

df_test <- df_week %>%
  filter(date == '2020-10-12' %>% as.Date()) %>% 
  filter(ID != "atm_sd77",
         ID != "atm_sd04",
         ID != "atm_sd81") %>%
  select(-date, 
         -ATM_W_sum,
         -week,
         -count,
         -ATM_N_sd, 
         -ATM_W_sd)  %>%
  na.omit()


# xgboost model fit
trctrl <- trainControl(method = "cv", 
                       number = 5,  
                       allowParallel = TRUE,
                       verbose = 2)

tune_grid <- expand.grid(nrounds = 100,
                         max_depth = c(3,5,10,15),
                         eta = c(0.001, 0.1, 0.1),
                         gamma = c(0.01, 0.005),
                         colsample_bytree = seq(0.2, 1, length.out = 5),
                         min_child_weight = c(0, .5, 1, 2),
                         subsample = c(0.5, 0.7, 1)
)
### actual value
then <- Sys.time() 
xgb_act_covid <- train(log(ATM_N_sum)~., 
                       data=df_train, 
                       method='xgbTree', 
                       metric='MAE', 
                       tuneGrid=tune_grid, 
                       trControl=trctrl,
                       tuneLength = 10)
print(xgb_act_covid)
print(difftime(Sys.time(), then))2

#RMSE
(df_train$ATM_N_sum - exp(predict(xgb_act_covid, df_train)))^2 %>% mean %>% sqrt

#MAE
(df_train$ATM_N_sum - exp(predict(xgb_act_covid, df_train))) %>% abs %>% mean 

#MAPE oos
((df_test$ATM_N_sum - exp(predict(xgb_act_covid, df_test))) / df_test$ATM_N_sum) %>% abs %>% mean
#SMAPE oos
((df_test$ATM_N_sum - exp(predict(xgb_act_covid, df_test))) / (df_test$ATM_N_sum/2 + exp(predict(xgb_act_covid, df_test))/2)) %>% abs %>% mean

