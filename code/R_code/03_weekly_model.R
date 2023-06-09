library(readr)
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

df <- list.rbind(data_list) %>% 
  mutate(ID = paste0("atm_", sprintf("sd%02d", ID)) %>% as.factor(),
         weekday = weekday %>% as.factor()) %>%
  filter(ATM_N > 0)

df$week <- 0
 
for (i in 1:(nrow(df) - 1)){
  if (df$weekday[i+1] == "Mon"){
    df$week[i+1] <- df$week[i] + 1
  } else {
    df$week[i+1] <- df$week[i]
  }
}

df_week <- 
  df %>%
  arrange(week, ID) %>%
  group_by(week, ID) %>%
  summarise(count = n(),
            monthday_start = first(monthday),
            date = min(date),
            ATM_W_sum = sum(ATM_W),
            ATM_N_sum = sum(ATM_N),
            ATM_W_sd = sd(ATM_W),
            ATM_N_sd = sd(ATM_N)) %>%
  filter(count == 7) %>%
  ungroup() %>%
  mutate(lag_ATM_W_sum = lag(ATM_W_sum),
         lag_ATM_N_sum = lag(ATM_N_sum),
         lag_ATM_W_sd = lag(ATM_W_sd),
         lag_ATM_N_sd = lag(ATM_N_sd),
         lag2_ATM_W_sum = lag(ATM_W_sum, 2),
         lag2_ATM_N_sum = lag(ATM_N_sum, 2),
         lag2_ATM_W_sd = lag(ATM_W_sd, 2),
         lag2_ATM_N_sd = lag(ATM_N_sd, 2),
         lag3_ATM_W_sum = lag(ATM_W_sum, 3),
         lag3_ATM_N_sum = lag(ATM_N_sum, 3),
         lag3_ATM_W_sd = lag(ATM_W_sd, 3),
         lag3_ATM_N_sd = lag(ATM_N_sd, 3),
         lag4_ATM_W_sum = lag(ATM_W_sum, 4),
         lag4_ATM_N_sum = lag(ATM_N_sum, 4),
         lag4_ATM_W_sd = lag(ATM_W_sd, 4),
         lag4_ATM_N_sd = lag(ATM_N_sd, 4),
         lag_ATM_W_sum4 = lag(ATM_W_sum, 4),
         lag_ATM_N_sum4 = lag_ATM_W_sum + lag4_ATM_W_sum + lag3_ATM_W_sum + lag4_ATM_W_sum,
         lag_ATM_N_sum4 = lag_ATM_N_sum + lag4_ATM_N_sum + lag3_ATM_N_sum + lag4_ATM_N_sum)

for (i in 1:10){
  df_week[paste0("sd", sprintf("%02d", i))] <- 
            sapply(1:dim(df_week)[1],
                   function(j) {
                     sum(df[df$ID == df_week$ID[j] & df$week == df_week$week[j], paste0("sd", sprintf("%02d", i))])
                   }
            )
  print(Sys.time())
  print(i)
}  

write_csv(df_week, "df_week_no_0W.csv")

# data split
set.seed(20220116)
df_precovid <- df_week %>%
  filter(date < '2020-04-01' %>% as.Date()) %>%
  select(-date, 
         -ATM_N_sum,
         -week,
         -count,
         -ATM_N_sd, 
         -ATM_W_sd) %>%
  na.omit()

df_covid <- df_week %>%
  filter(date >= '2020-04-01' %>% as.Date()) %>%
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
                        repeats=3)

### actual
then <- Sys.time()
mtry <- sqrt(ncol(df_precovid))
tunegrid <- expand.grid(.mtry=mtry)
rf_act_covid <- train(ATM_W_sum~., 
                      data=df_precovid, 
                      method='rf', 
                      metric='RMSE', 
                      tuneGrid=tunegrid, 
                      trControl=control)
print(rf_act_covid)
print(difftime(Sys.time(), then))


