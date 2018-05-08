if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(71)               
options(scipen = 9999, warn = -1, digits= 4)

train <- fread("train_reduce.csv", 
               col.names =c("ip", "app", "device", "os", "channel", "click_time", 
                            "is_attributed", "ip_nextClick", "ip_app_nextClick", "ip_channel_nextClick", "ip_os_nextClick"),  
               showProgress = FALSE)

invisible(gc())

most_freq_hours_in_test_data <- c("4","5","9","10","13","14")
least_freq_hours_in_test_data <- c("6","11","15")

train[, UsrappCount:=.N, by=list(ip,app,device,os)]
train[, app_f := .N, by = "app"]
train[, ip_dev_f := .N, by = "ip,device"]
train[, ip_app_f := .N, by = "ip,app"]



train <- train %>% mutate(wday = Weekday(click_time), 
                          hour = hour(click_time),
                          in_test_hh = ifelse(hour %in% most_freq_hours_in_test_data, 1,
                                              ifelse(hour %in% least_freq_hours_in_test_data, 3, 2))) %>%
  select(-c(click_time)) %>%
  add_count(ip, wday, in_test_hh) %>% rename("nip_day_test_hh" = n) %>%
  select(-c(in_test_hh)) %>%
  add_count(os, device) %>% rename("n_os_dev" = n) %>%
  add_count(os, app, channel) %>% rename("n_os_app_chan" = n) %>%
  add_count(app, channel) %>% rename("n_app_chan" = n) %>%
  add_count(ip, wday, hour) %>% rename("n_ip" = n) %>%
  add_count(ip, wday, hour, os) %>% rename("n_ip_os" = n) %>% 
  add_count(ip, wday, hour, app) %>% rename("n_ip_app" = n) %>%
  add_count(ip, wday, hour, app, os) %>% rename("n_ip_app_os" = n) %>% 
  add_count(app, wday, hour) %>% rename("n_app" = n) %>%
  add_count(ip, device, wday, hour) %>% rename("nip_dev_d_h" = n)


temp <-  train %>% 
  group_by(ip, wday, channel) %>%
  summarise(
    mean_h_ip_wday_chan = mean(hour),
    sd_h_ip_wday_chan = sd(hour))

train <- train %>%
  left_join(temp, by = c("ip", "wday", "channel"))

temp2 <-  train %>% 
  group_by(ip, app, os) %>%
  summarise(
    mean_h_ip_app_os = mean(hour),
    sd_h_ip_app_os = sd(hour))

train <- train %>%
  left_join(temp2, by = c("ip", "app", "os"))

temp3 <-  train %>% 
  group_by(ip, app, channel) %>%
  summarise(
    mean_h_ip_app_chan = mean(hour),
    sd_h_ip_app_chan = sd(hour))

train <- train %>%
  left_join(temp3, by = c("ip", "app", "channel")) %>%
  select(-c(ip)) 

rm(temp, temp2, temp3)
invisible(gc())

library(caret)
categorical_features = c("app", "device", "os", "channel", "hour")

train1 <- filter(train, wday < 4)
train2 <- train1 %>% 
 select(-c(is_attributed, wday))
               
dtrain = lgb.Dataset(data.matrix(train2),
                     label = as.numeric(train1$is_attributed),
                     categorical_feature = categorical_features)

rm(train1, train2)
invisible(gc())

train3 <- filter(train, wday == 4)
train4 <- train3 %>% 
  select(-c(is_attributed, wday))
         

dvalid = lgb.Dataset(data.matrix(train4), 
                     label = as.numeric(train3$is_attributed),
                     categorical_feature = categorical_features)

rm(train, train4, train3)
invisible(gc())

params = list(objective = "binary", 
              metric = "auc", 
              learning_rate= 0.1, 
              num_leaves= 7,
              max_depth= 3,
              min_child_samples= 100,
              max_bin= 255, # RAM dependent as per LightGBM documentation
              subsample= 0.7,
              subsample_freq= 1,
              colsample_bytree= 0.7,
              min_child_weight= 0,
              min_split_gain= 0,
              scale_pos_weight=99.75) # calculated for this dataset

set.seed(71)
model <- lgb.train(params, dtrain, valids = list(validation = dvalid), nthread = 7,
                   nrounds = 2000, verbose= 1, early_stopping_rounds = 30, eval_freq = 25)

kable(lgb.importance(model, percentage = TRUE))


#Predict
test <- fread("test.csv", colClasses = list(numeric=2:6), showProgress = FALSE)

sub <- data.table(click_id = test$click_id, is_attributed = NA) 
test$click_id <- NULL
invisible(gc())


test[, UsrappCount:=.N, by=list(ip,app,device,os)]
test[, app_f := .N, by = "app"]
test[, ip_dev_f := .N, by = "ip,device"]
test[, ip_app_f := .N, by = "ip,app"]



test <- test %>% mutate(wday = Weekday(click_time), 
                          hour = hour(click_time),
                          in_test_hh = ifelse(hour %in% most_freq_hours_in_test_data, 1,
                                              ifelse(hour %in% least_freq_hours_in_test_data, 3, 2))) %>%
  select(-c(click_time)) %>%
  add_count(ip, wday, in_test_hh) %>% rename("nip_day_test_hh" = n) %>%
  select(-c(in_test_hh)) %>%
  add_count(os, device) %>% rename("n_os_dev" = n) %>%
  add_count(os, app, channel) %>% rename("n_os_app_chan" = n) %>%
  add_count(app, channel) %>% rename("n_app_chan" = n) %>%
  add_count(ip, wday, hour) %>% rename("n_ip" = n) %>%
  add_count(ip, wday, hour, os) %>% rename("n_ip_os" = n) %>% 
  add_count(ip, wday, hour, app) %>% rename("n_ip_app" = n) %>%
  add_count(ip, wday, hour, app, os) %>% rename("n_ip_app_os" = n) %>% 
  add_count(app, wday, hour) %>% rename("n_app" = n) %>%
  add_count(ip, device, wday, hour) %>% rename("nip_dev_d_h" = n)  

temp <-  test %>% 
  group_by(ip, wday, channel) %>%
  summarise(
    mean_h_ip_wday_chan = mean(hour),
    sd_h_ip_wday_chan = sd(hour))

test <- test %>%
  left_join(temp, by = c("ip", "wday", "channel"))

temp2 <-  test %>% 
  group_by(ip, app, os) %>%
  summarise(
    mean_h_ip_app_os = mean(hour),
    sd_h_ip_app_os = sd(hour))

test <- test %>%
  left_join(temp2, by = c("ip", "app", "os"))

temp3 <-  test %>% 
  group_by(ip, app, channel) %>%
  summarise(
    mean_h_ip_app_chan = mean(hour),
    sd_h_ip_app_chan = sd(hour))

test <- test %>%
  left_join(temp3, by = c("ip", "app", "channel")) %>%
  select(-c(wday)) %>% select(-c(ip)) 
rm(temp, temp2, temp3)
gc()

preds <- predict(model, data = data.matrix(test[, colnames(test)]), n = model$best_iter)

preds <- as.data.frame(preds)
sub$is_attributed <- NULL
sub$is_attributed <- preds

rm(test)
invisible(gc())
fwrite(sub, "res_train_92000000_180425.csv")

summary(preds$preds)



#0.9728