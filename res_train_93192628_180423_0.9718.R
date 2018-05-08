if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(84)               
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
  add_count(ip, device, wday, hour) %>% rename("nip_dev_d_h" = n)  %>%
  select(-c(wday)) %>% select(-c(ip)) 


gc()

train[is.na(train)] <- 0



gc()

library(caret)
set.seed(71)
train_part <- createDataPartition(train$is_attributed, p = 0.7, list = FALSE)

categorical_features = c("app", "os", "channel", "hour")

dtrain = lgb.Dataset(data.matrix(train[train_part,] %>% select(-is_attributed)), 
                     label = as.numeric(train[train_part,]$is_attributed),
                     categorical_feature = categorical_features)



dvalid = lgb.Dataset(data.matrix(train[-train_part,] %>% select(-is_attributed)), 
                     label = as.numeric(train[-train_part,]$is_attributed),
                     categorical_feature = categorical_features)

rm(train)
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
                   nrounds = 1000, verbose= 1, early_stopping_rounds = 50, eval_freq = 25)

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
  add_count(ip, device, wday, hour) %>% rename("nip_dev_d_h" = n)  %>%
  select(-c(wday)) %>% select(-c(ip)) 


gc()

test[is.na(test)] <- 0



gc()

preds <- predict(model, data = data.matrix(test[, colnames(test)]), n = model$best_iter)

preds <- as.data.frame(preds)
sub$is_attributed <- NULL
sub$is_attributed <- preds

rm(test)
invisible(gc())
fwrite(sub, "res_train_93192628_180423.csv")

summary(preds$preds)

kable(lgb.importance(model, percentage = TRUE))

#0.9730