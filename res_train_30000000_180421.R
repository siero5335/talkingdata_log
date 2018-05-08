if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 4)

train <- fread("train.csv", skip=154903890, nrows=30000000, 
               col.names =c("ip", "app", "device", "os", "channel", "click_time", 
                            "is_attributed", "ip_nextClick", "ip_app_nextClick", "ip_channel_nextClick", "ip_os_nextClick"),  
               showProgress = FALSE)

invisible(gc())

most_freq_hours_in_test_data <- c("4","5","9","10","13","14")
least_freq_hours_in_test_data <- c("6","11","15")


train[, UsrappCount:=.N, by=list(ip,app,device,os)]
train[, UsrappNewness:=1:.N, by=list(ip,app,device,os)]
train[, UsrappRank:=1:.N,  by=list(ip, app, device, os)]
train[, UsrCount:=.N, by=list(ip,device,os)]
train[, UsrNewness:=1:.N, by=list(ip,device,os)]
train[, app_f := .N, by = "app"]
train[, ip_dev_f := .N, by = "ip,device"]
train[, ip_os_f := .N, by = "ip,os"]
train[, ip_app_f := .N, by = "ip,app"]
train[, ip_app_f := .N, by = "ip,app"]
train[, channel_f := .N, by = "channel"]


train <- train %>% mutate(wday = Weekday(click_time), 
                          hour = hour(click_time),
                          in_test_hh = ifelse(hour %in% most_freq_hours_in_test_data, 1,
                                              ifelse(hour %in% least_freq_hours_in_test_data, 3, 2)))

train$hour <- as.factor(train$hour)
levels(train$hour) <- c(levels(train$hour),'17')
train[train$hour %in% c('16', '17'), 'hour'] <- '17'
train$hour <- droplevels(train$hour)

train$hour <- as.integer(train$hour)

train <- train %>%
  select(-c(click_time)) %>%
  add_count(ip, wday, in_test_hh) %>% rename("nip_day_test_hh" = n) %>%
  select(-c(in_test_hh)) %>%
  add_count(ip, wday, hour) %>% rename("n_ip" = n) %>%
  add_count(ip, wday, hour, os) %>% rename("n_ip_os" = n) %>% 
  add_count(ip, wday, hour, app) %>% rename("n_ip_app" = n) %>%
  add_count(ip, wday, hour, app, os) %>% rename("n_ip_app_os" = n) %>% 
  add_count(app, wday, hour) %>% rename("n_app" = n) %>%
  add_count(device, hour) %>% rename("ndev_h" = n)  %>%
  add_count(ip, device, wday, hour) %>% rename("nip_dev_d_h" = n)  %>%
  add_count(channel, hour, device) %>% rename("nchan_dev_h" = n)  %>%
  select(-c(wday)) %>% select(-c(ip)) 


gc()

app_chan <-  paste(train$app, train$channel, sep = '_')
app_os <-  paste(train$app, train$os, sep = '_')
app_dev <-  paste(train$app, train$device, sep = '_')

chan_os <-  paste(train$channel, train$os, sep = '_')
chan_dev <-  paste(train$channel, train$device, sep = '_')
os_dev <-  paste(train$os, train$device, sep = '_')

train$app_chan <- as.factor(app_chan)
train$app_os <- as.factor(app_os)
train$app_dev  <- as.factor(app_dev)
train$chan_os <- as.factor(chan_os)
train$chan_dev <- as.factor(chan_dev)
train$os_dev <- as.factor(os_dev)

train[is.na(train)] <- 0

train$device <- NULL
train$n_ip_os <- NULL
train$ndev_h <- NULL
train$ip_nextClick  <- NULL
train$ip_os_nextClick <- NULL

app_chan <- NULL
app_dev  <- NULL
app_os   <- NULL
chan_dev <- NULL
chan_os  <- NULL

gc()

library(caret)
set.seed(71)
train_part <- createDataPartition(train$is_attributed, p = 0.6, list = FALSE)

categorical_features = c("app", "os", "channel", "hour", "app_chan", "app_os", "app_dev", "chan_os", "chan_dev", "os_dev")

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
              learning_rate= 0.05, 
              num_leaves= 7,
              max_depth= 3,
              min_child_samples= 100,
              max_bin= 150, # RAM dependent as per LightGBM documentation
              subsample= 0.7,
              subsample_freq= 1,
              colsample_bytree= 0.7,
              min_child_weight= 0,
              min_split_gain= 0,
              scale_pos_weight=99.75) # calculated for this dataset

set.seed(71)
model <- lgb.train(params, dtrain, valids = list(validation = dvalid), nthread = 4,
                   nrounds = 3000, verbose= 1, early_stopping_rounds = 50, eval_freq = 25)


#Predict
test <- fread("test.csv", colClasses = list(numeric=2:6), showProgress = FALSE)

sub <- data.table(click_id = test$click_id, is_attributed = NA) 
test$click_id <- NULL
invisible(gc())


test[, UsrappCount:=.N, by=list(ip,app,device,os)]
test[, UsrappNewness:=1:.N, by=list(ip,app,device,os)]
test[, UsrappRank:=1:.N,  by=list(ip, app, device, os)]
test[, UsrCount:=.N, by=list(ip,device,os)]
test[, UsrNewness:=1:.N, by=list(ip,device,os)]
test[, app_f := .N, by = "app"]
test[, ip_dev_f := .N, by = "ip,device"]
test[, ip_os_f := .N, by = "ip,os"]
test[, ip_app_f := .N, by = "ip,app"]
test[, ip_app_f := .N, by = "ip,app"]
test[, channel_f := .N, by = "channel"]



test <- test %>% mutate(wday = Weekday(click_time), 
                        hour = hour(click_time),
                        in_test_hh = ifelse(hour %in% most_freq_hours_in_test_data, 1,
                                            ifelse(hour %in% least_freq_hours_in_test_data, 3, 2)))

test$hour <- as.factor(test$hour)
levels(test$hour) <- c(levels(test$hour),'17')
test[test$hour %in% c('16', '17'), 'hour'] <- '17'
test$hour <- droplevels(test$hour)

test$hour <- as.integer(test$hour)

test <- test %>%
  add_count(ip, wday, in_test_hh) %>% rename("nip_day_test_hh" = n) %>%
  select(-c(in_test_hh)) %>%
  add_count(ip, wday, hour) %>% rename("n_ip" = n) %>%
  add_count(ip, wday, hour, os) %>% rename("n_ip_os" = n) %>% 
  add_count(ip, wday, hour, app) %>% rename("n_ip_app" = n) %>%
  add_count(ip, wday, hour, app, os) %>% rename("n_ip_app_os" = n) %>% 
  add_count(app, wday, hour) %>% rename("n_app" = n) %>%
  add_count(device, hour) %>% rename("ndev_h" = n)  %>%
  add_count(ip, device, wday, hour) %>% rename("nip_dev_d_h" = n)  %>%
  add_count(channel, hour, device) %>% rename("nchan_dev_h" = n)  %>%
  select(-c(wday)) %>% select(-c(ip)) 


gc()

app_chan <-  paste(test$app, test$channel, sep = '_')
app_os <-  paste(test$app, test$os, sep = '_')
app_dev <-  paste(test$app, test$device, sep = '_')

chan_os <-  paste(test$channel, test$os, sep = '_')
chan_dev <-  paste(test$channel, test$device, sep = '_')
os_dev <-  paste(test$os, test$device, sep = '_')

test$app_chan <- as.factor(app_chan)
test$app_os <- as.factor(app_os)
test$app_dev <- as.factor(app_dev)
test$chan_os <- as.factor(chan_os)
test$chan_dev <- as.factor(chan_dev)   
test$os_dev <- as.factor(os_dev)   

test[is.na(test)] <- 0

test$device <- NULL
test$n_ip_os <- NULL
test$ndev_h <- NULL
test$ip_nextClick  <- NULL
test$ip_os_nextClick <- NULL

app_chan <- NULL
app_dev  <- NULL
app_os   <- NULL
chan_dev <- NULL
chan_os  <- NULL

gc()
preds <- predict(model, data = data.matrix(test[, colnames(test)]), n = model$best_iter)

preds <- as.data.frame(preds)
sub$is_attributed <- NULL
sub$is_attributed <- preds

rm(test)
invisible(gc())
fwrite(sub, "res_train_30000000_180421.csv")

summary(preds$preds)

kable(lgb.importance(model, percentage = TRUE))

#0.9477