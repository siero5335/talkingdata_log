pacman::p_load(knitr, tidyverse, highcharter, caret, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(71)               
options(scipen = 9999, warn = -1, digits= 5)


train1 <- fread("train1.csv", colClasses = list(numeric=1:5),
               showProgress = FALSE) %>% select(-c(V1)) 

most_freq_hours_in_test_data <- c("4","5","9","10","13","14")
least_freq_hours_in_test_data <- c("6","11","15")


train1[, UsrappCount:=.N, by=list(ip,app,device,os)]
train1[, UsrappNewness:=1:.N, by=list(ip,app,device,os)]
train1[, UsrappRank:=1:.N,  by=list(ip, app, device, os)]
train1[, UsrCount:=.N, by=list(ip,device,os)]
train1[, UsrNewness:=1:.N, by=list(ip,device,os)]
train1[, UsrRank:=1:.N,     by=list(ip, os, device)]


train1 <- train1 %>% mutate(wday = Weekday(click_time), 
                    hour = hour(click_time),
                    in_test_hh = ifelse(hour %in% most_freq_hours_in_test_data, 1,
                                        ifelse(hour %in% least_freq_hours_in_test_data, 3, 2)))


train1$click_time <- as.POSIXct(train1$click_time, orders='%Y-%m-%d %H:%M:%S')

library(reticulate)
np <- import("numpy")
pd <- import("pandas")



train1$hour <- as.factor(train1$hour)
levels(train1$hour) <- c(levels(train1$hour),'17')
train1[train1$hour %in% c('16', '17'), 'hour'] <- '17'
train1$hour <- droplevels(train1$hour)

train1$hour <- as.integer(train1$hour)

train1 <- train1 %>%
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

app_tar <- as.data.frame(table(train1$app, train1$is_attributed)[,2]/ (table(train1$app, train1$is_attributed)[,1] + table(train1$app, train1$is_attributed)[,2] + 1))
colnames(app_tar) <- c("app_tar")
app_tar$app <- as.factor(rownames(app_tar))
train1$app <-  as.factor(train1$app)


device_tar <- as.data.frame(table(train1$device, train1$is_attributed)[,2]/ (table(train1$device, train1$is_attributed)[,1] + table(train1$device, train1$is_attributed)[,2] + 1))
colnames(device_tar) <- c("device_tar")
device_tar$device <- as.factor(rownames(device_tar))
train1$device <-  as.factor(train1$device)


os_tar <- as.data.frame(table(train1$os, train1$is_attributed)[,2]/ (table(train1$os, train1$is_attributed)[,1] + table(train1$os, train1$is_attributed)[,2] + 1))
colnames(os_tar) <- c("os_tar")
os_tar$os <- as.factor(rownames(os_tar))
train1$os <-  as.factor(train1$os)


channel_tar <- as.data.frame(table(train1$channel, train1$is_attributed)[,2]/ (table(train1$channel, train1$is_attributed)[,1] + table(train1$channel, train1$is_attributed)[,2] + 1))
colnames(channel_tar) <- c("channel_tar")
channel_tar$channel <- as.factor(rownames(channel_tar))
train1$channel <-  as.factor(train1$channel)


hour_tar <- as.data.frame(table(train1$hour, train1$is_attributed)[,2]/ (table(train1$hour, train1$is_attributed)[,1] + table(train1$hour, train1$is_attributed)[,2] + 1))
colnames(hour_tar) <- c("hour_tar")
hour_tar$hour <- as.factor(rownames(hour_tar))
train1$hour <-  as.factor(train1$hour)

train1 <- inner_join(train1, app_tar, by = "app")
train1 <- inner_join(train1, device_tar, by = "device")
train1 <- inner_join(train1, os_tar, by = "os")
train1 <- inner_join(train1, channel_tar, by = "channel")
train1 <- inner_join(train1, hour_tar, by = "hour")


app_chan <-  paste(train1$app, train1$channel, sep = '_')
app_os <-  paste(train1$app, train1$os, sep = '_')
app_dev <-  paste(train1$app, train1$device, sep = '_')

chan_os <-  paste(train1$channel, train1$os, sep = '_')
chan_dev <-  paste(train1$channel, train1$device, sep = '_')
os_dev <-  paste(train1$os, train1$device, sep = '_')

train1$app_chan <- as.factor(app_chan)
train1$app_os <- as.factor(app_os)
train1$app_dev  <- as.factor(app_dev)
train1$chan_os <- as.factor(chan_os)
train1$chan_dev <- as.factor(chan_dev)
train1$os_dev <- as.factor(os_dev)

app_chan_tar <- as.data.frame(table(train1$app_chan, train1$is_attributed)[,2]/ (table(train1$app_chan, train1$is_attributed)[,1] + table(train1$app_chan, train1$is_attributed)[,2] + 1))
colnames(app_chan_tar) <- c("app_chan_tar")
app_chan_tar$app_chan <- as.factor(rownames(app_chan_tar))

app_os_tar <- as.data.frame(table(train1$app_os, train1$is_attributed)[,2]/ (table(train1$app_os, train1$is_attributed)[,1] + table(train1$app_os, train1$is_attributed)[,2] + 1))
colnames(app_os_tar) <- c("app_os_tar")
app_os_tar$app_os <- as.factor(rownames(app_os_tar))

app_dev_tar <- as.data.frame(table(train1$app_dev, train1$is_attributed)[,2]/ (table(train1$app_dev, train1$is_attributed)[,1] + table(train1$app_dev, train1$is_attributed)[,2] + 1))
colnames(app_dev_tar) <- c("app_dev_tar")
app_dev_tar$app_dev <- as.factor(rownames(app_dev_tar))

chan_os_tar <- as.data.frame(table(train1$chan_os, train1$is_attributed)[,2]/ (table(train1$chan_os, train1$is_attributed)[,1] + table(train1$chan_os, train1$is_attributed)[,2] + 1))
colnames(chan_os_tar) <- c("chan_os_tar")
chan_os_tar$chan_os <- as.factor(rownames(chan_os_tar))

chan_dev_tar <- as.data.frame(table(train1$chan_dev, train1$is_attributed)[,2]/ (table(train1$chan_dev, train1$is_attributed)[,1] + table(train1$chan_dev, train1$is_attributed)[,2] + 1))
colnames(chan_dev_tar) <- c("chan_dev_tar")
chan_dev_tar$chan_dev <- as.factor(rownames(chan_dev_tar))

os_dev_tar <- as.data.frame(table(train1$os_dev, train1$is_attributed)[,2]/ (table(train1$os_dev, train1$is_attributed)[,1] + table(train1$os_dev, train1$is_attributed)[,2] + 1))
colnames(os_dev_tar) <- c("os_dev_tar")
os_dev_tar$os_dev <- as.factor(rownames(os_dev_tar))


train1 <- inner_join(train1, app_chan_tar, by = "app_chan")
train1 <- inner_join(train1, app_os_tar, by = "app_os")
train1 <- inner_join(train1, app_dev_tar, by = "app_dev")
train1 <- inner_join(train1, chan_os_tar, by = "chan_os")
train1 <- inner_join(train1, chan_dev_tar, by = "chan_dev")
train1 <- inner_join(train1, os_dev_tar, by = "os_dev")

train1$app_chan <- NULL
train1$app_os <- NULL
train1$app_dev  <- NULL
train1$chan_os <- NULL
train1$chan_dev <- NULL
train1$os_dev <- NULL

library(caret)
set.seed(71)
train_part <- createDataPartition(train1$is_attributed, p = 0.8, list = FALSE)

categorical_features = c(1:4, 12)

dtrain = lgb.Dataset(data.matrix(train1[train_part,] %>% select(-is_attributed)), 
                     label = as.numeric(train1[train_part,]$is_attributed),
                     categorical_feature = categorical_features)


dvalid = lgb.Dataset(data.matrix(train1[-train_part,] %>% select(-is_attributed)), 
                     label = as.numeric(train1[-train_part,]$is_attributed),
                     categorical_feature = categorical_features)

rm(valid)
invisible(gc())

params = list(objective = "binary", 
              metric = "auc", 
              learning_rate= 0.03,
              num_leaves= 7,
              max_depth= 4,
              min_child_samples= 100,
              max_bin= 100,
              subsample= 0.7, 
              subsample_freq= 1,
              colsample_bytree= 0.7,
              min_child_weight= 0,
              min_split_gain= 0)

model <- lgb.train(params, dtrain, valids = list(validation = dvalid), nthread = 4,
                   nrounds = 5000, verbose= 1, early_stopping_rounds = 50, eval_freq = 25)


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
test[, UsrRank:=1:.N,     by=list(ip, os, device)]


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
  select(-c(click_time)) %>%
  add_count(ip, wday, in_test_hh) %>% rename("nip_day_test_hh" = n) %>%
  select(-c(in_test_hh)) %>%
  add_count(ip, wday, hour) %>% rename("n_ip" = n) %>%
  add_count(ip, wday, hour, os) %>% rename("n_ip_os" = n) %>% 
  add_count(ip, wday, hour, app) %>% rename("n_ip_app" = n) %>%
  add_count(ip, wday, hour, app, os) %>% rename("n_ip_app_os" = n) %>% 
  add_count(app, wday, hour) %>% rename("n_app" = n) %>%
  select(-c(wday)) %>% select(-c(ip)) 


gc()

test$app <- as.factor(test$app)
test$device <- as.factor(test$device)
test$os <- as.factor(test$os)
test$channel <- as.factor(test$channel)
test$hour <- as.factor(test$hour)

test <- inner_join(test, app_tar, by = "app")
test <- inner_join(test, device_tar, by = "device")
test <- inner_join(test, os_tar, by = "os")
test <- inner_join(test, channel_tar, by = "channel")
test <- inner_join(test, hour_tar, by = "hour")


preds <- predict(model, data = data.matrix(test[, colnames(test)]), n = model$best_iter)
preds <- as.data.frame(preds)
sub$is_attributed <- NULL
sub$is_attributed <- preds

rm(test)
invisible(gc())
fwrite(sub, "res_train1_downsamp_180418_3.csv")

summary(preds$preds)


