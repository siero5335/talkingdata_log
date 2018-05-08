if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 5)

train <- fread("train.csv", skip=124903890, nrows=20000000, 
               col.names =c("ip", "app", "device", "os", "channel", "click_time", 
                            "is_attributed", "ip_nextClick", "ip_app_nextClick", "ip_channel_nextClick", "ip_os_nextClick"),  
               showProgress = FALSE)
               
               most_freq_hours_in_test_data <- c("4","5","9","10","13","14")
               least_freq_hours_in_test_data <- c("6","11","15")
               
               
               train[, UsrappCount:=.N, by=list(ip,app,device,os)]
               train[, UsrappNewness:=1:.N, by=list(ip,app,device,os)]
               train[, UsrappRank:=1:.N,  by=list(ip, app, device, os)]
               train[, UsrCount:=.N, by=list(ip,device,os)]
               train[, UsrNewness:=1:.N, by=list(ip,device,os)]
               train[, UsrRank:=1:.N,     by=list(ip, os, device)]
               
               
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
               
               app_tar <- as.data.frame(table(train$app, train$is_attributed)[,2]/ (table(train$app, train$is_attributed)[,1] + table(train$app, train$is_attributed)[,2] + 1))
               colnames(app_tar) <- c("app_tar")
               app_tar$app <- as.factor(rownames(app_tar))
               train$app <-  as.factor(train$app)
               
               
               device_tar <- as.data.frame(table(train$device, train$is_attributed)[,2]/ (table(train$device, train$is_attributed)[,1] + table(train$device, train$is_attributed)[,2] + 1))
               colnames(device_tar) <- c("device_tar")
               device_tar$device <- as.factor(rownames(device_tar))
               train$device <-  as.factor(train$device)
               
               
               os_tar <- as.data.frame(table(train$os, train$is_attributed)[,2]/ (table(train$os, train$is_attributed)[,1] + table(train$os, train$is_attributed)[,2] + 1))
               colnames(os_tar) <- c("os_tar")
               os_tar$os <- as.factor(rownames(os_tar))
               train$os <-  as.factor(train$os)
               
               
               channel_tar <- as.data.frame(table(train$channel, train$is_attributed)[,2]/ (table(train$channel, train$is_attributed)[,1] + table(train$channel, train$is_attributed)[,2] + 1))
               colnames(channel_tar) <- c("channel_tar")
               channel_tar$channel <- as.factor(rownames(channel_tar))
               train$channel <-  as.factor(train$channel)
               
               
               hour_tar <- as.data.frame(table(train$hour, train$is_attributed)[,2]/ (table(train$hour, train$is_attributed)[,1] + table(train$hour, train$is_attributed)[,2] + 1))
               colnames(hour_tar) <- c("hour_tar")
               hour_tar$hour <- as.factor(rownames(hour_tar))
               train$hour <-  as.factor(train$hour)
               
               train <- inner_join(train, app_tar, by = "app")
               train <- inner_join(train, device_tar, by = "device")
               train <- inner_join(train, os_tar, by = "os")
               train <- inner_join(train, channel_tar, by = "channel")
               train <- inner_join(train, hour_tar, by = "hour")
               
               
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
               
               app_chan_tar <- as.data.frame(table(train$app_chan, train$is_attributed)[,2]/ (table(train$app_chan, train$is_attributed)[,1] + table(train$app_chan, train$is_attributed)[,2] + 1))
               colnames(app_chan_tar) <- c("app_chan_tar")
               app_chan_tar$app_chan <- as.factor(rownames(app_chan_tar))
               
               app_os_tar <- as.data.frame(table(train$app_os, train$is_attributed)[,2]/ (table(train$app_os, train$is_attributed)[,1] + table(train$app_os, train$is_attributed)[,2] + 1))
               colnames(app_os_tar) <- c("app_os_tar")
               app_os_tar$app_os <- as.factor(rownames(app_os_tar))
               
               app_dev_tar <- as.data.frame(table(train$app_dev, train$is_attributed)[,2]/ (table(train$app_dev, train$is_attributed)[,1] + table(train$app_dev, train$is_attributed)[,2] + 1))
               colnames(app_dev_tar) <- c("app_dev_tar")
               app_dev_tar$app_dev <- as.factor(rownames(app_dev_tar))
               
               chan_os_tar <- as.data.frame(table(train$chan_os, train$is_attributed)[,2]/ (table(train$chan_os, train$is_attributed)[,1] + table(train$chan_os, train$is_attributed)[,2] + 1))
               colnames(chan_os_tar) <- c("chan_os_tar")
               chan_os_tar$chan_os <- as.factor(rownames(chan_os_tar))
               
               chan_dev_tar <- as.data.frame(table(train$chan_dev, train$is_attributed)[,2]/ (table(train$chan_dev, train$is_attributed)[,1] + table(train$chan_dev, train$is_attributed)[,2] + 1))
               colnames(chan_dev_tar) <- c("chan_dev_tar")
               chan_dev_tar$chan_dev <- as.factor(rownames(chan_dev_tar))
               
               os_dev_tar <- as.data.frame(table(train$os_dev, train$is_attributed)[,2]/ (table(train$os_dev, train$is_attributed)[,1] + table(train$os_dev, train$is_attributed)[,2] + 1))
               colnames(os_dev_tar) <- c("os_dev_tar")
               os_dev_tar$os_dev <- as.factor(rownames(os_dev_tar))
               
               
               train <- inner_join(train, app_chan_tar, by = "app_chan")
               train <- inner_join(train, app_os_tar, by = "app_os")
               train <- inner_join(train, app_dev_tar, by = "app_dev")
               train <- inner_join(train, chan_os_tar, by = "chan_os")
               train <- inner_join(train, chan_dev_tar, by = "chan_dev")
               train <- inner_join(train, os_dev_tar, by = "os_dev")
               
               train$app_chan <- NULL
               train$app_os <- NULL
               train$app_dev  <- NULL
               train$chan_os <- NULL
               train$chan_dev <- NULL
               train$os_dev <- NULL
               
               library(caret)
               set.seed(71)
               train_part <- createDataPartition(train$is_attributed, p = 0.8, list = FALSE)
               
               categorical_features = c("app", "device", "os", "channel", "hour")
               
               dtrain = lgb.Dataset(data.matrix(train[train_part,] %>% select(-is_attributed)), 
                                    label = as.numeric(train[train_part,]$is_attributed),
                                    categorical_feature = categorical_features)
               
               
               
               dvalid = lgb.Dataset(data.matrix(train[-train_part,] %>% select(-is_attributed)), 
                                    label = as.numeric(train[-train_part,]$is_attributed),
                                    categorical_feature = categorical_features)
               
               rm(train)
               rm(valid)
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
               model <- lgb.train(params, dtrain, valids = list(validation = dvalid), nthread = 7,
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
               
               test <- inner_join(test, app_chan_tar, by = "app_chan")
               test <- inner_join(test, app_os_tar, by = "app_os")
               test <- inner_join(test, app_dev_tar, by = "app_dev")
               test <- inner_join(test, chan_os_tar, by = "chan_os")
               test <- inner_join(test, chan_dev_tar, by = "chan_dev")
               test <- inner_join(test, os_dev_tar, by = "os_dev")
               
               test$app_chan <- NULL
               test$app_os <- NULL
               test$app_dev <- NULL
               test$chan_os <- NULL
               test$chan_dev <- NULL
               test$os_dev <- NULL
               
               preds <- predict(model, data = data.matrix(test[, colnames(test)]), n = model$best_iter)
               
               preds <- as.data.frame(preds)
               sub$is_attributed <- NULL
               sub$is_attributed <- preds
               
               rm(test)
               invisible(gc())
               fwrite(sub, "res_train_20000000_180420.csv")
               
               summary(preds$preds)
               
               kable(lgb.importance(model, percentage = TRUE))
               