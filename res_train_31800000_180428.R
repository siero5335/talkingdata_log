if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(71)               
options(scipen = 9999, warn = -1, digits= 4)

train <- fread("train_red3.csv", 
               showProgress = FALSE)


X <- copy(train)[, -c("click_time", "attributed_time", "nextClick", "is_attributed"), with=F]
fea <- c("ip", "app", "device", "os", "channel")
for (f in fea) {
  levels <- sort(names(which(table(X[[f]]) > 300)))
  X[[f]] <- factor(X[[f]], levels=levels)
}

colnames(X) <- c("ip_F", "app_F", "device_F", "os_F", "channel_F")

train <- data.frame(train, X[, 2:5])

rm(X)

gc()
gc()

train2 <- fread("train_reduce_2.csv", 
               col.names =c("ip", "app", "device", "os", "channel", "click_time",
                            "is_attributed" , "nextClick"),  
               showProgress = FALSE)


most_freq_hours_in_test_data <- c("4","5","9","10","13","14")
least_freq_hours_in_test_data <- c("6","11","15")


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
  add_count(ip, wday, hour, app, os) %>% rename("n_ip_app_os" = n)

invisible(gc())
invisible(gc())

temp <-  train %>% 
  group_by(ip, app, os) %>%
  summarise(
    mean_h_ip_app_os = mean(hour),
    sd_h_ip_app_os = sd(hour))

train <- train %>%
  left_join(temp, by = c("ip", "app", "os")) %>%
  select(-c(ip)) 

rm(temp)
invisible(gc())


categorical_features = c("app", "device", "os", "channel", "hour", 
                         "app_F", "device_F", "os_F", "channel_F",
                         "app_SD", "device_SD", "os_SD", "channel_SD")

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

X <- copy(test)[, -c("click_time", "attributed_time", "nextClick", "is_attributed"), with=F]
fea <- c("ip", "app", "device", "os", "channel")
for (f in fea) {
  levels <- sort(names(which(table(X[[f]]) > 300)))
  X[[f]] <- factor(X[[f]], levels=levels)
}

colnames(X) <- c("ip_F", "app_F", "device_F", "os_F", "channel_F")

test <- data.frame(test, X[, 2:5])

rm(X)

gc()
gc()

test2 <- fread("test.csv", 
                col.names =c("ip", "app", "device", "os", "channel", "click_time",
                             "is_attributed" , "nextClick"),  
                showProgress = FALSE)

test2[, (fea) := lapply(.SD, as.factor), .SDcols = fea
       ][, (fea) := lapply(.SD, fct_lump, prop=0.002), .SDcols = fea
         ][, c("click_time", "is_attributed") := NULL]

colnames(test2) <- c("ip_SD", "app_SD", "device_SD", "os_SD", "channel_SD" , "nextClick")



test <- data.frame(test, test2[, 2:5])

rm(test2)

gc()
gc()

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
  add_count(ip, wday, hour, app, os) %>% rename("n_ip_app_os" = n)

invisible(gc())
invisible(gc())

temp <-  test %>% 
  group_by(ip, app, os) %>%
  summarise(
    mean_h_ip_app_os = mean(hour),
    sd_h_ip_app_os = sd(hour))

test <- test %>%
  left_join(temp, by = c("ip", "app", "os")) %>%
  select(-c(ip)) 

rm(temp)
invisible(gc())


preds <- predict(model, data = data.matrix(test[, colnames(test)]), n = model$best_iter)

preds <- as.data.frame(preds)
sub$is_attributed <- NULL
sub$is_attributed <- preds

rm(test)
invisible(gc())
fwrite(sub, "res_train_31800000_180426.csv")
