if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(71)               
options(scipen = 9999, warn = -1, digits= 4)

train <- fread("train_red3.csv", 
               showProgress = FALSE)

train$hour <- NULL

gc()
gc()


most_freq_hours_in_test_data <- c("4","5","9","10","13","14")
least_freq_hours_in_test_data <- c("6","11","15")

train[, app_f := .N, by = "app"]
train[, channel_f := .N, by = "channel"]

train <- train %>% mutate(wday = Weekday(click_time), 
                          hour = hour(click_time),
                          in_test_hh = ifelse(hour %in% most_freq_hours_in_test_data, 1,
                                              ifelse(hour %in% least_freq_hours_in_test_data, 3, 2))) %>%
  select(-c(click_time)) %>%
  add_count(ip, wday, in_test_hh) %>% rename("nip_day_test_hh" = n) %>%
  select(-c(in_test_hh))

invisible(gc())
invisible(gc())



categorical_features = c("app", "device", "os", "channel", "hour", "app_f", "channel_f")

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
              learning_rate= 0.05, 
              num_leaves= 7,
              max_depth= 3,
              min_child_samples= 100,
              max_bin= 100, # RAM dependent as per LightGBM documentation
              subsample= 0.7,
              subsample_freq= 1,
              colsample_bytree= 0.7,
              min_child_weight= 0,
              min_split_gain= 0,
              scale_pos_weight=200) # calculated for this dataset

set.seed(71)
model <- lgb.train(params, dtrain, valids = list(validation = dvalid), nthread = 7,
                   nrounds = 2000, verbose= 1, early_stopping_rounds = 30, eval_freq = 25)

kable(lgb.importance(model, percentage = TRUE))





#Predict
test <- fread("test2.csv", colClasses = list(numeric=2:6), showProgress = FALSE)

sub <- data.table(click_id = test$click_id, is_attributed = NA) 
test$click_id <- NULL
invisible(gc())

test[, app_f := .N, by = "app"]
test[, channel_f := .N, by = "channel"]

gc()
gc()


test <- test %>% mutate(wday = Weekday(click_time), 
                          hour = hour(click_time),
                          in_test_hh = ifelse(hour %in% most_freq_hours_in_test_data, 1,
                                              ifelse(hour %in% least_freq_hours_in_test_data, 3, 2))) %>%
  select(-c(click_time)) %>%
  add_count(ip, wday, in_test_hh) %>% rename("nip_day_test_hh" = n) %>%
  select(-c(in_test_hh))

invisible(gc())
invisible(gc())



preds <- predict(model, data = data.matrix(test[, colnames(test)]), n = model$best_iter)

preds <- as.data.frame(preds)
sub$is_attributed <- NULL
sub$is_attributed <- preds

rm(test)
invisible(gc())
fwrite(sub, "res_train_31800000_180430_2.csv")
