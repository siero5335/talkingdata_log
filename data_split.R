if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)

train <- fread("train.csv", 
               col.names =c("ip", "app", "device", "os", "channel", "click_time", 
                            "is_attributed", "ip_nextClick", "ip_app_nextClick", "ip_channel_nextClick", "ip_os_nextClick"),  
               showProgress = FALSE)

gc()

library(caret)
set.seed(71)
train_part <- createDataPartition(train$is_attributed, p = 0.75, list = FALSE)


gc()
gc()

train_reduce <- as.data.frame(train[train_part,])

fwrite(train_reduce, "train_reduce.csv")

rm(train_reduce)

gc()
gc()

train_reduce_2 <- as.data.frame(train[-train_part,])
fwrite(train_reduce_2, "train_reduce_2.csv")


gc()
gc()

