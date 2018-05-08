pacman::p_load(knitr, tidyverse, highcharter, caret, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(71)               
options(scipen = 9999, warn = -1, digits= 4)



train <- fread("train.csv", 
               col.names =c("ip", "app", "device", "os", "channel", "click_time", 
                            "attributed_time", "is_attributed"),  
               showProgress = FALSE) %>% select(-c(attributed_time)) 

trainV1 <- fread("trainV1.csv")

trainV1$V1 <- NULL
trainV1$day <- NULL
trainV1$hour <- NULL
trainV1$minute <- NULL
trainV1$second <- NULL

train <- data.frame(train, trainV1)

fwrite(train,"train.csv")



test <- fread("test.csv", showProgress = FALSE)

testV1 <- fread("testV1.csv")

testV1$V1 <- NULL
testV1$click_id <- NULL

test <- data.frame(test, testV1)

fwrite(test,"test.csv")

