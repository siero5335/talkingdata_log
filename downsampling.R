pacman::p_load(knitr, tidyverse, highcharter, caret, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(71)               
options(scipen = 9999, warn = -1, digits= 4)

train <- fread("train.csv", 
               col.names =c("ip", "app", "device", "os", "channel", "click_time", 
                            "is_attributed", "ip_nextClick", "ip_app_nextClick", "ip_channel_nextClick", "ip_os_nextClick"), 
               showProgress = FALSE)

train$is_attributed <- as.factor(train$is_attributed)

set.seed(71)  
train1 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train1, "train1.csv")
rm(train1)
gc()
gc()

set.seed(42)  
train2 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train2, "train2.csv")
rm(train2)
gc()
gc()

set.seed(84)  
train3 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train3, "train3.csv")
rm(train3)
gc()
gc()

set.seed(114514)  
train4 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train4, "train4.csv")
rm(train4)
gc()
gc()

set.seed(1145141919)  
train5 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train5, "train5.csv")
rm(train5)
gc()
gc()

set.seed(96473)  
train6 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train6, "train6.csv")
rm(train6)
gc()
gc()

set.seed(876)  
train7 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train7, "train7.csv")
rm(train7)
gc()
gc()

set.seed(346)  
train8 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train8, "train8.csv")
rm(train8)
gc()
gc()


set.seed(765)  
train9 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train9, "train9.csv")
rm(train9)
gc()
gc()


set.seed(961)  
train10 <- downSample(train[, -'is_attributed'], train$is_attributed, list = FALSE, yname = "is_attributed")

write.csv(train10, "train10.csv")
rm(train10)
gc()
gc()


rm(train)
gc()
gc()
