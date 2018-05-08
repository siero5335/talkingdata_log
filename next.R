if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 4)

train <- fread("train_reduce_2.csv", 
               col.names =c("ip", "app", "device", "os", "channel", "click_time", 
                            "is_attributed", "ip_nextClick", "ip_app_nextClick", "ip_channel_nextClick", "ip_os_nextClick"),  
               showProgress = FALSE)

invisible(gc())

most_freq_hours_in_test_data <- c("4","5","9","10","13","14")
least_freq_hours_in_test_data <- c("6","11","15")

train$ip_nextClick  <- NULL
train$ip_os_nextClick <- NULL

train <- train %>% mutate(wday = Weekday(click_time), 
                          hour = hour(click_time))


prd <- train %>%
  group_by(wday, hour) %>%
  summarise(
    mean_app = mean(app, na.rm = T),
    sd_app = sd(app, na.rm = T)
  )