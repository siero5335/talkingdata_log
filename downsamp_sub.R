if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 4)

test1 <- fread("res_dawn1.csv",  showProgress = FALSE)
test2 <- fread("res_dawn2.csv",  showProgress = FALSE)
test3 <- fread("res_dawn3.csv",  showProgress = FALSE)
test4 <- fread("res_dawn4.csv",  showProgress = FALSE)
test5 <- fread("res_dawn5.csv",  showProgress = FALSE)
test6 <- fread("res_dawn6.csv",  showProgress = FALSE)
test7 <- fread("res_dawn7.csv",  showProgress = FALSE)
test8 <- fread("res_dawn8.csv",  showProgress = FALSE)
test9 <- fread("res_dawn9.csv",  showProgress = FALSE)
test10 <- fread("res_dawn10.csv",  showProgress = FALSE)

df <- (test1$is_attributed +
  test2$is_attributed +
  test3$is_attributed +
  test4$is_attributed +
  test5$is_attributed +
  test6$is_attributed +
  test7$is_attributed +
  test8$is_attributed +
  test9$is_attributed + 
  test10$is_attributed)/10

sub <- data.frame(test1$click_id, df)
colnames(sub) <- c("click_id","is_attributed")
fwrite(sub, "downsamp10_mean_180502.csv")
