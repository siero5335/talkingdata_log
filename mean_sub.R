if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 4)

test1 <- fread("imbalanced_data.csv",  showProgress = FALSE)
test2 <- fread("res_train_92000000_180425.csv",  showProgress = FALSE)
test3 <- fread("sub_it7.csv",  showProgress = FALSE)
test4 <- fread("sub_it23.csv",  showProgress = FALSE)
test5 <- fread("submission_hm2.csv",  showProgress = FALSE)
test6 <- fread("wordbatch_fm_ftrl.csv",  showProgress = FALSE)

df <- (test1$is_attributed +
  test2$is_attributed +
  test3$is_attributed +
  test4$is_attributed +
  test5$is_attributed +
  test6$is_attributed )/6

sub <- data.frame(test1$click_id, df)
colnames(sub) <- c("click_id","is_attributed")
fwrite(sub, "Mean6_180503.csv")
