if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, highcharter, data.table, lubridate, pROC, tictoc, DescTools, lightgbm)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 4)

test2 <- fread("submission_final.csv",  showProgress = FALSE)
test3 <- fread("sub_it6.csv",  showProgress = FALSE)
test4 <- fread("sub_it23.csv",  showProgress = FALSE)
test5 <- fread("submission_hm2.csv",  showProgress = FALSE)
test6 <- fread("wordbatch_fm_ftrl.csv",  showProgress = FALSE)

df <- (test2$is_attributed +
  test3$is_attributed +
  test4$is_attributed +
  test5$is_attributed +
  test6$is_attributed )/5

sub <- data.frame(test2$click_id, df)
colnames(sub) <- c("click_id","is_attributed")
fwrite(sub, "Mean6_180506_1.csv")
