library(magrittr)
library(dplyr)
library(purrr)

### Total Distnace
in_data <- read.table("~/Documents/GitHub/advent2024/day1/data/input.txt")

list1 <- in_data[, 1] %>%
  sort

list2 <- in_data[, 2] %>%
  sort

total_distance <- as.numeric(sum(abs(list1-list2)))


# Similarity Score --------------------------------------------------------
### Create Table of Frequencies of list2
freq_list2 <- table(list2)

###Create Function for Apply
f_score <- function(x, flist = freq_list2) {

  score <- x * flist[names(flist) == x]
  
  return(score)
}

score_list <- as.numeric(sapply(list1, f_score))

total_score <- sum(score_list, na.rm = TRUE)
