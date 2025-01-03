# Libraries ---------------------------------------------------------------
library(magrittr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# Read in Data ------------------------------------------------------------

###Get max number of columns
ncol_max <- max(count.fields("day2/data/day2.txt"))

###Read Data
in_data <- read.table("day2/data/day2.txt", col.names = paste0("V", seq_len(ncol_max)), fill = TRUE) %>%
  mutate(report = 1:nrow(.))

# Part 1 ------------------------------------------------------------------
long_data <- in_data %>%
  pivot_longer(cols = -report) %>%
  filter(!is.na(value)) %>%
  mutate(prevValue = if_else(report == lag(report), lag(value), NA),
         diff_value = value - prevValue,
         diff_sign = sign(diff_value),
         diff_check = if_else(abs(diff_value) >= 1 & abs(diff_value) <= 3, 0, 1),
         prevSign = if_else(report == lag(report), lag(diff_sign), NA),
         sign_check = if_else(diff_sign == prevSign, 0, 1),
         diff_check = if_else(is.na(diff_check), 0, diff_check),
         sign_check = if_else(is.na(sign_check), 0, sign_check),
         error_check = if_else(diff_check + sign_check > 0, 1, 0)) %>%
  select(report, error_check) %>%
  group_by(report) %>%
  summarize(nErrors = sum(error_check)) %>%
  ungroup()

safe_reports <- long_data %>%
  filter(nErrors == 0) %>%
  summarize(total_safe = n())

# Part 2 ------------------------------------------------------------------
f_diff_check <- function(v1, v2) {
  tmp_diff <- v2 - v1
  diff_dummy <- ifelse(abs(tmp_diff) >= 1 & abs(tmp_diff) <= 3, 0, 1)
  
  return(diff_dummy)
}

result_vec <- c()

for (report_number in 1:nrow(in_data)) {
  ## Get Report
  report <- in_data[report_number, ] %>%
    select(-report) %>%
    as.numeric %>%
    discard(is.na)
  
  report_error <- 0
  
  for (i in 0:length(report)) {
    if (i == 0) {
      tmpReport <- report
    } else {
      tmpReport <- report[-c(i)]
    }
    
    for (level in 2:length(tmpReport)) {
      diff_error <- 0
      sign_error <- 0
      
      ### Check Diff
      diff_error <- f_diff_check(tmpReport[level - 1], tmpReport[level])
      
      currSign <- sign(tmpReport[level] - tmpReport[level - 1])  
      
      if (level == 2) {
        prevSign <- currSign
      } else {
        sign_error <- ifelse(currSign == prevSign, 0, 1)
        prevSign <- currSign
      }
      
      if (diff_error != 0 | sign_error != 0) {
        report_error <- 1
        break
      }
      else (
        report_error <- 0
      )
    }
    
    if (report_error == 0) {
      break
    }
  }
  
  result_vec <- c(result_vec, ifelse(report_error == 0, 1, 0))
}

part2sum <- sum(result_vec)
