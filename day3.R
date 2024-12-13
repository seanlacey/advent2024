library(magrittr)
library(dplyr)
library(stringr)

data_in <- readLines("day3/data/day3.txt", warn = FALSE)

# Part 1 ------------------------------------------------------------------
data_extract <- str_extract_all(data_in, "mul\\(\\d*\\,\\d*\\)") %>%
  unlist

f_mult <- function(x) {
  nums <- x %>%
    str_replace("mul\\(","") %>%
    str_replace("\\)","")
  
  mult1 <- word(nums, 1, sep = ",") %>%
    as.numeric
  
  mult2 <- word(nums, 2, sep = ",") %>%
    as.numeric
  
  return(mult1 * mult2)
}

all_mults <- sapply(data_extract, f_mult)

part1sum <- sum(all_mults)

# Part 2 ------------------------------------------------------------------
data_extract <- str_extract_all(data_in, "mul\\(\\d*\\,\\d*\\)|do\\(\\)|don't\\(\\)") %>%
  unlist

do_yes <- 1
rm_vec <- c()

for(i in 1:length(data_extract)) {
  if (data_extract[i] == "do()") {
    do_yes <- 1
  } else if (data_extract[i] == "don't()") {
    do_yes <- 0
  } 
  
  if (do_yes == 0 | data_extract[i] == "do()" | data_extract[i] == "don't()") {
    rm_vec <- c(rm_vec,i)
  }
}

data_filtered <- data_extract[-rm_vec]

filter_mults <- sapply(data_filtered, f_mult)

part2sum <- sum(filter_mults)
