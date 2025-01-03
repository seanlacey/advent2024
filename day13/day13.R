###Load libraries as needed here
library(dplyr)
library(stringr)

###Load data
input_file <- "input.txt"

in_data <- scan(file = paste0("day13/data/",input_file), "character", sep = "\n",
                blank.lines.skip = TRUE) |>
  matrix(ncol = 3, byrow = TRUE) |>
  as.data.frame()

###String cleaning function
string_clean <- function(x) {
  return(str_replace_all(str_remove_all(x, ","), "[\\+\\=]", " "))
}

###Parse columns
data_mat_part1 <- in_data |>
  mutate(id = row_number(),
         V1_clean = string_clean(V1),
         a_x = as.numeric(word(V1_clean,4)),
         a_y = as.numeric(word(V1_clean,6)),
         V2_clean = string_clean(V2),
         b_x = as.numeric(word(V2_clean,4)), 
         b_y = as.numeric(word(V2_clean,6)),
         V3_clean = string_clean(V3),
         prize_x = as.numeric(word(V3_clean,3)),
         prize_y = as.numeric(word(V3_clean,5)),
         press_b = (a_y * prize_x - a_x * prize_y) / (a_y * b_x - a_x * b_y),
         press_a = (prize_x - (b_x * press_b)) / a_x,
         press_b_check = if_else(round(press_b) == press_b & press_b <= 100, press_b, NA),
         press_a_check = if_else(round(press_a) == press_a & press_a <= 100, press_a, NA),
         tokens = if_else(!is.na(press_b_check) & !is.na(press_a_check), press_a_check * 3 + press_b_check * 1, NA)) |>
  select(id, a_x, a_y, b_x, b_y, prize_x, prize_y,
         press_a, press_b, press_b_check, press_a_check, tokens)


#Part 1 ------------------------------------------------------------------
part1answer <- sum(data_mat_part1$tokens, na.rm = TRUE)

#Part 2 ------------------------------------------------------------------
###Parse columns
data_mat_part2 <- in_data |>
  mutate(id = row_number(),
         V1_clean = string_clean(V1),
         a_x = as.numeric(word(V1_clean,4)),
         a_y = as.numeric(word(V1_clean,6)),
         V2_clean = string_clean(V2),
         b_x = as.numeric(word(V2_clean,4)), 
         b_y = as.numeric(word(V2_clean,6)),
         V3_clean = string_clean(V3),
         prize_x = as.numeric(word(V3_clean,3)) + 10000000000000,
         prize_y = as.numeric(word(V3_clean,5)) + 10000000000000,
         press_b = (a_y * prize_x - a_x * prize_y) / (a_y * b_x - a_x * b_y),
         press_a = (prize_x - (b_x * press_b)) / a_x,
         press_b_check = if_else(round(press_b) == press_b, press_b, NA),
         press_a_check = if_else(round(press_a) == press_a, press_a, NA),
         tokens = if_else(!is.na(press_b_check) & !is.na(press_a_check), press_a_check * 3 + press_b_check * 1, NA)) |>
  select(id, a_x, a_y, b_x, b_y, prize_x, prize_y,
         press_a, press_b, press_b_check, press_a_check, tokens)

part2answer <- sum(data_mat_part2$tokens, na.rm = TRUE)
