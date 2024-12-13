library(magrittr)
library(dplyr)
library(tidyr)

### Read Data
in_data <- read.table("day4/data/day4.txt", sep="") 

num_row <- as.numeric(nrow(in_data))
num_col <- as.numeric(nchar(in_data$V1[1]))

separate_data <- in_data %>%
  separate(V1, into = paste0("COL", 1:num_col), sep = 1:(num_col - 1))


# Part 1-------------------------------------------------------------------------

### Define function that searches around found X's 
### and returns number of "MAS" that are found

letter_scan <- function(r, c) {
  xmas_count <- 0
  
  for (r_move in -1:1) {
    r1 <- r  + r_move
    r2 <- r1 + r_move
    r3 <- r2 + r_move
    
    ### If the row to search would be ineligible, skip this iteration
    if (r1 < 1 | r1 > num_row | 
        r2 < 1 | r2 > num_row | 
        r3 < 1 | r3 > num_row) {next}
    
    for (c_move in -1:1) {
      c1 <- c + c_move
      c2 <- c1 + c_move
      c3 <- c2 + c_move
      
      ### If col to search would be ineligible (or the one we're searching from), skip this iteration
      if (c1 < 1 | c1 > num_col |
          c2 < 1 | c2 > num_col | 
          c3 < 1 | c3 > num_col |
          (r_move == 0 & c_move == 0)) {next}
      
      letter1 <- separate_data[r1, c1]
      letter2 <- separate_data[r2, c2]
      letter3 <- separate_data[r3, c3]
      
      if (letter1 == "M" & letter2 == "A" & letter3 == "S") {
        xmas_count <- xmas_count + 1
      }
    }
  }
  
  return(xmas_count)
}

xmas_count_vec <- c()

for (r in 1:num_row) {
  for (c in 1:num_col) {
    ###If X, scan surrounding for M then A then S
    if (separate_data[r,c] == "X") {
      xmas_count_vec <- c(xmas_count_vec, letter_scan(r, c))
    }
  }  
}

total_xmas <- sum(xmas_count_vec)

# Part 2 ------------------------------------------------------------------
### Define function that searches around found A's 
### and returns number of "x-mas" that are found

diag_check <- function(r, c) {
  ### X-mas can't exist at the boundaries
  if (r == 1 | c == 1 | r == num_row | c == num_col) {
    return(0)
  }
  
  ### Define coordinates of all 4 corners
  ### d will represent which diagonal and r/c will be row/col
  d1r1 <- r - 1
  d1c1 <- c - 1
  d1r2 <- r + 1
  d1c2 <- c + 1
  
  d2r1 <- r + 1
  d2c1 <- c - 1
  d2r2 <- r - 1
  d2c2 <- c + 1

  ###Grab letters
  d1letter1 <- separate_data[d1r1, d1c1]
  d1letter2 <- separate_data[d1r2, d1c2]
  
  d2letter1 <- separate_data[d2r1, d2c1]
  d2letter2 <- separate_data[d2r2, d2c2]
  
  ###Check if x-mas is made
  if (((d1letter1 == "M" & d1letter2 == "S") | (d1letter1 == "S" & d1letter2 == "M")) &
      ((d2letter1 == "M" & d2letter2 == "S") | (d2letter1 == "S" & d2letter2 == "M"))) {
    return(1)
  } else (
    return(0)
  )
}

x_mas_count_vec <- c()

for (r in 1:num_row) {
  for (c in 1:num_col) {
    ###If A, check diagonals
    if (separate_data[r,c] == "A") {
      x_mas_count_vec <- c(x_mas_count_vec, diag_check(r, c))
    }
  }  
}

total_x_mas <- sum(x_mas_count_vec)
