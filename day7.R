library(dplyr)

###Read input
in_text <- gsub(":", "", readLines("day7/data/input.txt", warn = FALSE)) |> as.data.frame()

in_data <- stringr::str_split_fixed(in_text[ , 1], " ", Inf) |> as.data.frame()

names(in_data) <- c("total", paste0("VAL", 1:(length(in_data) - 1)))

# Part 1 ------------------------------------------------------------------
operator_vec <- c("+", "*")

###Function that evaluates the equation vector left to right
vec_eval <- function(eq_vec) {
  new_vec <- c(eval(parse(text = paste(eq_vec[1:3], collapse = " "))), eq_vec[-(1:3)])
  
  if (length(new_vec) == 1) {
    return(new_vec)
  } else {
    vec_eval(new_vec)
  }
}

math_check <- function(x) {
  x <- x |>
    as.numeric() |>
    na.omit()
  
  trueI <<- trueI + 1
  print(trueI)
  
  total_val <- x[1]
  val_vec <- x[-1]
  
  num_val <- length(val_vec) |> as.numeric()
  
  ###Number of operator locations
  num_spots <- length(val_vec) - 1
  
  ###Number of possible combinations
  num_possible <- 2 ** num_spots
  
  ###Get all possible operator permutations
  operator_set <- gtools::permutations(2, num_spots, operator_vec, repeats = TRUE)
  
  result_val <- 0
  
  ###Get equations and evaluate
  for (i in 1:num_possible) {
    eq_vec <- c(rbind(val_vec, c(operator_set[i, ], "")))[1:(num_spots + num_val)]
    
    eq_result <- as.numeric(vec_eval(eq_vec))
    
    if (as.numeric(total_val) == eq_result) {
      result_val <- eq_result
      
      break
    }
  }
  
  return(result_val)
}

trueI <- 0
result_out <- apply(in_data, 1, math_check)

part1answer <- sum(result_out)

# Part 2 ------------------------------------------------------------------
operator_vec <- c("+", "*", "||")

###Vec Function
###Function that evaluates the equation vector left to right
vec_eval_part2 <- function(eq_vec) {
  first_vec <- eq_vec[1:3]
  second_vec <- eq_vec[-(1:3)]
  
  eval_txt <- ifelse(first_vec[2] == "||", paste0(first_vec[1], first_vec[3]),
                     eval(parse(text = paste(first_vec, collapse = " "))))
  eval_txt <- format(as.numeric(eval_txt), scientific = FALSE)
  
  new_vec <- c(eval_txt, second_vec)
  
  if (length(new_vec) == 1) {
    return(new_vec)
  } else {
    vec_eval_part2(new_vec)
  }
}

math_check_part2 <- function(x) {
  x <- x |>
    as.numeric() |>
    na.omit()
  
  trueI <<- trueI + 1
  print(trueI)
  
  total_val <- x[1]
  val_vec <- x[-1]
  
  num_val <- length(val_vec) |> as.numeric()
  
  ###Number of operator locations
  num_spots <- length(val_vec) - 1
  
  ###Number of possible combinations
  num_possible <- 3 ** num_spots
  
  ###Get all possible operator permutations
  operator_set <- gtools::permutations(3, num_spots, operator_vec, repeats = TRUE)
  
  result_val <- 0
  
  ###Get equations and evaluate
  for (i in 1:num_possible) {
    eq_vec <- c(rbind(val_vec, c(operator_set[i, ], "")))[1:(num_spots + num_val)]
    
    eq_result <- as.numeric(vec_eval_part2(eq_vec))
    
    if (as.numeric(total_val) == eq_result) {
      result_val <- eq_result
      
      break
    }
  }
  
  return(result_val)
}

trueI <- 0
result_out <- apply(in_data, 1, math_check_part2)

part2answer <- sum(result_out)
