###Libraries
library(collections)
library(stringr)

###Read in data
input_file <- "input.txt"

in_data <- scan(file = paste0("day17/data/",input_file), "character", sep = "\n",
                blank.lines.skip = TRUE) |>
  str_replace_all(fixed(":"), "")


registers <- in_data[which(word(in_data, 1) == "Register")]

instructions <- in_data[which(word(in_data, 1) == "Program")] |>
  word(2)

###Initialize register dictionary
register_dict <- dict()

for (r in registers) {
  register <- word(r, 2)
  
  register_dict$set(register, as.numeric(word(r, 3)))
}

###Clean up 
rm(r, register, registers)

###Combo Operand Function
combo <- function(op) {
  if (op %in% c(1,2,3)){
    return(op)
  } else if (op == 4) {
    return(register_dict$get("A"))
  } else if (op == 5) {
    return(register_dict$get("B"))
  } else if (op == 6) {
    return(register_dict$get("C"))
  } else if (op == 7) {
    print("THIS SHOULD NOT HAPPEN")
  }
}

###Instruction functions 
###These will return TRUE or FALSE 
###TRUE: i_point jumps by 2
###FALSE: i_point doesn't jump
adv <- function(i_point, op) {
  numerator <- register_dict$get("A")
  denominator <- 2 ** combo(op)
  
  result <- floor(numerator / denominator)
  
  register_dict$set("A", result)
  
  return(i_point+2)
}

bxl <- function(i_point, op) {
  result <- bitwXor(register_dict$get("B"), op)
  
  register_dict$set("B", result)
  
  return(i_point + 2)
}

bst <- function(i_point, op) {
  result <- combo(op) %% 8
  
  register_dict$set("B", result)
  
  return(i_point + 2)
}

jnz <- function(i_point, op) {
  reg_a <- register_dict$get("A")
  
  if (reg_a == 0) {
    return(i_point + 2)
  }
  else{
    return(op)
  }
}

bxc <- function(i_point, op) {
  result <- bitwXor(register_dict$get("B"), register_dict$get("C"))
  
  register_dict$set("B", result)
  
  return(i_point + 2)
}

out <- function(i_point, out_vec, op) {
  result <- combo(op) %% 8
  
  out_vec <- c(out_vec, result)
  
  out_list <- list()
  out_list[["i_point"]] <- i_point + 2
  out_list[["out_vec"]] <- out_vec
  
  return(out_list)
}

bdv <- function(i_point, op) {
  numerator <- register_dict$get("A")
  denominator <- 2 ** combo(op)
  
  result <- floor(numerator / denominator)
  
  register_dict$set("B", result)
  
  return(i_point + 2)
}

cdv <- function(i_point, op) {
  numerator <- register_dict$get("A")
  denominator <- 2 ** combo(op)
  
  result <- floor(numerator / denominator)
  
  register_dict$set("C", result)
  
  return(i_point + 2)
}

program_function <- function(instructions) {
  instruct_parse <- instructions |>
    str_split(",") |>
    unlist() |>
    as.numeric()
  
  ###Initialize Instruction Pointer
  i_point <- 0
  
  ###Output Vector
  out_vec <- c()

  ###LOOP
  while (i_point < length(instruct_parse)) {
    cur_instruct <- c(instruct_parse[i_point + 1], instruct_parse[i_point + 2])
    
    n_func <- cur_instruct[1]
    operand <- cur_instruct[2]
    
    if (n_func == 0) {
      i_point <- adv(i_point, operand)
    } else if (n_func == 1) {
      i_point <- bxl(i_point, operand)
    } else if (n_func == 2) {
      i_point <- bst(i_point, operand)
    } else if (n_func == 3) {
      i_point <- jnz(i_point, operand)
    } else if (n_func == 4) {
      i_point <- bxc(i_point, operand)
    } else if (n_func == 5) {
      out_list <- out(i_point, out_vec, operand)
      
      out_vec <- out_list$out_vec
      i_point <- out_list$i_point
    } else if (n_func == 6) {
      i_point <- bdv(i_point, operand)
    } else if (n_func == 7) {
      i_point <- cdv(i_point, operand)
    }
  }
  
  out_program <- paste0(out_vec, collapse = ",") 
  
  return(out_program)
}


#Part 1 ------------------------------------------------------------------
part1answer <- program_function(instructions)
part1answer
