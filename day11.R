###Space to read in libraries (if needed)
library(purrr)

###Read in input
input_file <- "input.txt"

in_data <- readLines(paste0("day11/data/",input_file), warn = FALSE)

###Separate into stone vector
stone_vec <- unlist(strsplit(in_data, " ")) |> as.numeric()

#If the stone is engraved with the number 0, 
#it is replaced by a stone engraved with the number 1.
zero_stone <- function(x) {
  return(1)
}

#If the stone is engraved with a number that has an even number of digits, 
#it is replaced by two stones. The left half of the digits are engraved on 
#the new left stone, and the right half of the digits are engraved on the 
#new right stone. (The new numbers don't keep extra leading zeroes: 1000 
#would become stones 10 and 0.)
split_stone <- function(x) {
  x_char <- x |> as.character()
  num_digits <- nchar(x)
  
  y1 <- substr(x_char, 1, num_digits/2) |> as.numeric()
  y2 <- substr(x_char, (num_digits/2) + 1, num_digits) |> as.numeric()
  
  return(c(y1, y2))
}

#If none of the other rules apply, the stone is replaced by a new stone; 
#the old stone's number multiplied by 2024 is engraved on the new stone.
other_stone <- function(x) {
  return(x * 2024)
}

###Blink Function
blink <- function(stones) {
  zero_pos <- which(stones == 0)
  split_pos <- which(nchar(stones) %% 2 == 0)
  other_pos <- c(1:length(stones))
  
  if (length(zero_pos) != 0 & length(split_pos != 0)) {
    other_pos <- other_pos[-c(zero_pos, split_pos)]
  } else if (length(zero_pos) != 0) {
    other_pos <- other_pos[-zero_pos]
  } else if (length(split_pos) != 0) {
    other_pos <- other_pos[-split_pos]
  }
  
  zero_vec <- map_vec(stones[zero_pos], zero_stone)
  other_vec <- map_vec(stones[other_pos], other_stone)
  split_vec <- unlist(map(stones[split_pos], split_stone))
  
  out_vec <- c(zero_vec, split_vec, other_vec)
  
  return(out_vec)
}

#Part 1 ------------------------------------------------------------------
for (i in 1:25) {
  print(paste0("Blink ",i))
  stone_vec <- blink(stone_vec)
}

part1answer <- length(stone_vec) |> as.numeric()

#Part 2 ------------------------------------------------------------------
###Blink Function
blink2 <- function(stone) {
  if (stone == 0) {
    return(1)
  } else if (nchar(stone) %% 2 == 0) {
    return(split_stone(stone))
  } else {
    return(other_stone(stone))
  }
  
  return(stone)
}

###Set number of iterations
n_iter <- 75

###Create data frame for holding the counts
stone_counts <- as.data.frame(stone_vec)
colnames(stone_counts)[1] <- "stones"
stone_counts$ITER0 <- 1
stone_counts[,paste0("ITER",1:n_iter)] <- 0
  
for (n_blink in 1:n_iter) {
  print(paste0("Blink ", n_blink))
  for (stone in stone_counts$stones) {
    s_count <- stone_counts[which(stone_counts$stones == stone), paste0("ITER",n_blink - 1)]
    
    new_stones <- blink2(stone)
    
    for (new_stone in new_stones) {
      if (!(new_stone %in% stone_counts$stones)) {
        stone_counts <- rbind(stone_counts, c(new_stone, rep(0, n_iter + 1)))
      }
      
      stone_counts[which(stone_counts$stones == new_stone), paste0("ITER",n_blink)] <- 
        stone_counts[which(stone_counts$stones == new_stone), paste0("ITER",n_blink)] + s_count
    }
  }
}

part2answer <- sum(stone_counts$ITER75)
