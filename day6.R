library(dplyr)
library(readr)

# Reading Data and Initial Work -------------------------------------------
### Get number of columns
num_col <- nchar(readLines("day6/data/input.txt", n = 1)) |>
  as.numeric()

in_data <- read_fwf("day6/data/input.txt", col_positions = fwf_widths(rep(1, num_col))) |>
  as.matrix()

### Get number of rows
num_row <- nrow(in_data) |> 
  as.numeric()

direction <- c("^", ">", "V", "<")
moves <- list(c(-1, 0), c(0, 1), c(1, 0), c(0, -1))

### Locate Guard
guard_found <- 0

for (cur_row in 1:num_row) {
  for (cur_col in 1:num_col) {
    ### I assume the guard will start as "^", but wanted to make it more general
    if (in_data[cur_row, cur_col] %in% direction) {
      ### Setting up guard as (row, col, direction, in map?)
      guard_coord <- c(cur_row, cur_col, which(direction == in_data[cur_row, cur_col]), 0)
      guard_found <- 1
      break
    }
  }
  
  if (guard_found == 1) {break}
}

guard_start <- guard_coord

# Part 1 ------------------------------------------------------------------
### Create copy of in_data for tracking progress
move_map <- in_data

### Mark starting position as used
move_map[guard_coord[1], guard_coord[2]] <- "X"

###Guard move function
guard_move <- function(guard_coord) {
  ###Parse Guard
  new_direct <- guard_coord[3]
  new_row <- guard_coord[1] + moves[[new_direct]][1]
  new_col <- guard_coord[2] + moves[[new_direct]][2]
  on_map <- guard_coord[4]
  
  ###Return if row and new col take out of bounds prior to move check
  if (new_row < 1 | new_row > num_row |
      new_col < 1 | new_col > num_col) {
    new_guard_coord <- c(new_row, new_col, new_direct, 1)
    
    return(new_guard_coord)
  }
  
  ###Check for blocked path
  move_check <- 1
  
  ###Looping to allow for multiple blocked paths in a row
  while (move_check < 4) {
    if (move_map[new_row, new_col] == "#") {
      new_direct <- ifelse(new_direct == 4, 1, new_direct + 1)
      new_row <- guard_coord[1] + moves[[new_direct]][1]
      new_col <- guard_coord[2] + moves[[new_direct]][2]
      move_check <- move_check + 1
    } else {
      move_check <- 99
    }
  }
  
  ###Check if we're still on the map
  on_map <- ifelse((new_row > 1 & new_row <= num_row) & 
                   (new_col > 1 & new_col <= num_col), 0, 1)
  
  new_guard_coord <- c(new_row, new_col, new_direct, on_map)
  
  return(new_guard_coord)
}

###Initialize count of moves
move_count <- 1

###Also going to add in a list of the coordinates traveled for use in part 2
guard_path <- list()
guard_path[[1]] <- c(guard_coord[1], guard_coord[2], guard_coord[3])

i <- 2

while (guard_coord[4] == 0) {
  guard_coord <- guard_move(guard_coord)
  guard_path[[i]] <- c(guard_coord[1], guard_coord[2], guard_coord[3])
  i <- i + 1
  
  ###Technically while loop should handle this, but need
  ###to avoid issues with next if statement
  if (guard_coord[4] == 1) {
    break
  }
  
  if (move_map[guard_coord[1], guard_coord[2]] != "X") {
    move_count <- move_count + 1
    move_map[guard_coord[1], guard_coord[2]] <- "X"
  }
}

### Clean up for part 2
rm(move_map, cur_col, cur_row, guard_coord, guard_found, i)

# Part 2 ------------------------------------------------------------------
### Function to combine rows, columns, direction for quick history check
historic_value <- function(guard_coord) {
  return(paste(guard_coord[1], guard_coord[2], guard_coord[3], sep = ","))
}

loop_vec <- c()
obstacle_vec <- c()

for (step_num in 2:(length(guard_path) - 1)) {
  print(step_num)
  
  ###Initialize Values
  move_map <- in_data
  guard_coord <- guard_start
  obstacle_loc <- guard_path[[step_num]][-c(3, 4)]
  
  ### If obstacle location would be same as starting spot, skip it
  if (paste(obstacle_loc[1], obstacle_loc[2], sep = ",") == 
      paste(guard_start[1], guard_start[2], sep = ",")) {
    next
  }
  
  ### If obstacle location would be same as spot right in front of guard, skip it
  if (paste(obstacle_loc[1], obstacle_loc[2], sep = ",") == 
      paste(guard_path[[2]][1], guard_path[[2]][2], sep = ",")) {
    next
  }
  
  ###Update map with obstacle
  move_map[obstacle_loc[1], obstacle_loc[2]] <- "#"
  
  history_path <- c(historic_value(guard_coord))
  
  while (guard_coord[4] == 0 & !any(duplicated(history_path))) {
    guard_coord <- guard_move(guard_coord)
    
    history_path <- c(history_path, historic_value(guard_coord))
    
    ###Technically while loop should handle this, but need
    ###to avoid issues with next if statement
    if (guard_coord[4] == 1) {
      break
    }
    
    if (move_map[guard_coord[1], guard_coord[2]] != "X") {
      move_count <- move_count + 1
      move_map[guard_coord[1], guard_coord[2]] <- "X"
    }
  }
  
  loop_vec <- c(loop_vec,ifelse(guard_coord[4] == 0, 1, 0))
  
  if (guard_coord[4] == 0) {
    obstacle_vec <- c(obstacle_vec, paste(obstacle_loc[1], obstacle_loc[2], sep = ","))
  }
}

sum(loop_vec)

length(obstacle_vec)

obstacle_vec_u <- unique(obstacle_vec)

length(obstacle_vec_u)



####################



# ### Route check function
# obstacle_check <- function(x) {
#   testI <<- testI + 1
#   print(testI)
#   
#   ### Reset move map
#   move_map <- in_data
#   
#   ###Save initial guard
#   guard_coord_init <- c(x[1], x[2], x[3], 0)
#   guard_coord <- guard_coord_init
#   
#   ###Save obstacle location
#   obstacle <- c(guard_coord_init[1] + moves[[guard_coord_init[3]]][1],
#                 guard_coord_init[2] + moves[[guard_coord_init[3]]][2])
#   
#   ### If obstacle is out of bounds then stop here
#   if (obstacle[1] < 1 | obstacle[1] > num_row |
#       obstacle[2] < 1 | obstacle[2] > num_col) {
#     obstacle_vec <- c(0, 0, 0)
#     
#     return(obstacle_vec)
#   }
#   
#   ###Only proceed if guard is in bounds and obstacle not in front
#   if (((guard_coord[1] > 1 & guard_coord[1] <= num_row) & 
#        (guard_coord[2] > 1 & guard_coord[2] <= num_col)) &
#       (move_map[obstacle[1], obstacle[2]] != "#")) {
#     
#     ###Change current direction, assuming path ahead is blocked
#     guard_coord[3] <- ifelse(guard_coord_init[3] == 4, 1, guard_coord_init[3] + 1)
#     
#     direction_change <- 0
#     loop_yn <- 0
#     
#     ### Loop through guard movements, tracking direction
#     ### Stop when new move takes them out of bounds, or loop
#     ### is detected
#     while (1 == 1) {
#       guard_coord <- guard_move(guard_coord)
#       
#       ### if new move took them out of bounds stop
#       if (guard_coord[4] == 1) {
#         break
#       }
#       
#       ###Update count for move_map
#       ###This is for detecting loops not involving the original spot
#       ###I'm assuming that if I'm crossing the same spot more than 100 times then it's stuck in an 
#       ###infinite loop
#       move_map[guard_coord[1], guard_coord[2]] <- ifelse(move_map[guard_coord[1], guard_coord[2]] == "." |
#                                                            move_map[guard_coord[1], guard_coord[2]] == "^", "1",
#                                                          as.character(as.numeric(move_map[guard_coord[1], guard_coord[2]]) + 1))
#       
#       if (as.numeric(move_map[guard_coord[1], guard_coord[2]]) > 100) {
#         loop_yn <- 1
#         break
#       }
#       
#       if (guard_coord[1] == guard_coord_init[1] &
#           guard_coord[2] == guard_coord_init[2] &
#           guard_coord[3] == guard_coord_init[3]) {
#         loop_yn <- 1
#         break
#       } 
#     }
#     
#     obstacle_vec <- c( obstacle[1], obstacle[2], loop_yn)
#   } else {
#     ### Default no for out of bounds obstacle
#     obstacle_vec <- c(0, 0, 0)
#   }
#   
#   return(obstacle_vec)
# }
