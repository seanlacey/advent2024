###Load libraries as needed here
library(dplyr)
library(stringr)
library(R6)

###Set Row and Column Numbers (example)
# row_num <- 7
# col_num <- 11

##Set Row and Column Numbers (actual)
row_num <- 103
col_num <- 101

###Find the quadrant divides
row_divide <- ceiling(row_num / 2)
col_divide <- ceiling(col_num / 2)

###Load data
input_file <- "input.txt"

in_data <- scan(file = paste0("day14/data/",input_file), c("character", "character"), sep = " ",
                blank.lines.skip = TRUE) |>
  matrix(ncol = 2, byrow = TRUE) |>
  as.data.frame()

coord_extract <- function(x) {
  x_out <- as.numeric(word(str_remove_all(x, "[pv\\=]"), 1, sep = ","))
  y_out <- as.numeric(word(str_remove_all(x, "[pv\\=]"), 2, sep = ","))
  
  return(complex(real = x_out, imaginary = y_out))
}

###Parse Text
in_data <- in_data |>
  mutate(id = row_number(),
         p_coord = coord_extract(V1) + (1 + 1i),
         v_coord = coord_extract(V2)) |>
  dplyr::select(id, p_coord, v_coord)

num_robot <- nrow(in_data) |>
  as.numeric()

###Function for determining quadrant
quadrant <- function(x) {
  if (Im(x) < row_divide) {
    if (Re(x) < col_divide) {
      q <- 1
    } else if (Re(x) > col_divide) {
      q <- 2
    } else {
      q <- 0
    }
  } else if (Im(x) > row_divide) {
    if (Re(x) < col_divide) {
      q <- 3
    } else if (Re(x) > col_divide) {
      q <- 4
    } else {
      q <- 0
    }
  } else{
    q <- 0
  }
  
  return(q)
}

###Create Robot Object
robot <- R6Class("robot",
                 private = list(
                   initial_pos = NULL,
                   current_pos = NULL,
                   velocity = NULL
                 ),
                 public = list(
                   initialize = function(position, velocity) {
                     private$initial_pos = position
                     private$current_pos = position
                     private$velocity = velocity
                   },
                   
                   ###Getters
                   get_current_pos = function() {
                     return(private$current_pos)
                   },
                   get_initial_pos = function() {
                     return(private$initial_pos)
                   },
                   get_velocity = function() {
                     return(private$velocity)
                   },
                   
                   ###Check for out of bounds positions and move if
                   ###necessary
                   teleport = function(position) {
                     ###Check row position
                     row <- Im(position)
                     
                     if (row > row_num) {
                       new_row = row - row_num
                     } else if (row < 1) {
                       new_row = row + row_num
                     } else {
                       new_row = row
                     }
                     ###Check col position
                     col <- Re(position)
                     
                     if (col > col_num) {
                       new_col = col - col_num
                     } else if (col < 1) {
                       new_col = col + col_num
                     } else {
                       new_col = col
                     }
                     
                     return(complex(real = new_col, imaginary = new_row))
                   }, 
                   
                   ###Robot move once
                   patrol = function() {
                     new_pos <- private$current_pos + private$velocity
                     
                     new_pos <- self$teleport(new_pos)
                     
                     private$current_pos <- new_pos
                   },
                   
                   ###Function to see if robot has neighbor
                   ###If so, return how many
                   neighbor = function(curr_pos_vec) {
                     direction_vec <- c(0 - 1i, 1 + 0i, 0 + 1i, -1 + 0i)
                     adjacent_vec <- private$current_pos + direction_vec
                     
                     return(sum(adjacent_vec %in% curr_pos_vec))
                   }
                 ))

###Initialize robots
robot_list <- list()

for (r in 1:num_robot) {
  robot_list[[r]] <- robot$new(in_data[r, 2], in_data[r, 3])
}

#Part 1 ------------------------------------------------------------------
for (i in 1:100) {
  for (r in 1:num_robot) {
    robot_list[[r]]$patrol()
  }
}

###Initialize position dataframe
curr_df <- data.frame(id = numeric(),
                      quadrant = numeric(),
                      position = numeric())

for (r in 1:num_robot) {
  curr_df[r, "id"] <- r
  curr_df[r, "position"] <- robot_list[[r]]$get_current_pos()
  curr_df[r, "quadrant"] <- quadrant(robot_list[[r]]$get_current_pos())
}

part1summary <- curr_df |>
  filter(quadrant != 0) |>
  group_by(quadrant) |>
  summarize(robot_count = n())

part1answer <- prod(part1summary[,2])

#Part 2 ------------------------------------------------------------------
matrix_gen <- function(robot_list) {
  out_mat <- rep(".", row_num * col_num) |>
    matrix(ncol = col_num, byrow = TRUE)

  for (r in robot_list) {
    r_coord <- r$get_current_pos()
    
    if (out_mat[Im(r_coord),Re(r_coord)] == ".") {
      out_mat[Im(r_coord),Re(r_coord)] <- "1"
    } else {
      out_mat[Im(r_coord),Re(r_coord)] <- as.character(as.numeric(out_mat[Im(r_coord),Re(r_coord)]) + 1)
    }
  }  
  
  return(out_mat)
}

#The main idea here is that I iterate through the robot patrols a sufficiently
#high number of times (here 10000). For any of those iterations, if there is a
#large number of neighbors, then generate the matrix and check the shape.

neighbor_vec <- c()
matrix_list <- list()
matrix_iter <- 1

for (i in 1:10000) {
  print(paste0("Iteration: ", i))
  neighbor_sum <- 0
  curr_pos_vec <- c()
  
  for (r in 1:(num_robot)) {
    robot_list[[r]]$patrol()
    
    neighbor_sum <- neighbor_sum + robot_list[[r]]$neighbor(curr_pos_vec)
    curr_pos_vec <- c(curr_pos_vec, robot_list[[r]]$get_current_pos())
  }
  
  ###If a round has a high number of neighbors, output that matrix
  if (neighbor_sum > 200) {
    matrix_list[[matrix_iter]] <- list()
    matrix_list[[matrix_iter]][["i"]] <- i
    matrix_list[[matrix_iter]][["mat"]] <- matrix_gen(robot_list)
    matrix_iter <- matrix_iter + 1
  }
  
  neighbor_vec <- c(neighbor_vec, neighbor_sum)
}

#I first decided to set the number of neighbors to >200 and lucked out in 
#there only being one iteration that met that criteria. The below is just
#me extracting out the matrix information based on my input, it's not
#guaranteed to be the same for everyone
part2answer <- matrix_list[[1]]$i
mat_out <- matrix_list[[1]]$mat[20:54,40:73]
