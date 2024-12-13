library(readr)
library(R6)

# Reading Data and Initial Work -------------------------------------------

###Putting this here to make it easier to switch between test data
###and the real data
input_file <- "input.txt"

### Get number of columns
num_col <- nchar(readLines(paste0("day8/data/", input_file), n = 1)) |>
  as.numeric()

in_data <- read_fwf(paste0("day8/data/", input_file), col_positions = fwf_widths(rep(1, num_col))) |>
  as.matrix()

###Get number of rows
num_row <- nrow(in_data) |>
  as.numeric()

###Get vector of all unique antenna values (removing ".")
u_antenna <- unique(as.vector(in_data))
u_antenna <- u_antenna[!(u_antenna %in% ".")]

###Initialize Empty List
coord_list <- lapply(1:length(u_antenna), \(x) vector())
names(coord_list) <- u_antenna

###Get coordinates for all antennas (storing as complex numbers)
for (i in 1:num_row) {
  for (j in 1:num_col) {
    if (in_data[i, j] != ".") {
      coord_list[[in_data[i, j]]] <- c(coord_list[[in_data[i, j]]], complex(real = i, imaginary = j))     
    }
  }
}

### Part 1 ------------------------------------------------------------------

###Function that takes two points and calculates antinodes
antinode_find <- function(point1, point2) {
  new_x <- (1 - 2) * Re(point1) + 2 * Re(point2)
  new_y <- (1 - 2) * Im(point1) + 2 * Im(point2)
  
  node <- complex(real = new_x, imaginary = new_y)
  
  ###If new node is outside of map, return (9999,9999) combo to filter
  ###out later
  if ((new_x >= 1 & new_x <= num_row) &
      (new_y >= 1 & new_y <= num_col)) {
    return(node)
  } else {
    return(complex(real = 9999, imaginary = 9999))
  }
  
}

vec_list_func <- function(x) {
  ###Get all pairs and their reverse
  antenna_loc <- t(combn(x, 2))
  antenna_loc <- rbind(antenna_loc, antenna_loc[ , c(2, 1)])
  
  ###Get all nodes
  antinode_vec <- mapply(antinode_find, antenna_loc[,1], antenna_loc[,2])
  antinode_vec <- unique(antinode_vec[!(antinode_vec %in% "9999+9999i")])
  
  return(antinode_vec)
}

antinode_list <- lapply(coord_list, vec_list_func)

part1answer <- length(unique(unlist(antinode_list))) |> as.numeric()

### Part 2 ------------------------------------------------------------------
###Necessary helper function found in help files
is.wholenumber <- function(x, tol = .Machine$double.eps**0.5) {
  abs(x - round(x)) < tol
}

###Line Object (Named to avoid conflict with line function)
lineObject <- R6Class("lineObject", 
  private = list(
    m = NULL,
    b = NULL,
    point1 = NULL,
    point2 = NULL
  ),
  
  public = list(
    initialize = function(point1, point2) {
      private$point1 <- point1
      private$point2 <- point2
      
      private$m <- (Im(private$point2 - private$point1)) / (Re(private$point2 - private$point1))
      private$b <- ifelse(private$m != Inf & private$m != 0, Im(private$point1) - private$m * Re(private$point1), NULL)
    },
    
    ###Function that grabs all points on the line
    line_check = function(x) {
      y <- private$m * x + private$b
      
      ###Only return if y is an integer
      if (is.wholenumber(y)) {
        ###If new node is outside of map, return (9999,9999) combo to filter
        ###out later
        if ((x >= 1 & x <= num_row) &
            (y >= 1 & y <= num_col)) {
          return(complex(real = x, imaginary = round(y)))
        } else {
          return(complex(real = 9999, imaginary = 9999))
        }
      } else {
        return(complex(real = 9999, imaginary = 9999))
      }
    },
    
    generate_nodes = function() {
      if (private$m == Inf) {
        node_vec <- complex(real = Re(private$point1), imaginary = rep(1:num_col))
      } else if (private$m == 0) {
        node_vec <- complex(real = rep(1:num_row), imaginary = Im(private$point1))
      } else {
        node_vec <- unlist(lapply(1:num_row, self$line_check))
        node_vec <- node_vec[!(node_vec %in% "9999+9999i")]
      }
      
      return(node_vec)
    }
  )
)

###Function that takes two points and calculates antinodes
antinode_find_part2 <- function(point1, point2) {
  ###Create new line
  newline <- lineObject$new(point1, point2)

  return(newline$generate_nodes())
}

vec_list_func_part2 <- function(x) {
  ###Get all pairs and their reverse
  antenna_loc <- t(combn(x, 2))
  
  ###Get all nodes
  ###Get all nodes
  antinode_vec <- mapply(antinode_find_part2, antenna_loc[,1], antenna_loc[,2])
  antinode_vec <- unique(unlist(antinode_vec))
  
  return(antinode_vec)
}

antinode_list_part2 <- lapply(coord_list, vec_list_func_part2)

part2answer <- length(unique(unlist(antinode_list_part2))) |> as.numeric()
