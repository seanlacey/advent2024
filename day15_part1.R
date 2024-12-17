####Leaving room for libraries to add as needed
library(readr)
library(stringr)
library(R6)

##Read in data
input_file <- "input.txt"

###Get map dimensions
map_rows <- which(readLines(paste0("day15/data/",input_file), warn = FALSE) == "") - 1

map_cols <- nchar(readLines(paste0("day15/data/",input_file), n = 1)) |>
  as.numeric()

###Read in map
box_map <- read_fwf(paste0("day15/data/",input_file), 
                    col_positions = fwf_widths(rep(1, map_cols)), 
                    n_max = map_rows) |>
  as.matrix()

###Read in directions
directions <- paste0(readLines(paste0("day15/data/",input_file), warn = FALSE)[-(1:(map_rows + 1))], collapse = "")

###Change directions stirng into a vector. Replacing characters with the movements
###to be made
dir_vec <- c()

for (d in 1:nchar(directions)) {
  if (substr(directions, d, d) == "^") {dir_vec <- c(dir_vec, 0 - 1i)}
  else if (substr(directions, d, d) == ">") {dir_vec <- c(dir_vec, 1 + 0i)}
  else if (substr(directions, d, d) == "v") {dir_vec <- c(dir_vec, 0 + 1i)}
  else if (substr(directions, d, d) == "<") {dir_vec <- c(dir_vec, -1 + 0i)}
}

###Create position vectors of walls, boxes, and find the robot
boxNamer <- function() {
  i <- 0
  function(node) sprintf("B%g", (i <<- i + 1))
}

###Define Robot Object
robot <- R6Class("robot",
  private = list(
    initial_pos = NULL,
    current_pos = NULL
  ),
  public = list(
    initialize = function(position) {
      private$initial_pos = position
      private$current_pos = position
    },
    get_current_pos = function() {
      return(private$current_pos)
    },
    
    ###Move function
    move = function(d) {
      new_loc <- private$current_pos + d
      
      if (new_loc %in% box_df[,2]) {
        neighbor_name <- box_df[which(box_df[,2] == new_loc),1]
        result <- box_list[[neighbor_name]]$move(d)
        
        if (result == TRUE) {
          private$current_pos = new_loc
        }
      } else if (!(new_loc %in% wall_pos)) {
        private$current_pos = new_loc
      }
    }
  )
)

###Define Box Object
box <- R6Class("box",
  private = list(
    initial_pos = NULL,
    current_pos = NULL,
    id = NULL
  ),
  public = list(
    initialize = function(id, position) {
      private$id = id
      private$initial_pos = position
      private$current_pos = position
    },
    get_id = function() {
      return(private$id)
    },
    get_current_pos = function() {
      return(private$current_pos)
    },
    move = function(d) {
      new_loc <- private$current_pos + d
      
      if (new_loc %in% box_df[,2]) {
        neighbor_name <- box_df[which(box_df[,2] == new_loc),1]
        result <- box_list[[neighbor_name]]$move(d)
        
        if (result == TRUE) {
          private$current_pos = new_loc
          box_df[which(box_df[,1] == private$id), 2] <<- new_loc
          return(TRUE)
        } else {
          return(FALSE)
        }
      } else if (!(new_loc %in% wall_pos)) {
        private$current_pos = new_loc
        box_df[which(box_df[,1] == private$id), 2] <<- new_loc
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    calc_gps = function() {
      from_top <- Im(private$current_pos) - 1
      from_left <- Re(private$current_pos) - 1
      
      return(100 * from_top + from_left)
    }
  )
)

###Function to generate new map
map_gen <- function() {
  out_mat <- rep(".", map_rows * map_cols) |>
    matrix(ncol = map_cols, byrow = TRUE)
  
  for (w in wall_pos) {
    out_mat[Im(w), Re(w)] <- "#"
  }
  
  for (b in box_df$curr_pos) {
    out_mat[Im(b), Re(b)] <- "O"
  }
  
  out_mat[Im(rob$get_current_pos()), Re(rob$get_current_pos())] <- "@"
  
  return(out_mat)
}

box_list <- list()
box_name <- boxNamer()
box_iter <- 1
box_df <- data.frame(box_name = character(),
                     curr_pos = complex())

wall_pos <- c()

for (cur_col in 1:map_cols) {
  for (cur_row in 1:map_rows) {
    if (box_map[cur_row, cur_col] == "#") {
      wall_pos <- c(wall_pos, complex(real = cur_col, imaginary = cur_row))
    } else if (box_map[cur_row, cur_col] == "O") {
      new_name <- box_name()
      box_list[[new_name]] <- box$new(id = new_name,
                                      complex(real = cur_col, imaginary = cur_row))
      box_df[box_iter, 1] <- new_name 
      box_df[box_iter, 2] <- complex(real = cur_col, imaginary = cur_row)
      
      box_iter <- box_iter + 1
    } else if (box_map[cur_row, cur_col] == "@") {
      rob <- robot$new(complex(real = cur_col, imaginary = cur_row))
    }
  }
}

###Cleaning up some unnecessary items
rm(input_file, d, directions, box_iter, new_name)


for (i in 1:length(dir_vec)) {
  rob$move(dir_vec[i])
}

# new_map <- map_gen()

###Calculate GPS Sum
part1answer <- 0

for (b in box_df$box_name) {
  part1answer <- part1answer + box_list[[b]]$calc_gps()
}
