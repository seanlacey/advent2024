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

in_data <- readLines(paste0("day15/data/",input_file), warn = FALSE)

box_map_line <- paste0(in_data[1:map_rows], collapse = "")
directions <- paste0(in_data[-(1:(map_rows + 1))], collapse = "")

###Update box_map
box_map_line <- box_map_line |>
  str_replace_all(fixed("#"), fixed("##")) |>
  str_replace_all(fixed("O"), fixed("[]")) |>
  str_replace_all(fixed("."), fixed("..")) |>
  str_replace_all(fixed("@"), fixed("@."))

map_cols <- map_cols * 2

box_map <- as.vector(str_split_fixed(box_map_line, pattern = "", n = nchar(box_map_line))) |>
  matrix(ncol = map_cols, byrow = TRUE)

###Change directions string into a vector. Replacing characters with the movements
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
###Should be the same from part 1
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

                     if (any(new_loc %in% unlist(box_df[,2:3], use.names = FALSE))) {
                       neighbor_name <- box_df[which(box_df$curr_pos1 == new_loc |
                                                     box_df$curr_pos2 == new_loc),1]
                       
                       result <- box_list[[neighbor_name]]$move(d)

                       if (result == TRUE) {
                         private$current_pos = new_loc
                       }
                     } else if (!(any(new_loc %in% wall_pos))) {
                       private$current_pos = new_loc
                     }
                   }
                 )
)

###Define Box Object
###Updating this from part 1. 
###Position is now going to be a vector of two complex numbers
###representing the two positions of the box
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
                 move_left = function(d) {
                   new_loc <- private$current_pos + d
                   
                   if (new_loc[1] %in% box_df$curr_pos2) {
                     neighbor_name <- box_df[which(box_df$curr_pos2 == new_loc[1]),1]
                     
                     result <- box_list[[neighbor_name]]$move_left(d)  
                     
                     if (result == TRUE) {
                       private$current_pos = new_loc
                       box_df[which(box_df[,1] == private$id), 2] <<- new_loc[1]
                       box_df[which(box_df[,1] == private$id), 3] <<- new_loc[2]
                       box_df[which(box_df[,1] == private$id), 4] <<- paste(new_loc[1], new_loc[2], sep = " ")
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }
                   } else if (!(new_loc[1] %in% wall_pos)) {
                     private$current_pos = new_loc
                     box_df[which(box_df[,1] == private$id), 2] <<- new_loc[1]
                     box_df[which(box_df[,1] == private$id), 3] <<- new_loc[2]
                     box_df[which(box_df[,1] == private$id), 4] <<- paste(new_loc[1], new_loc[2], sep = " ")
                     return(TRUE)
                   } else {
                     return(FALSE)
                   }
                 },
                 move_right = function(d) {
                   new_loc <- private$current_pos + d
                   
                   if (new_loc[2] %in% box_df$curr_pos1) {
                     neighbor_name <- box_df[which(box_df$curr_pos1 == new_loc[2]),1]
                     
                     result <- box_list[[neighbor_name]]$move_right(d)  
                     
                     if (result == TRUE) {
                       private$current_pos = new_loc
                       box_df[which(box_df[,1] == private$id), 2] <<- new_loc[1]
                       box_df[which(box_df[,1] == private$id), 3] <<- new_loc[2]
                       box_df[which(box_df[,1] == private$id), 4] <<- paste(new_loc[1], new_loc[2], sep = " ")
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }
                   } else if (!(new_loc[2] %in% wall_pos)) {
                     private$current_pos = new_loc
                     box_df[which(box_df[,1] == private$id), 2] <<- new_loc[1]
                     box_df[which(box_df[,1] == private$id), 3] <<- new_loc[2]
                     box_df[which(box_df[,1] == private$id), 4] <<- paste(new_loc[1], new_loc[2], sep = " ")
                     return(TRUE)
                   } else {
                     return(FALSE)
                   }
                 },
                 ###Function to test that vertical is viable before moving anything
                 move_vertical_test = function(d) {
                   new_loc <- private$current_pos + d
                   
                   ###Our box is directly above or below a box
                   if (all(paste0(new_loc, collapse = " ") %in% box_df$curr_pos_all)) {
                     neighbor_name <- box_df[which(box_df$curr_pos1 == new_loc[1]),1]
                     
                     result <- box_list[[neighbor_name]]$move_vertical_test(d)
                     
                     if (result == TRUE) {
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }           
                   }
                   ###Our box is above or below two boxes
                   else if ((new_loc[1] %in% box_df$curr_pos2) &
                            (new_loc[2] %in% box_df$curr_pos1)) {
                     neighbor_name1 <- box_df[which(box_df$curr_pos2 == new_loc[1]),1]
                     neighbor_name2 <- box_df[which(box_df$curr_pos1 == new_loc[2]),1]
                     
                     result1 <- box_list[[neighbor_name1]]$move_vertical_test(d)
                     result2 <- box_list[[neighbor_name2]]$move_vertical_test(d)
                     
                     if (result1 == TRUE & result2 == TRUE) {
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }  
                   } 
                   ###Our box is above or below the right side of a box
                   else if (new_loc[1] %in% box_df$curr_pos2 &
                            !(new_loc[2] %in% wall_pos)) {
                     neighbor_name <- box_df[which(box_df$curr_pos2 == new_loc[1]),1]
                     
                     result <- box_list[[neighbor_name]]$move_vertical_test(d)
                     
                     if (result == TRUE) {
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }   
                   }
                   ###Our box is above or below the left side of a box
                   else if (new_loc[2] %in% box_df$curr_pos1 &
                            !(new_loc[1] %in% wall_pos)) {
                     neighbor_name <- box_df[which(box_df$curr_pos1 == new_loc[2]),1]
                     
                     result <- box_list[[neighbor_name]]$move_vertical_test(d)
                     
                     if (result == TRUE) {
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }   
                   } else if (!(any(new_loc %in% wall_pos))) {
                     return(TRUE)
                   } else {
                     return(FALSE)
                   }
                 },
                 move_vertical = function(d) {
                   new_loc <- private$current_pos + d
                   
                   ###Our box is directly above or below a box
                   if (all(paste0(new_loc, collapse = " ") %in% box_df$curr_pos_all)) {
                     neighbor_name <- box_df[which(box_df$curr_pos1 == new_loc[1]),1]
                     
                     result <- box_list[[neighbor_name]]$move_vertical(d)
                     
                     if (result == TRUE) {
                       private$current_pos = new_loc
                       box_df[which(box_df[,1] == private$id), 2] <<- new_loc[1]
                       box_df[which(box_df[,1] == private$id), 3] <<- new_loc[2]
                       box_df[which(box_df[,1] == private$id), 4] <<- paste(new_loc[1], new_loc[2], sep = " ")
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }           
                   }
                   ###Our box is above or below two boxes
                   else if ((new_loc[1] %in% box_df$curr_pos2) &
                            (new_loc[2] %in% box_df$curr_pos1)) {
                     neighbor_name1 <- box_df[which(box_df$curr_pos2 == new_loc[1]),1]
                     neighbor_name2 <- box_df[which(box_df$curr_pos1 == new_loc[2]),1]
                     
                     result1 <- box_list[[neighbor_name1]]$move_vertical(d)
                     result2 <- box_list[[neighbor_name2]]$move_vertical(d)
                     
                     if (result1 == TRUE & result2 == TRUE) {
                       private$current_pos = new_loc
                       box_df[which(box_df[,1] == private$id), 2] <<- new_loc[1]
                       box_df[which(box_df[,1] == private$id), 3] <<- new_loc[2]
                       box_df[which(box_df[,1] == private$id), 4] <<- paste(new_loc[1], new_loc[2], sep = " ")
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }  
                   } 
                   ###Our box is above or below the right side of a box
                   else if (new_loc[1] %in% box_df$curr_pos2) {
                     neighbor_name <- box_df[which(box_df$curr_pos2 == new_loc[1]),1]
                     
                     result <- box_list[[neighbor_name]]$move_vertical(d)
                     
                     if (result == TRUE) {
                       private$current_pos = new_loc
                       box_df[which(box_df[,1] == private$id), 2] <<- new_loc[1]
                       box_df[which(box_df[,1] == private$id), 3] <<- new_loc[2]
                       box_df[which(box_df[,1] == private$id), 4] <<- paste(new_loc[1], new_loc[2], sep = " ")
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }   
                   }
                   ###Our box is above or below the left side of a box
                   else if (new_loc[2] %in% box_df$curr_pos1) {
                     neighbor_name <- box_df[which(box_df$curr_pos1 == new_loc[2]),1]
                     
                     result <- box_list[[neighbor_name]]$move_vertical(d)
                     
                     if (result == TRUE) {
                       private$current_pos = new_loc
                       box_df[which(box_df[,1] == private$id), 2] <<- new_loc[1]
                       box_df[which(box_df[,1] == private$id), 3] <<- new_loc[2]
                       box_df[which(box_df[,1] == private$id), 4] <<- paste(new_loc[1], new_loc[2], sep = " ")
                       return(TRUE)
                     } else {
                       return(FALSE)
                     }   
                   } else if (!(any(new_loc %in% wall_pos))) {
                     private$current_pos = new_loc
                     box_df[which(box_df[,1] == private$id), 2] <<- new_loc[1]
                     box_df[which(box_df[,1] == private$id), 3] <<- new_loc[2]
                     box_df[which(box_df[,1] == private$id), 4] <<- paste(new_loc[1], new_loc[2], sep = " ")
                     return(TRUE)
                   } else {
                     return(FALSE)
                   }
                 },
                 move = function(d) {
                   ###Switch to use function suited to direction
                   if (d == -1+0i) {
                     result <- self$move_left(d)
                   }
                   else if (d == 1+0i) {
                     result <- self$move_right(d)
                   } 
                   else {
                     result <- self$move_vertical_test(d)
                      
                      ###Only run if the test version comes back positive
                      if (isTRUE(result)) {
                        result <- self$move_vertical(d)
                      }
                   }
                   
                   return(result)
                 },
                 calc_gps = function() {
                   from_top <- Im(private$current_pos[1]) - 1
                   from_left <- Re(private$current_pos[1]) - 1
                   
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
  
  for (b in box_df$curr_pos1) {
    out_mat[Im(b), Re(b)] <- "["
    out_mat[Im(b), Re(b) + 1] <- "]"
  }
  
  out_mat[Im(rob$get_current_pos()), Re(rob$get_current_pos())] <- "@"
  
  return(out_mat)
}

box_list <- list()
box_name <- boxNamer()
box_iter <- 1
box_df <- data.frame(box_name = character(),
                     curr_pos1 = complex(),
                     curr_pos2 = complex(),
                     curr_pos_all = character())

wall_pos <- c()

for (cur_col in 1:map_cols) {
  for (cur_row in 1:map_rows) {
    if (box_map[cur_row, cur_col] == "#") {
      wall_pos <- c(wall_pos, complex(real = cur_col, imaginary = cur_row))
    } else if (box_map[cur_row, cur_col] == "[") {
      new_name <- box_name()
      
      box_loc <- c(complex(real = cur_col, imaginary = cur_row),
                   complex(real = cur_col + 1, imaginary = cur_row))
      
      box_list[[new_name]] <- box$new(id = new_name,
                                      position = box_loc)
      box_df[box_iter, 1] <- new_name 
      box_df[box_iter, 2:3] <- box_loc
      box_df[box_iter, 4] <- paste(box_loc[1], box_loc[2], sep = " ")
      
      box_iter <- box_iter + 1
    } else if (box_map[cur_row, cur_col] == "@") {
      rob <- robot$new(complex(real = cur_col, imaginary = cur_row))
    }
  }
}

###Cleaning up some unnecessary items
rm(input_file, d, directions, box_iter, new_name, box_map_line,
   cur_col, cur_row)

# test_maps <- list()

for (i in 1:length(dir_vec)) {
  rob$move(dir_vec[i])
  # test_maps[[i]] <- map_gen()
}

###Calculate GPS Sum
part2answer <- 0

for (b in box_df$box_name) {
  part2answer <- part2answer + box_list[[b]]$calc_gps()
}
