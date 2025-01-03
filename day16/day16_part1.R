###Space for libraries to be added as needed
library(readr)
library(collections)

###Set input file
input_file <- "input.txt"

###Get number of columns
num_col <- nchar(readLines(paste0("day16/data/",input_file), n = 1)) |>
  as.numeric()

in_data <- read_fwf(paste0("day16/data/",input_file), col_positions = fwf_widths(rep(1, num_col))) |>
  as.matrix()

###Get number of rows
num_row <- nrow(in_data) |> 
  as.numeric()

###Direction Vector
dir_vec <- c(0-1i, 1+0i, 0+1i, -1+0i)

###Parse Maze
s_loc_tmp <- which(in_data == "S", arr.ind = TRUE)
start_coord <- complex(real = s_loc_tmp[2], imaginary = s_loc_tmp[1])

e_loc_tmp <- which(in_data == "E", arr.ind = TRUE)
end_coord <- complex(real = e_loc_tmp[2], imaginary = e_loc_tmp[1])

w_loc_tmp <- which(in_data == "#", arr.ind = TRUE)
wall_vec <- apply(w_loc_tmp, 1, function(x) {complex(real = x[2], imaginary = x[1])})

p_loc_tmp <- which(in_data == ".", arr.ind = TRUE)
path_vec <- apply(p_loc_tmp, 1, function(x) {complex(real = x[2], imaginary = x[1])})
path_vec <- c(start_coord, path_vec, end_coord)

###Start facing east (right)
start_dir <- dir_vec[2]

rm(s_loc_tmp, e_loc_tmp, w_loc_tmp, p_loc_tmp)

###Queue Item Creator
q_item <- function(coord, dir) {
  out_list <- list(pos = coord,
                   dir = dir)
  
  return(out_list)
}

###Initialize Scores
score_dict <- dict()

for (i in 1:length(path_vec)) {
  score_dict$set(path_vec[i], 100000000) 
}

###Set start score to 0
score_dict$set(start_coord, 0)

path_q <- priority_queue()

###Add start to queue
path_q$push(q_item(start_coord, start_dir),0)

###Function to get list of neighbors
get_neighbors = function(coord) {
  n <- c()
  
  for (d in dir_vec) {
    new_loc <- coord + d
  
    if (maze_map[Im(new_loc), Re(new_loc)] != "#") {
      n <- c(n, new_loc)
    }
  }
  
  return(n)
}

###Make copy of map to track used spaces
maze_map <- in_data

###LOOP
while (TRUE) {
  curr_list <- path_q$pop()
  
  if (curr_list$pos == end_coord) {
    print(paste0("This is the end, my only friend, the end"))
    break
  }
  neighbors <- get_neighbors(curr_list$pos)
  
  if (!is.null(neighbors)) {
    for (n in neighbors) {
      score <- score_dict$get(curr_list$pos)
      
      if ((n - curr_list$pos) != curr_list$dir) {
        score <- score + 1001
      } else {
        score <- score + 1
      }
      
      if (score < score_dict$get(n)) {
        score_dict$set(n, score)
        
        path_q$push(q_item(n, n - curr_list$pos),-score)
      }
    }  
  }
  
  maze_map[Im(curr_list$pos), Re(curr_list$pos)] <- "X"
}

min_score <- score_dict$get(end_coord)
print(min_score)
