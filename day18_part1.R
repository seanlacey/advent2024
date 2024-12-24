###Load Libraries
library(collections)

###NOTE: For the data, I need to add 1 to all coordinates since they're
###referenced 0, X rather than 1, X 

###Load Data (Example)
# side_size <- 7
# byte_sim_num <- 12
# in_data <- readLines("day18/data/example.txt", warn = FALSE)

###Load Data (Actual)
side_size <- 71
byte_sim_num <- 1024
in_data <- readLines("day18/data/input.txt", warn = FALSE)

###Parse coordinates into complex numbers
byte_vec <- c()

for (p in in_data) {
  X <- as.numeric(unlist(strsplit(p,","))[1]) + 1
  Y <- as.numeric(unlist(strsplit(p,","))[2]) + 1
  
  byte_vec <- c(byte_vec, complex(real = X, imaginary = Y))
}

rm(p, X, Y)

###Set start and end coordinates
start <- 1+1i
end <- complex(real = side_size, imaginary = side_size)

###Direction Vector
dir_vec <- c(0-1i, 1+0i, 0+1i, -1+0i)

###Draw Map
maze_map <- matrix(data = rep(".", side_size**2), nrow = side_size, ncol = side_size)

###Simulate bytes falling
for (i in 1:byte_sim_num) {
  coord <- byte_vec[i]
  
  maze_map[Im(coord), Re(coord)] <- "#"
}

###Get list of paths and walls
w_loc_tmp <- which(maze_map == "#", arr.ind = TRUE)
wall_vec <- apply(w_loc_tmp, 1, function(x) {complex(real = x[2], imaginary = x[1])})

p_loc_tmp <- which(maze_map == ".", arr.ind = TRUE)
path_vec <- apply(p_loc_tmp, 1, function(x) {complex(real = x[2], imaginary = x[1])})

###Initialize Scores
score_dict <- dict()

for (i in 1:length(path_vec)) {
  score_dict$set(path_vec[i], 100000000) 
}

###Set start score to 0
score_dict$set(start, 0)

path_q <- priority_queue()

###Add start to queue
path_q$push(start,0)

###Function to get list of neighbors
get_neighbors = function(coord) {
  n <- c()
  
  for (d in dir_vec) {
    new_loc <- coord + d

    ###Make sure new_loc is in the map
    if ((Im(new_loc) > 0 & Im(new_loc) <= side_size) &
        (Re(new_loc) > 0 & Re(new_loc) <= side_size)) {
      if (step_map[Im(new_loc), Re(new_loc)] != "#") {
        n <- c(n, new_loc)
      }
    }
  }
  
  return(n)
}

###Make copy of map to track used spaces
step_map <- maze_map

###LOOP
while (TRUE) {
  curr_coord <- path_q$pop()
  
  if (curr_coord == end) {
    print(paste0("This is the end, my only friend, the end"))
    break
  }
  
  neighbors <- get_neighbors(curr_coord)
  
  if (!is.null(neighbors)) {
    for (n in neighbors) {
      score <- score_dict$get(curr_coord) + 1
      
      if (score < score_dict$get(n)) {
        score_dict$set(n, score)
        
        path_q$push(n,-score)
      }
    }  
  }
  
  step_map[Im(curr_coord), Re(curr_coord)] <- "X"
}

min_score <- score_dict$get(end)
print(min_score)
