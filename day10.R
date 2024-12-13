library(readr)
library(dplyr)
library(data.tree)

#Reading Data and Initial Work -------------------------------------------

###Putting this here to make it easier to switch between test data
###and the real data
input_file <- "input.txt"

###Get number of columns
num_col <- nchar(readLines(paste0("day10/data/", input_file), n = 1)) |>
  as.numeric()

in_data <- read_fwf(paste0("day10/data/", input_file), col_positions = fwf_widths(rep(1, num_col))) |>
  as.matrix()

###Get number of rows
num_row <- nrow(in_data) |>
  as.numeric()

###Create array of coordinate movements indicating up, right, down, left
directions <- complex(real = c(0,1,0,-1), imaginary = c(-1,0,1,0))

###Find trailheads. Storing as complex since they can be more compactly referenced
trailheads <- which(in_data == 0, arr.ind = TRUE)
trailheads <- complex(real = trailheads[,2], imaginary = trailheads[,1])

###Get list of trial endings
trailends <- which(in_data == 9, arr.ind = TRUE)
trailends <- complex(real = trailends[,2], imaginary = trailends[,1])

#Part 1 ------------------------------------------------------------------
##Function I found on stack overflow to give nodes unique names
nodeNamer <- function() {
  i <- 0
  function(node) sprintf("N%g", (i <<- i + 1))
}

###Initialize list
# trail_list <- vector("list", length = length(trailheads))
trail_tree <- Node$new("ROOT", coordinates = complex(real = 0, imaginary = 0), direction = 0, end = 0)

###Initialize trees
for (i in 1:length(trailheads)) {
  trail_tree$AddChild(paste0("P",i), coordinates = trailheads[i], direction = 0, end = 0)
}

node_check <- function(node, terrain) {
  ###Create vector of directions to search
  d_vec <- 1:4

  ###Remove direction last node came from
  if (node$direction != 0) {
    d_vec[-node$direction]
  }

  for (d in d_vec) {
    new_node <- node$coordinates + directions[d]

    ###First check that the coordinates are on the map
    ###Then check if it meets the terrain requirements
    if ((Re(new_node) >= 1 & Re(new_node) <= num_row) &
        (Im(new_node) >= 1 & Im(new_node) <= num_col)) {
      if (in_data[Im(new_node),Re(new_node)] == terrain) {
        child <- node$AddChild(name.node())
        child$coordinates <- new_node
        child$direction <- d
        child$end <- ifelse(terrain == 9, 1, 0)
        
        if (child$end != 1) {node_check(child, terrain + 1)}
      }
    }
  }
  
  return(node)
}

###Initialize node names
name.node <- nodeNamer()

trail_scores <- tibble(
  trailname = character(),
  num_trail = numeric()
)

part1answer <- 0

part2answer <- 0

for (child in trail_tree$children) {
  node_check(child, 1)

  end_coords <- child$Get("coordinates", filterFun = function(x) {x$end == 1})
  
  trail_scores <- rbind(trail_scores, c(child$name, as.numeric(sum(trailends %in% end_coords))))
  part1answer <- part1answer + sum(trailends %in% end_coords)
  
  unique_trails <- child$Get("names", filterFun = function(x) {x$end == 1})
  
  part2answer <- part2answer + length(unique_trails)
}


