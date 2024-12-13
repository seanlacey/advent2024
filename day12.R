###Leave area for libraries if needed
library(readr)
library(raster)
library(dplyr)

###Set input file
input_file <- "input.txt"

###Get number of columns
num_col <- nchar(readLines(paste0("day12/data/", input_file), n = 1)) |>
  as.numeric()

in_data <- read_fwf(paste0("day12/data/", input_file), col_positions = fwf_widths(rep(1, num_col))) |>
  as.matrix()

###Get number of rows
num_row <- nrow(in_data) |> 
  as.numeric()

###Create array of coordinate movements indicating up, right, down, left
directions <- complex(real = c(0,1,0,-1), imaginary = c(-1,0,1,0))

###Get unique region identifies
###Note there could be multiple regions within
unique_id <- unique(as.vector(in_data))

#Part 1 ------------------------------------------------------------------
###Extract region(s) from map, and locate clumps (distinct regions)
region_extract <- function(map, region) {
  out_map <- gsub(paste0("[^",region_id,"]"), "0", map)
  out_map <- gsub(paste0("[",region_id,"]"), "1", out_map)
  
  out_map <- mapply(out_map, FUN = as.numeric)
  out_map <- matrix(data = out_map, ncol = num_col, nrow = num_row)
  
  ###Use Raster to find distinct regions
  out_map <- raster(out_map)
  out_map <- as.matrix(clump(out_map, directions = 4))
  
  return(out_map)
}

###Count edges for a specific coordinate
edge_find <- function(coord) {
  edges <- 0
  
  for (d in directions) {
    new_coord <- coord + d
    
    if (Re(new_coord) < 1 | Re(new_coord) > num_col) {
      edges <- edges + 1
    } else if (Im(new_coord) < 1 | Im(new_coord) > num_col) {
      edges <- edges + 1
    } else if (is.na(region_map[Im(new_coord), Re(new_coord)])) {
      edges <- edges + 1
    }
  }
  
  return(edges)
}

###Initialize region dataframe
region_results <- data.frame(region = character(),
                             area = numeric(),
                             perimeter = numeric())

###Create a map that's just one region id
for (region_id in unique_id) {
  print(paste0("Region ", region_id))

  region_map <- region_extract(in_data, region_id)

  region_count <- na.omit(unique(as.vector(region_map)))

  for (r in region_count) {
    ###Get coordinates for all 1
    region_loc <- which(region_map == r, arr.ind = TRUE)
    region_loc <- complex(real = region_loc[,2], imaginary = region_loc[,1])

    area <- length(region_loc)

    perimeter <- 0

    for (coord in region_loc) {
      perimeter <- perimeter + edge_find(coord)
    }

    region_results[nrow(region_results) + 1, ] <- list(paste0(region_id,r), area, perimeter)
  }
}

region_results$totals <- region_results$area * region_results$perimeter

part1total <- sum(region_results$totals)

#Part 2 ------------------------------------------------------------------
###side function
edge_detect <- function(coord){
  out_vec <- c(Re(coord), Im(coord))
  
  for (d in directions) {
    new_coord <- coord + d
    
    if (Re(new_coord) < 1 | Re(new_coord) > num_col) {
      out_vec <- c(out_vec, 1)
    } else if (Im(new_coord) < 1 | Im(new_coord) > num_col) {
      out_vec <- c(out_vec, 1)
    } else if (is.na(region_map[Im(new_coord), Re(new_coord)])) {
      out_vec <- c(out_vec, 1)
    } else {
      out_vec <- c(out_vec, 0)
    }
  }
  
  return(out_vec)
}

###Initialize region dataframe
region_results2 <- data.frame(region = character(),
                              area = numeric(),
                              sides = numeric())

for (region_id in unique_id) {
  
  region_map <- region_extract(in_data, region_id)
  
  region_count <- na.omit(unique(as.vector(region_map)))
  
  for (r in region_count) {
    ###Get coordinates for all 1
    region_loc <- which(region_map == r, arr.ind = TRUE)
    region_loc <- complex(real = region_loc[,2], imaginary = region_loc[,1])
    
    area <- length(region_loc)
    
    ###Initialize edge dataframe
    edge_df <- data.frame(x_coord = numeric(),
                          y_coord = numeric(),
                          up = numeric(),
                          right = numeric(),
                          down = numeric(),
                          left = numeric())
    
    for (coord in region_loc) {
      edge_df[nrow(edge_df) + 1,] <- edge_detect(coord)
    }
    
    side_vec <- c()
    
    for (d in 1:length(directions)) {
      ###Changing which coord to check first based on direction
      if (d == 1 | d == 3) {
        coords <- c("y_coord", "x_coord")
      } else {
        coords <- c("x_coord", "y_coord") 
      }
      
      dir_tmp <- edge_df[which(edge_df[,d + 2] == 1), c(1,2)] |>
        arrange(!!sym(coords[1]), !!sym(coords[2]))
      
      for (row in 1:nrow(dir_tmp)) {
        if (row == 1) {
          dir_tmp[row, "side"] <- 1
        } else {
          if (dir_tmp[row, coords[1]] == dir_tmp[row - 1, coords[1]] &
              dir_tmp[row, coords[2]] == dir_tmp[row - 1, coords[2]] + 1) {
            dir_tmp[row, "side"] <- dir_tmp[row - 1, "side"]
          } else {
            dir_tmp[row, "side"] <- dir_tmp[row - 1, "side"] + 1
          }
        }
      }
      
      side_vec <- c(side_vec, max(dir_tmp$side))
    }
    
    r <- 1
    
    region_results2[nrow(region_results2) + 1, ] <- list(paste0(region_id,r), area, sum(side_vec))
  } 
}

region_results2$totals <- region_results2$area * region_results2$sides

part2total <- sum(region_results2$totals)



