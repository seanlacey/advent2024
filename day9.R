library(stringr)
library(dplyr)

input_file <- "input.txt"

data_in <- readLines(paste0("day9/data/",input_file), warn = FALSE)

string_size <- nchar(data_in) |> as.numeric()

###Parse out string into vector
string_vec <- str_split_fixed(data_in, pattern = "", n = string_size) |> as.numeric()

###Grab the file portion of the vector and add IDs
file_vec <- string_vec[c(TRUE, FALSE)]
file_mat <- rbind(0:(length(file_vec) - 1), file_vec)

###Adding two buffer columns at the beginning to ensure file_mat stays a matrix
file_mat <- cbind(c(999999,999999), c(999999,999999), file_mat)

###Grab the free space portion of the vector
free_vec <- string_vec[c(FALSE, TRUE)]


#Part 1 -------------------------------------------------------------------------

###Initialize ordered file list and remove added from file_mat
ordered_files <- file_mat[,3]
file_mat <- file_mat[,-3]

###Define Free Space Variable
free_space <- free_vec[1]

while (ncol(file_mat) > 2) {
  if (file_mat[2,ncol(file_mat)] <= free_space) {
    ###Add file to ordered files and remove from file_mat
    ordered_files <- cbind(ordered_files, c(file_mat[1,ncol(file_mat)], file_mat[2,ncol(file_mat)]))

    ###Update amount of free space and check for 0
    free_space <- free_space - file_mat[2,ncol(file_mat)]

    ###Remove file from end of file_mat
    file_mat <- file_mat[,-ncol(file_mat)]

    ###If out of free space,
    if (free_space == 0) {
      #remove from free_vec
      free_vec <- free_vec[-1]

      #Add next file to ordered_files
      ordered_files <- cbind(ordered_files, c(file_mat[1,3], file_mat[2,3]))

      ###Remove from file_mat
      file_mat <- file_mat[,-3]

      ###Grab next free_space
      free_space <- free_vec[1]
    }
  } else if (free_space == 0) {
    ###This is for cases where the value in the original data for free space was 0
    free_vec <- free_vec[-1]

    #Add next file to ordered_files
    ordered_files <- cbind(ordered_files, c(file_mat[1,3], file_mat[2,3]))

    ###Remove from file_mat
    file_mat <- file_mat[,-3]

    ###Grab next free_space
    free_space <- free_vec[1]

  } else if (ncol(file_mat) == 3) {
    ###Special case for the end of the file to prevent splitting the last file
    ###unnecessarily

    ###Grab last value and add to ordered_files
    ordered_files <- cbind(ordered_files, c(file_mat[1,3], file_mat[2,3]))

    ###Remove from file_mat
    file_mat <- file_mat[,-3]
  }  else {
    ###If there is not enough space for the whole file, add amount we can to ordered files
    ordered_files <- cbind(ordered_files, c(file_mat[1,ncol(file_mat)], free_space))

    ###Update file_mat with new space amount
    file_mat[2,ncol(file_mat)] <- file_mat[2,ncol(file_mat)] - free_space

    ###Remove from free_vec
    free_vec <- free_vec[-1]

    #Add next file to ordered_files
    ordered_files <- cbind(ordered_files, c(file_mat[1,3], file_mat[2,3]))

    ###Remove from file_mat
    file_mat <- file_mat[,-3]

    ###Grab next free_space
    free_space <- free_vec[1]
  }
}

###Checksum
x <- ordered_files

position <- 0
part1total <- 0

for (i in 1:ncol(x)) {
  for (j in 1:x[2,i]) {
    part1total <- part1total + (x[1,i] * position)
    position <- position + 1
  }
}

#Part 2 ------------------------------------------------------------------
###Function to find free spaces to left of current file
free_to_left <- function(file_n) {
  curr_col <- which(colnames(disk_mat) == file_n)
  
  cols_to_left <- grep("FREE", colnames(disk_mat)[1:curr_col], value = TRUE)
  
  return(cols_to_left)
}


adjacent_col <- function(file_n) {
  ###Get adjacent columns
  curr_col <- which(colnames(disk_mat) == file_n)
  
  cols_vec <- colnames(disk_mat)[c(curr_col - 1, curr_col + 1)]
  
  cols_vec[1] <- ifelse(is.na(cols_vec[1]), "NONE", cols_vec[1])
  cols_vec[2] <- ifelse(is.na(cols_vec[2]), "NONE", cols_vec[2])
  
  return(cols_vec)
}

###Create disk matrix retianing free space
disk_mat <- rbind(0:(length(string_vec) - 1), string_vec)

disk_mat[1,] <- ifelse(disk_mat[1,] %% 2 == 0, disk_mat[1,] / 2, 999999)

###Add row indicating if file (TRUE) or free (FALSE)
disk_mat <- rbind(disk_mat, !(disk_mat[1,] == 999999))

###Get number of files and free space)
num_files <- ncol(disk_mat[,disk_mat[3,] == TRUE]) |> as.numeric()
num_free <- ncol(disk_mat[,disk_mat[3,] == FALSE]) |> as.numeric()

###Interlacing the names
file_names <- paste0("FILE", 0:(num_files - 1))
free_names <- paste0("FREE", 0:(num_free - 1))

disk_names <- rbind(file_names, free_names) |>
  as.vector()

###If num_files and num_free are unequal, this will remove
###extra names
disk_names <- disk_names[1:(num_files + num_free)]

colnames(disk_mat) <- disk_names

###Name rows
rownames(disk_mat) <- c("id", "size", "type")

###Making tibble to use relocate function
disk_mat <- as_tibble(disk_mat)

###Used for naming new free columns as needed to avoid conflicts
free_iter <- num_free

####Begin loopping through files
for (file_n in rev(file_names)) {
  print(file_n)

  file_size <- disk_mat[2,file_n] |> as.numeric()

  ####Get list of free spaces to search
  free_search <- free_to_left(file_n)

  ###Get adjacent columns
  cols_left_right <- adjacent_col(file_n)

  for (free_n in free_search) {

    free_size <- disk_mat[2,free_n] |> as.numeric()

    if (file_size < free_size) {
      remaining_size <- free_size - file_size

      disk_mat <- disk_mat |>
        relocate(all_of(file_n), .before = all_of(free_n))

      disk_mat[2, free_n] <- remaining_size

      ###If columns to left and right of initial file location are free,
      ###combine together and add remaining free space
      ###Else If column to left is free, add
      ###remaining there
      ###Else if column to right is free, add remaining there
      ###Else add new column and relocate after column to left
      if (str_detect(cols_left_right[1], "FREE") &
          str_detect(cols_left_right[2], "FREE")) {
        disk_mat[2, cols_left_right[1]] <- disk_mat[2, cols_left_right[1]] +
          disk_mat[2, cols_left_right[2]] + file_size

        disk_mat <- select(disk_mat, -any_of(cols_left_right[2]))
      } else if (str_detect(cols_left_right[1], "FREE")) {
        disk_mat[2, cols_left_right[1]] <- disk_mat[2, cols_left_right[1]] + file_size
      } else if (str_detect(cols_left_right[2], "FREE")) {
        disk_mat[2, cols_left_right[2]] <- disk_mat[2, cols_left_right[2]] + file_size
      } else {
        disk_mat[,paste0("FREE",free_iter)] <- c(999999, file_size, 0)

        disk_mat <- disk_mat |>
          relocate(all_of(paste0("FREE",free_iter)), .after = all_of(cols_left_right[1]))

        free_iter <- free_iter + 1
      }
      break
    } else if (file_size == free_size) {
      disk_mat <- disk_mat |>
        relocate(all_of(file_n), .before = all_of(free_n))

      ###Remove Free_N
      disk_mat <- select(disk_mat, -all_of(free_n))

      ###Check to see if one of the adjacent columns is one we removed
      cols_left_right[1] <- ifelse(cols_left_right[1] == free_n, "NONE", cols_left_right[1])
      cols_left_right[2] <- ifelse(cols_left_right[2] == free_n, "NONE", cols_left_right[2])

      ###If columns to left and right of initial file location are free,
      ###combine together and add remaining free space
      ###Else If column to left is free, add
      ###remaining there
      ###Else if column to right is free, add remaining there
      ###Else add new column and relocate after column to left
      if (str_detect(cols_left_right[1], "FREE") &
          str_detect(cols_left_right[2], "FREE")) {
        disk_mat[2, cols_left_right[1]] <- disk_mat[2, cols_left_right[1]] +
          disk_mat[2, cols_left_right[2]] + file_size

        disk_mat <- select(disk_mat, -any_of(cols_left_right[2]))
      } else if (str_detect(cols_left_right[1], "FREE")) {
        disk_mat[2, cols_left_right[1]] <- disk_mat[2, cols_left_right[1]] + file_size
      } else if (str_detect(cols_left_right[2], "FREE")) {
        disk_mat[2, cols_left_right[2]] <- disk_mat[2, cols_left_right[2]] + file_size
      } else if (cols_left_right[1] == "NONE" & cols_left_right[2] == "NONE") {
        disk_mat[,paste0("FREE",free_iter)] <- c(999999, file_size, 0)
        
        free_iter <- free_iter + 1
      } else {
        disk_mat[,paste0("FREE",free_iter)] <- c(999999, file_size, 0)
        
        if (cols_left_right[1] == "NONE") {
          disk_mat <- disk_mat |>
            relocate(all_of(paste0("FREE",free_iter)), .before = all_of(cols_left_right[2]))
        } else {
          disk_mat <- disk_mat |>
            relocate(all_of(paste0("FREE",free_iter)), .after = all_of(cols_left_right[1])) 
        }
        
        free_iter <- free_iter + 1
      }
      break
    }
  }
}

# input_out <- disk_mat

if (0 %in% disk_mat[2,]) {
  disk_mat <- disk_mat[,-which(disk_mat[2,] == 0 & disk_mat[1,] == 999999)] 
}

###Checksum Function
x <- disk_mat

position <- 0
part2total <- 0

for (i in 1:ncol(x)) {
  for (j in 1:as.numeric(x[2,i])) {
    if (x[1,i] != 999999) {
      part2total <- part2total + as.numeric(x[1,i]) * position
    }

    position <- position + 1
  }
}
