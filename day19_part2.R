####Leaving room for libraries to add as needed
library(collections)

##Read in data
input_file <- "input.txt"

###Read In File
in_data <- readLines(paste0("day19/data/",input_file), warn = FALSE)

towels <- unlist(strsplit(in_data[1], ", "))
patterns <- in_data[3:length(in_data)]

###Get maximum size of towels
max_size <- max(nchar(towels))

pattern_dict <- dict()

pattern_match <- function(pattern, max_size) {
  if (pattern_dict$has(pattern)) {
    return(pattern_dict$get(pattern))
  }
  
  if (pattern == "") {
    return(1)
  }
  
  p_count <- 0
  
  for (towel in towels) {
    section <- substr(pattern, 1, nchar(towel))
    
    if (section == towel) {
      new_section <- substr(pattern,nchar(towel) + 1, nchar(pattern))
      p_count <- p_count + pattern_match(new_section, max_size)
    }
  }
  
  pattern_dict$set(pattern, p_count)
  
  return(p_count)
}

pattern_list <- sapply(patterns, function(x) {
  start.time <- Sys.time()
  print(paste0("Starting: ",x)) 
  n_combo <- pattern_match(x, max_size)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  return(n_combo)
})

format(sum(pattern_list), scientific = FALSE)
