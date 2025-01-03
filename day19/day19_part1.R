####Leaving room for libraries to add as needed

##Read in data
input_file <- "input.txt"

###Read In File
in_data <- readLines(paste0("day19/data/",input_file), warn = FALSE)

towels <- unlist(strsplit(in_data[1], ", "))
patterns <- in_data[3:length(in_data)]

###Get maximum size of towels
max_size <- max(nchar(towels))

pattern_match <- function(pattern, max_size) {
  for (size in 1:max_size) {
    section <- substr(pattern, 1, size)
    
    if (section %in% towels) {
      new_section <- substr(pattern,size + 1, nchar(pattern))
      
      if (new_section == "") {
        return(TRUE)
      } else {
        rc <- pattern_match(new_section, max_size)
        
        if (rc == TRUE) {return(TRUE)}
      }
    }
  }
  
  return(FALSE)
}

pattern_list <- sapply(patterns, function(x) {pattern_match(x, max_size)})

sum(pattern_list)
