library(dplyr)

rules_in <- read.table("day5/data/rules.txt", sep = "|")
updates_in <- read.table("day5/data/update.txt", sep = ",", fill = TRUE)


# Part 1 ------------------------------------------------------------------
sort_compare <- function(x) {
  curr_update <- x |>
    as.numeric() |>
    na.omit()
  
  correct_order <- c(curr_update[1])
  ord_vec <- c()
  
  for (curr_page in curr_update) {
    ord_count <- 0
    
    test_pages <- curr_update[!(curr_update %in% curr_page)]
    
    for (oth_page in test_pages) {
      ord_count <- ifelse(curr_page %in% rules_in[rules_in$V1 == oth_page, 2],
                          ord_count + 1,
                          ord_count)
    }
    
    ord_vec <- c(ord_vec, ord_count)
  }

  correct_order <- data.frame(curr_update, ord_vec) |>
    arrange(ord_vec) |>
    pull(curr_update)
  
  if (all(curr_update == correct_order)) {
    middle_value <- curr_update[ceiling(length(curr_update) / 2)]
  } else {
    middle_value <- 0
  }
  
  return(middle_value)
}

order_check <- apply(updates_in, 1, sort_compare)

sum(order_check)

# Part 2 ------------------------------------------------------------------
sort_compare_incorrect <- function(x) {
  curr_update <- x |>
    as.numeric() |>
    na.omit()
  
  correct_order <- c(curr_update[1])
  ord_vec <- c()
  
  for (curr_page in curr_update) {
    ord_count <- 0
    
    test_pages <- curr_update[!(curr_update %in% curr_page)]
    
    for (oth_page in test_pages) {
      ord_count <- ifelse(curr_page %in% rules_in[rules_in$V1 == oth_page, 2],
                          ord_count + 1,
                          ord_count)
    }
    
    ord_vec <- c(ord_vec, ord_count)
  }
  
  correct_order <- data.frame(curr_update, ord_vec) |>
    arrange(ord_vec) |>
    pull(curr_update)
  
  if (!all(curr_update == correct_order)) {
    middle_value <- correct_order[ceiling(length(correct_order) / 2)]
  } else {
    middle_value <- 0
  }
  
  return(middle_value)
}

order_check_incorrect <- apply(updates_in, 1, sort_compare_incorrect)

sum(order_check_incorrect)
