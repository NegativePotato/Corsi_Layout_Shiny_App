library(dplyr)


add_directional_columns <- function(df) {
  output <- df %>%
    arrange(x, y) %>%  # Ensure correct order based on x and y positions
    mutate(
      left_to_right_next = lead(x) > x,   # TRUE if the next element is to the right
      left_to_right_previous = lag(x) < x,  # TRUE if the previous element is to the left
      bottom_to_top_next = lead(y) > y,   # TRUE if the next element is above
      bottom_to_top_previous = lag(y) < y  # TRUE if the previous element is below
    ) %>%
    tidyr::replace_na(list(
      left_to_right_next = FALSE,
      left_to_right_previous = FALSE,
      bottom_to_top_next = FALSE,
      bottom_to_top_previous = FALSE
    )) %>%  # Replace NA values (first or last elements) with FALSE
    mutate(
      x1 = ifelse(left_to_right_next & bottom_to_top_next, x_TL, 
                  ifelse(!left_to_right_next & bottom_to_top_next, x_BL, 
                         ifelse(left_to_right_next & !bottom_to_top_next, x_TR, x_BR))),
      y1 = ifelse(left_to_right_next & bottom_to_top_next, y_TL, 
                  ifelse(!left_to_right_next & bottom_to_top_next, y_BL, 
                         ifelse(left_to_right_next & !bottom_to_top_next, y_TR, y_BR))), 
      x2 = ifelse(left_to_right_next & bottom_to_top_next, lead(x_TL), 
                  ifelse(!left_to_right_next & bottom_to_top_next, lead(x_BL), 
                         ifelse(left_to_right_next & !bottom_to_top_next, lead(x_TR), lead(x_BR)))),
      y2 = ifelse(left_to_right_next & bottom_to_top_next, lead(y_TL), 
                  ifelse(!left_to_right_next & bottom_to_top_next, lead(y_BL), 
                         ifelse(left_to_right_next & !bottom_to_top_next, lead(y_TR), lead(y_BR))))
    )
  
  return(output)
}

# Example usage:
# df <- data.frame(label = c("A", "B", "C"), x = c(1, 3, 2), y = c(1, 2, 3))
# add_directional_columns(df)

a <- data.frame(x = c(100, 200, 300), y = c(0, 400, 150))
add_directional_columns(a)
