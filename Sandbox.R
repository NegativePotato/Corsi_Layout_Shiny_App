library(dplyr)


add_directional_columns <- function(df) {
  output <- df %>%
    rowwise() %>% 
    mutate(
      x_TR = generate_square_vertices(x, y, side)$x_TR,
      y_TR = generate_square_vertices(x, y, side)$y_TR,
      x_TL = generate_square_vertices(x, y, side)$x_TL,
      y_TL = generate_square_vertices(x, y, side)$y_TL,
      x_BR = generate_square_vertices(x, y, side)$x_BR,
      y_BR = generate_square_vertices(x, y, side)$y_BR,
      x_BL = generate_square_vertices(x, y, side)$x_BL,
      y_BL = generate_square_vertices(x, y, side)$y_BL
    ) %>%
    ungroup() %>%
    mutate(
      left_to_right_next = lead(x) > x,   # TRUE if the next element is to the right
      left_to_right_previous = lag(x) < x,  # TRUE if the previous element is to the left
      bottom_to_top_next = lead(y) > y,   # TRUE if the next element is above
      bottom_to_top_previous = lag(y) < y  # TRUE if the previous element is below
    ) %>%
    # tidyr::replace_na(list(
    #   left_to_right_next = FALSE,
    #   left_to_right_previous = FALSE,
    #   bottom_to_top_next = FALSE,
    #   bottom_to_top_previous = FALSE
    # )) %>%  # Replace NA values (first or last elements) with FALSE
    mutate(
      x1_current = ifelse(left_to_right_next & bottom_to_top_next, x_TL,
                          ifelse(!left_to_right_next & bottom_to_top_next, x_BL,
                                 ifelse(left_to_right_next & !bottom_to_top_next, x_TR, x_BR))),
      y1_current = ifelse(left_to_right_next & bottom_to_top_next, y_TL, 
                          ifelse(!left_to_right_next & bottom_to_top_next, y_BL, 
                                 ifelse(left_to_right_next & !bottom_to_top_next, y_TR, y_BR))), 
      x2_current = ifelse(left_to_right_next & bottom_to_top_next, x_BR, 
                          ifelse(!left_to_right_next & bottom_to_top_next, x_TR, 
                                 ifelse(left_to_right_next & !bottom_to_top_next, x_BL, x_TL))),
      y2_current = ifelse(left_to_right_next & bottom_to_top_next, y_BR, 
                          ifelse(!left_to_right_next & bottom_to_top_next, y_TR, 
                                 ifelse(left_to_right_next & !bottom_to_top_next, y_BL, y_TL))), 
      x1_next = ifelse(left_to_right_next & bottom_to_top_next, lead(x_TL), 
                       ifelse(!left_to_right_next & bottom_to_top_next, lead(x_BL), 
                              ifelse(left_to_right_next & !bottom_to_top_next, lead(x_TR), lead(x_BR)))),
      y1_next = ifelse(left_to_right_next & bottom_to_top_next, lead(y_TL), 
                       ifelse(!left_to_right_next & bottom_to_top_next, lead(y_BL), 
                              ifelse(left_to_right_next & !bottom_to_top_next, lead(y_TR), lead(y_BR)))), 
      x2_next = ifelse(left_to_right_next & bottom_to_top_next, lead(x_BR), 
                       ifelse(!left_to_right_next & bottom_to_top_next, lead(x_TR), 
                              ifelse(left_to_right_next & !bottom_to_top_next, lead(x_BL), lead(x_TL)))),
      y2_next = ifelse(left_to_right_next & bottom_to_top_next, lead(y_BR), 
                       ifelse(!left_to_right_next & bottom_to_top_next, lead(y_TR), 
                              ifelse(left_to_right_next & !bottom_to_top_next, lead(y_BL), lead(y_TL))))
    ) %>%
    select(label, x1_current, y1_current, x2_current, y2_current, x1_next, y1_next, x2_next, y2_next) %>% 
    filter(!is.na(x1_current))
  print(output)
  
  output <- output %>%
    tidyr::pivot_longer(cols = contains("_"),
                        names_to = c(".value", "element"), 
                        names_sep = "_")
  
  return(output)
}

# Example usage:
# df <- data.frame(label = c("A", "B", "C"), x = c(1, 3, 2), y = c(1, 2, 3))
# add_directional_columns(df)

a <- data.frame(x = c(100, 200, 300), y = c(0, 400, 150))
add_directional_columns(a)

test <- read.csv("squares_data_2.csv") %>%
  rowwise() %>% 
  mutate(
    x_TR = generate_square_vertices(x, y, side)$x_TR,
    y_TR = generate_square_vertices(x, y, side)$y_TR,
    x_TL = generate_square_vertices(x, y, side)$x_TL,
    y_TL = generate_square_vertices(x, y, side)$y_TL,
    x_BR = generate_square_vertices(x, y, side)$x_BR,
    y_BR = generate_square_vertices(x, y, side)$y_BR,
    x_BL = generate_square_vertices(x, y, side)$x_BL,
    y_BL = generate_square_vertices(x, y, side)$y_BL
  ) %>%
  ungroup() 

labels_overlap <- c("5", "2", "4", "6")
test_filtered <- test %>% 
  filter(label %in% labels_overlap) %>% 
  arrange(match(label, labels_overlap)) %>%
  mutate(
    left_to_right_next = lead(x) > x,   # TRUE if the next element is to the right
    left_to_right_previous = lag(x) < x,  # TRUE if the previous element is to the left
    bottom_to_top_next = lead(y) > y,   # TRUE if the next element is above
    bottom_to_top_previous = lag(y) < y  # TRUE if the previous element is below
  ) %>% 
  mutate(
    x1_current = ifelse(left_to_right_next & bottom_to_top_next, x_TL,
                        ifelse(!left_to_right_next & bottom_to_top_next, x_BL,
                               ifelse(left_to_right_next & !bottom_to_top_next, x_TR, x_BR))),
    y1_current = ifelse(left_to_right_next & bottom_to_top_next, y_TL, 
                        ifelse(!left_to_right_next & bottom_to_top_next, y_BL, 
                               ifelse(left_to_right_next & !bottom_to_top_next, y_TR, y_BR))), 
    x2_current = ifelse(left_to_right_next & bottom_to_top_next, x_BR, 
                        ifelse(!left_to_right_next & bottom_to_top_next, x_TR, 
                               ifelse(left_to_right_next & !bottom_to_top_next, x_BL, x_TL))),
    y2_current = ifelse(left_to_right_next & bottom_to_top_next, y_BR, 
                        ifelse(!left_to_right_next & bottom_to_top_next, y_TR, 
                               ifelse(left_to_right_next & !bottom_to_top_next, y_BL, y_TL))), 
    x1_next = ifelse(left_to_right_next & bottom_to_top_next, lead(x_TL), 
                     ifelse(!left_to_right_next & bottom_to_top_next, lead(x_BL), 
                            ifelse(left_to_right_next & !bottom_to_top_next, lead(x_TR), lead(x_BR)))),
    y1_next = ifelse(left_to_right_next & bottom_to_top_next, lead(y_TL), 
                     ifelse(!left_to_right_next & bottom_to_top_next, lead(y_BL), 
                            ifelse(left_to_right_next & !bottom_to_top_next, lead(y_TR), lead(y_BR)))), 
    x2_next = ifelse(left_to_right_next & bottom_to_top_next, lead(x_BR), 
                     ifelse(!left_to_right_next & bottom_to_top_next, lead(x_TR), 
                            ifelse(left_to_right_next & !bottom_to_top_next, lead(x_BL), lead(x_TL)))),
    y2_next = ifelse(left_to_right_next & bottom_to_top_next, lead(y_BR), 
                     ifelse(!left_to_right_next & bottom_to_top_next, lead(y_TR), 
                            ifelse(left_to_right_next & !bottom_to_top_next, lead(y_BL), lead(y_TL))))
  ) %>%
  select(label, x1_current, y1_current, x2_current, y2_current, x1_next, y1_next, x2_next, y2_next) %>% 
  filter(!is.na(x1_current))

output <- test_filtered %>%
  tidyr::pivot_longer(cols = contains("_"),
                      names_to = c(".value", "element"), 
                      names_sep = "_")

test_directional <- add_directional_columns(test_filtered)  
test_directional


test <- read.csv("squares_data_2.csv") 
test_overlap <- read.csv("sequence_data_2.csv") 
test_overlap_2 <- test_overlap %>%
  mutate(edge_number = rep(seq(nrow(test_overlap)/2), each = 2)) %>%
  group_by(edge) %>% 
  dplyr::reframe(
    edge_number = rep(edge_number[1], 5),
    edge = rep(edge[1], 5),
    vertex_id = seq(5),
    x = c(x1[1], x2, x1[2], x1[1]), 
    y = c(y1[1], y2, y1[2], y1[1])) %>% 
  arrange(edge_number, vertex_id)
test_overlap_2
intersections <- test_overlap_2 %>% 
  group_by(edge) %>% 
  dplyr::reframe(find_intersecting_squares(test, data.frame(x = x, y = y)))
intersections

