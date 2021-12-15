library(tidyverse)

see_matrix <- function(data = x){
  data %>% 
    distinct(row, col, value) %>% 
    select(row,col,value) %>% 
    pull(value) %>% 
    matrix(nrow = 10, byrow = T)
}

x <-
  tibble(x = (read_lines("02_data/day_11.txt"))) %>% 
  mutate(row = row_number()) %>% 
  mutate(value = str_split(x,"")) %>% 
  unnest(value) %>% 
  mutate(value = as.numeric(value)) %>% 
  group_by(row) %>% 
  mutate(col = row_number()) %>% 
  ungroup() %>% 
  relocate(value, .after = col) %>% 
  select(-x)

adj <- expand.grid(xd = c(-1,0,1), yd = c(-1,0,1))

adjacent <- function(x){
  d <- x %>% distinct(row,col,value)
  d2 <- x %>% distinct(row,col,value)
  d %>% 
    crossing(adj) %>% 
    mutate(row2 = row +xd,
           col2 = col + yd) %>%
    arrange(row,col) %>% 
    inner_join(d2, by = c(row2 = "row", col2 = "col"), suffix = c("", "2")) %>% 
    filter(row != row2 | col != col2)  
}

x <- list(x = x, flashes = 0)

flash_octopus <- function(x = x, iteration = 1) {
  x <- x$x %>%
    adjacent() %>%
    mutate(across(.cols = c("value", "value2"), .fns = ~ . + 1))
  
  tens <- 1
  
  while (tens != 0) {
    x <- x %>%
      group_by(row, col) %>%  mutate(add_this_many = sum(value2 >= 10)) %>% ungroup() %>%
      mutate(value = ifelse(value >= 10,-100, value)) %>%
      mutate(value = value + add_this_many) %>%
      adjacent()
    
    tens <- x %>% distinct(row, col, value) %>% filter(value >= 10) %>% nrow()
    
  }
  
  flashes <- x %>% distinct(row, col, value) %>% count(did_flash = value < 0) %>% filter(did_flash) %>% pull(n)
  
  if (length(flashes) == 0) {flashes <- 0}
  
  x <- x %>% mutate(value = ifelse(value < 0, 0, value))
  
  x <- list(x = x, flashes = flashes)
  
  return(x)
}

one_hundred <- accumulate(1:100, .f = flash_octopus, .init = x)

one_hundred %>% map(2) %>% unlist() %>% sum()

three_hundred <- accumulate(1:300, .f = flash_octopus, .init = x)

which(unlist(map(three_hundred, 2)) == 100)[1] - 1
