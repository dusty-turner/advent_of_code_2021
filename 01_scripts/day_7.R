library(tidyverse)

dat <-
  read_lines("02_data/day_7.txt") %>% str_split(",") %>% unlist() %>% as.numeric()


distance <- function(data = dat, position = 1) {
  out <- tibble(distance = sum(abs(data - position)), position = position)
  return(out)
}

min(dat):max(dat) %>% 
  map_dfr(.f = ~distance(data = dat, position = .x)) %>% 
  filter(distance == min(distance))


distance_plus <- function(data = dat, position = 5) {
  
  n<-abs(data-position)+1
  fuel <- sum(n*(n-1)/2)
  out <- tibble(fuel = fuel, position = position)
  return(out)
}

distance_plus(data = dat, position = 1)


min(dat):max(dat) %>% 
  map_dfr(.f = ~distance_plus(data = dat, position = .x)) %>% 
  filter(fuel == min(fuel))
    



fuel_calculation <- function(data = dat, position = 1, type) {
  if(type == "distance"){out <- tibble(fuel = sum(abs(data - position)), position = position, type)}
  if(type == "triangular"){out <- tibble(fuel = sum((abs(data-position)+1)*((abs(data-position)+1)-1)/2), position = position, type)}
  return(out)
}

rep(min(dat):max(dat),2) %>% 
  map2_dfr(.y = c(rep("distance",length(min(dat):max(dat))),rep("triangular",length(min(dat):max(dat)))),.f = ~fuel_calculation(data = dat, position = .x, type = .y)) %>% 
  group_split(type) %>% 
  map(~filter(.x,fuel == min(fuel)))