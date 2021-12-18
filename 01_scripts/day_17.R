library(tidyverse)

# min_x <- 20
# max_x <- 30
# min_y <- -10
# max_y <- -5

min_x <- 201
max_x <- 230
min_y <- -99
max_y <- -65

start <- tibble(x = 0, y = 0)

move_one <- function(start, initial_x, initial_y){
  end <-
  start %>% 
    mutate(x = x+initial_x, y = y + initial_y)
  return(end)
  
}


# x can be no higher than max x + 1 = 30
# x must be higher than 


x_poss <- map_dbl(1:max_x, ~choose(.x,2))
x_ans <- c(match(x_poss[which(x_poss %in% min_x:max_x)], x_poss)) - 1

y_ans <- abs(min_y)-1



check_trajectory_part_1 <- function(x_init = 7, y_init = -1){

initial_velocity_x <- x_init
final_velocity_x <- initial_velocity_x-max_x
initial_velocity_y <- y_init

x_velocity_vec <- initial_velocity_x:final_velocity_x
x_velocity_vec[which(x_velocity_vec<0)]=0
y_velocity_vec <- (initial_velocity_y:-200)[1:length(x_velocity_vec)]


if(sum(cumsum(x_velocity_vec) %in% min_x:max_x) == 0){
  out <- tibble(it_works = FALSE, x_init = x_init, y_init =y_init)
  return(out)
}
if(sum(cumsum(y_velocity_vec) %in% min_y:max_y) == 0){
  # out <- tibble(x = NULL,y = NULL, all_in = NULL, x_init = x_init, y_init =y_init)
  out <- tibble(it_works = FALSE, x_init = x_init, y_init =y_init)
  return(out)
}

if(sum((cumsum(x_velocity_vec) %in% min_x:max_x)[which(cumsum(y_velocity_vec) %in% min_y:max_y == TRUE)])>0){
  out <- tibble(it_works = TRUE, x_init = x_init, y_init =y_init)
  return(out)
}

out <- tibble(it_works = FALSE, x_init = x_init, y_init =y_init)
return(out)

## part1

# trajectory <-
# accumulate2(.x = x_velocity_vec, .y = y_velocity_vec, .f = move_one, .init = start) %>%
#   bind_rows()

# out<-
# trajectory %>% 
#   mutate(all_in = x %in% min_x:max_x + y %in% max_y:min_y == 2) %>% 
#   mutate(one_in = sum(all_in)) %>%
#   filter(one_in !=0) %>% 
#   arrange(desc(y)) %>% 
#   slice(2) %>% 
#   mutate(initial_x = x_init, initial_y = y_init)
# return(out)
}


check_trajectory_part_1(x_init = 20,y_init = 9)
check_trajectory_part_1(x_init = 7,y_init = 9)

possible_x_vec <- x_ans[1]:max_x
possible_y_vec <- y_ans:min_y

possibilities <- crossing(possible_x_vec, possible_y_vec) 

we_know_these_exist <- crossing(possible_x_vec = min_x:max_x, possible_y_vec = min_y:max_y)

possibilities <-
possibilities %>% anti_join(we_know_these_exist) 

library(furrr)
plan(multisession)

answers <- future_map2(.x = possibilities$possible_x_vec, .y = possibilities$possible_y_vec, .f = ~check_trajectory_part_1(x_init = .x,y_init = .y), .progress = T)

answers %>% bind_rows(.id = "id") %>% filter(it_works) %>%  nrow() +
we_know_these_exist %>% nrow()

