library(tidyverse)
## 1
tibble(x = read_lines("02_data/day_2.txt")) %>% 
  separate(col = x, into = c("direction","distance"), convert = TRUE) %>% 
  group_by(real_direction = direction == "forward") %>% 
  mutate(distance = ifelse(direction == "up", distance * -1, distance)) %>% 
  summarise(movement = sum(distance)) %>% 
  pull(movement) %>% prod()




tibble(x = read_lines("02_data/day_2.txt")) %>% 
  separate(col = x, into = c("direction","distance"), convert = TRUE) %>%
  mutate(distance = as.double(distance)) %>% 
  mutate(aim_change = case_when(direction == "forward" ~ 0,
                                direction == "down" ~ distance,
                                direction == "up" ~ -1 * distance)) %>% 
  mutate(aim = cumsum(aim_change)) %>% 
  mutate(horizontal_position_increase = ifelse(direction == "forward", distance, 0)) %>% 
  mutate(depth_position_increase = ifelse(direction == "forward", aim * distance,0)) %>% 
  mutate(horizontal_position = cumsum(horizontal_position_increase)) %>% 
  mutate(depth_position = cumsum(depth_position_increase)) %>% 
  slice_tail(n = 1) %>% 
  summarise(answer = horizontal_position * depth_position) 
