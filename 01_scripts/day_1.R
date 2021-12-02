library(tidyverse)

## 1
tibble(measurements = as.numeric(read_lines("02_data/day1.txt"))) %>%  
  filter(measurements > lag(measurements,1)) %>% 
  nrow()
  
## 2
tibble(measurements = as.numeric(read_lines("02_data/day1.txt"))) %>%  
  mutate(roll_sum = zoo::rollsum(measurements,3, fill = NA, align = "right")) %>% 
  filter(roll_sum > lag(roll_sum,1)) %>% 
  nrow()