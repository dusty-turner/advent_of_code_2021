library(tidyverse)

## 1
tibble(binary = read_lines("02_data/day_3.txt")) %>% 
  transmute(col = str_split_fixed(binary, pattern = "", n = nchar(binary))) %>% 
  pull(col) %>% as_tibble() %>% 
  summarise(across(.cols = everything(), .fns = ~if_else(mean(as.numeric(.))>.5,1,0))) %>% 
  pivot_longer(cols = everything(), values_to = "gamma", names_to = "slot")  %>% 
  mutate(epsilon = if_else(gamma == 1, 0, 1)) %>% 
  pivot_longer(cols = -slot) %>% 
  pivot_wider(names_from = slot, values_from = value) %>% 
  unite(col = "binary",  where(is.double), sep = "")  %>% 
  mutate(decimal = strtoi(as.double(binary), base = 2)) %>% 
  pull(decimal) %>% prod()

## 2

data <-
tibble(binary = read_lines("02_data/day_3.txt")) %>% 
  transmute(col = str_split_fixed(binary, pattern = "", n = nchar(binary))) %>% 
  pull(col) %>% as_tibble() %>% 
  summarise(across(.cols = everything(), .fns = ~as.numeric(.))) 

filter_rating <- function(data, col = "V1", type = "oxygen"){
  if(nrow(data)==1){out <- data} 
  else if(type == "oxygen"){out <- data[data[,col]==if_else(mean(data[,col][[1]])>=.5,1,0),]}
  else if(type == "co2"){out <- data[data[,col]==if_else(mean(data[,col][[1]])<.5,1,0),]}
}
  
reduce(.x = names(data), .f = filter_rating, .init = data, "oxygen") %>%
  unite(col = "binary",everything(), sep = "") %>% 
  pull() %>% 
  strtoi(base = 2) *

reduce(.x = names(data), .f = filter_rating, .init = data, "co2") %>%
  unite(col = "binary",everything(), sep = "") %>% 
  pull() %>% 
  strtoi(base = 2)
