library(tidyverse)

data <-
  tibble(loc = read_lines(file = "02_data/day_13.txt")) %>%
  filter(!str_detect(loc, "fold")) %>% filter(loc != "") %>%
  separate(col = loc,into = c("x", "y"),sep = ",",convert = T) 

lines <-
  tibble(loc = read_lines(file = "02_data/day_13.txt")) %>% 
  filter(str_detect(loc, "fold")) %>% filter(loc != "") %>% 
  mutate(loc = str_remove(loc, "fold along ")) %>% 
  separate(loc, c("axis","num"), "=", convert = T) %>% 
  mutate(num = as.double(num))


fold <- function(data, axis, num){
out <-
  data %>% 
  mutate(x = if_else(x > num & axis == "x", num - (x-num), as.double(x))) %>% 
  mutate(y = if_else(y > num & axis == "y", num - (y-num), as.double(y)))  
return(out)
}

fold(data = data, axis = lines$axis[1], num = lines$num[1]) %>% count(x,y)

reduce2(.x = lines$axis, .y = lines$num, .f = fold, .init = data) %>% 
  count(x,y)  %>% 
  mutate(y = -1*y) %>% 
  ggplot(aes(x = x, y = y)) +
  geom_point()

