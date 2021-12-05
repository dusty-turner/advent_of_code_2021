library(tidyverse)

movements <-
# tibble(directions = read_lines("02_data/day_5_temp.txt")) %>%
tibble(directions = read_lines("02_data/day_5.txt")) %>%
  mutate(directions = str_replace_all(string = directions, pattern = " -> ", ",")) %>% 
  separate(col = directions, into = c("x1", "y1", "x2", "y2"), sep = ",", convert = T) %>% 
  select(x1,x2,y1,y2) %>% 
  filter(x1==x2 | y1==y2) 
  # mutate(minx1 = min(x1,x2)) %>% 
  # group_split(id = row_number()) %>% 
  # map(~select(.,-id)) 

max <- movements %>% pivot_longer(cols = everything()) %>% summarise(max(value)) %>% pull()

create_lines <- function(lines){

move <- lines

if(move$y1==move$y2){  
xlow  <- min(move$x1, min(move$x2))
xhigh <- max(move$x1, min(move$x2))
ylow  <- min(move$y1, min(move$y2))
yhigh <- max(move$y1, min(move$y2))
byrow <- T
} else if(move$x1==move$x2){
  ylow  <- min(move$x1, min(move$x2))
  yhigh <- max(move$x1, min(move$x2))
  xlow  <- min(move$y1, min(move$y2))
  xhigh <- max(move$y1, min(move$y2))
 byrow <- F
}

  
out <-
matrix(
c(
rep(rep(0,10),ylow),

c(rep(0,xlow),rep(1,xhigh-xlow+1), rep(0,9-xhigh)),

rep(rep(0,10),9-ylow)
), byrow = byrow, nrow = 10, ncol = 10)

return(out)

}

movements %>% group_split(row_number(), .keep = F) %>% 
  purrr::map(~create_lines(lines = .x)) %>% 
  reduce(`+`) %>% as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  filter(value >=2) %>% 
  count()

