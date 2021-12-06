library(tidyverse)

movements <-
# tibble(directions = read_lines("02_data/day_5_temp.txt")) %>%
tibble(directions = read_lines("02_data/day_5.txt")) %>%
  mutate(directions = str_replace_all(string = directions, pattern = " -> ", ",")) %>% 
  separate(col = directions, into = c("x1", "y1", "x2", "y2"), sep = ",", convert = T) %>% 
  select(x1,x2,y1,y2) 

create_lines <- function(lines){

move <- lines

if(move$y1==move$y2) {
  xlow  <- min(move$x1, min(move$x2))
  xhigh <- max(move$x1, min(move$x2))
  ylow  <- min(move$y1, min(move$y2))
  yhigh <- max(move$y1, min(move$y2))
  byrow <- T
} else if (move$x1 == move$x2) {
  ylow  <- min(move$x1, min(move$x2))
  yhigh <- max(move$x1, min(move$x2))
  xlow  <- min(move$y1, min(move$y2))
  xhigh <- max(move$y1, min(move$y2))
  byrow <- F
}

out <-
  matrix(
    c(
      rep(rep(0,1000),ylow), ## zeroes before row/col
      c(rep(0,xlow),rep(1,xhigh-xlow+1), rep(0,999-xhigh)), ## row/col of 1s
      rep(rep(0,1000),999-ylow) ## zeroes after row/col
      ), byrow = byrow, nrow = 1000, ncol = 1000
    )

return(out)

}

movements %>% 
  filter(x1==x2 | y1==y2) %>% 
  group_split(row_number(), .keep = F) %>% 
  purrr::map(~create_lines(lines = .x)) %>% 
  reduce(`+`) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  filter(value >=2) %>% 
  count()

###

cross_routes <- function(move){

  xlow  <- min(move$x1, min(move$x2))
  xhigh <- max(move$x1, min(move$x2))
  ylow  <- min(move$y1, min(move$y2))
  yhigh <- max(move$y1, min(move$y2))

mat <-  matrix(rep(0,1000000), ncol = 1000)

if((move$x2>move$x1 & move$y2>move$y1) | (move$x2<move$x1 & move$y2<move$y1)){
  for (i in 1:length(xlow:xhigh)) {
    mat[ylow+i,xlow+i] <- 1
  }
} else{
  for (i in 1:length(xlow:xhigh)) {
    mat[ylow+i,xhigh+2-i] <- 1
  }
  
}

return(mat)

}

a <-
  movements %>% 
  filter(x1==x2 | y1==y2) %>% 
  group_split(row_number(), .keep = F) %>% 
  purrr::map(~create_lines(lines = .x)) %>% 
  reduce(`+`) 

b <-
  movements %>%
  filter(x1!=x2 & y1!=y2) %>%
  group_split(row_number(), .keep = F) %>% 
  purrr::map(~cross_routes(move = .x)) %>% 
  reduce(`+`) 
 
as_tibble(a+b) %>% 
  pivot_longer(cols = everything()) %>%
  filter(value >=2) %>%
  count()
