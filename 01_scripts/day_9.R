library(tidyverse)

##1
input <-
matrix(read_lines("02_data/day_9.txt")) %>% 
  strsplit("") %>% unlist() %>% as.numeric() %>% matrix(byrow = T, nrow = 100) 

checker <- function(row, col){

  if(col < ncol(input)){right <- input[row,col]-input[row,col+1] < 0}else{right<-1}
  if(col != 1){left  <- input[row,col]-input[row,col-1] < 0}else{left<-1}
  if(row != 1){upper <- input[row,col]-input[row-1,col] < 0}else{upper<-1}
  if(row < nrow(input)){lower <- input[row,col]-input[row+1,col] < 0}else{lower<-1}
  
  out <- tibble(number = input[row,col], 
                row = row, 
                col = col,
                num_increase = sum(right,left,upper,lower)
              )
  
  return(out)

}

library(furrr)
plan(multicore)

future_map2_dfr(.x = sort(rep(1:nrow(input),ncol(input))),.y = rep(1:ncol(input),nrow(input)), .f = ~checker(row = .x,col = .y), .progress = T) %>% 
  filter(num_increase == 4) %>% 
  mutate(number = number + 1) %>% 
  summarise(answer = sum(number))

#2
input <-
  matrix(read_lines("02_data/day_9.txt")) %>%
  strsplit("") %>% unlist() %>% str_replace_all(pattern = "9","X") %>% 
  matrix(byrow = T, nrow = 100) 

input <- list(input = input, basin_size = NULL)

sweep <- function(input, row, col){
  if(col != ncol(input))  {input[row,col][input[row,col+1]=="B" && input[row,col] !="X"] <- "B"} # look right
  if(row != nrow(input))  {input[row,col][input[row+1,col]=="B" && input[row,col] !="X"] <- "B"} # look down
  if(col != 1)            {input[row,col][input[row,col-1]=="B" && input[row,col] !="X"] <- "B"} # look left
  if(row != 1)            {input[row,col][input[row-1,col]=="B" && input[row,col] !="X"] <- "B"} # look up
  return(input)
}

id_reduce_count_replace <- function(input){
  input <- unique.matrix(input$input, MARGIN = 1)

  input <- t(input) %>% str_c(collapse = "") %>% str_replace(pattern = "[^X]", replacement = "B") %>% str_split(pattern = "") %>% unlist() %>% matrix(byrow = T, nrow = nrow(input))

  if(nrow(input) > 1){print(input[2,])}
  
  row <- nrow(input)
  identified_basin <- reduce2(.x = 
                                c(sort(rep(1:nrow(input),ncol(input))),rev(sort(rep(1:nrow(input),ncol(input)))),sort(rep(1:nrow(input),ncol(input))),rev(sort(rep(1:nrow(input),ncol(input))))),
                              .y = c(rep(1:ncol(input),nrow(input)),rep(1:ncol(input),nrow(input)),rev(c(rep(1:ncol(input),nrow(input)),rep(1:ncol(input),nrow(input))))), 
                              .f = sweep, .init = input)
  basin_size <- identified_basin %>%  str_count("B") %>% sum()

  input <- str_replace_all(identified_basin,"B", "X") %>% matrix(byrow = F, nrow = row)
  if(sum(str_count(input, pattern = "X"))==sum(nchar(input))){message("Finished")}
  print(x)

return(list(input = input, basin_size = basin_size))
}

tictoc::tic()
accumulate(.x = 1:225, .f = id_reduce_count_replace, .init = input) %>% map(2) %>% unlist() %>% sort() %>% tail(3) %>% prod()
tictoc::toc()

