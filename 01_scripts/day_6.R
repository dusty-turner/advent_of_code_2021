library(tidyverse)

## attempt 1
initial_fish <- tibble(fish = read_lines("02_data/day_6.txt")) %>%
  separate_rows(fish, sep = ",", convert = T) 

progress_day <- function(fish = initial_fish, after_day = 1){
fish <-
  fish %>% 
  mutate(fish = ifelse(fish <= 0 , "6,8", fish-1)) %>% 
  separate_rows(fish, sep = ",", convert = T) 
return(fish)
}

tictoc::tic()
1:80 %>% 
  reduce(.f = progress_day, .init = initial_fish) %>% nrow()
tictoc::toc()


## attempt 2
initial_fish <- as.numeric(unlist(strsplit(read_lines("02_data/day_6.txt"),",")))

progress_day <- function(fish = initial_fish, after_day = 1) {
  
  add_fish <- sum(fish == 0)
  next_day <- fish - 1
  next_day[next_day < 0] <- 6

  out <- c(next_day, rep(8, add_fish))
return(out)  
}

tictoc::tic()
1:80 %>% 
  reduce(.f = progress_day, .init = initial_fish) %>% length()
tictoc::toc()


fish <- as.numeric(unlist(strsplit(read_lines("02_data/day_6.txt"),",")))

initial_fish <- c(
length(fish[fish==8]),
length(fish[fish==7]),
length(fish[fish==6]),
length(fish[fish==5]),
length(fish[fish==4]),
length(fish[fish==3]),
length(fish[fish==2]),
length(fish[fish==1]),
length(fish[fish==0]))

progress_day <- function(fish = initial_fish, after_day = 1){

births = fish[9]
get_older <- replace_na(lag(fish),0)
fish = c(births, get_older[2], get_older[3]+births, get_older[-c(1:3)])
out <- fish
print(after_day)
return(out)
}


tictoc::tic()
# 1:256 %>% 
1:256 %>% 
  reduce(.f = progress_day, .init = initial_fish) %>% sum()
tictoc::toc()
