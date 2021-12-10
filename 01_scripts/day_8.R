library(tidyverse)

# part 1
tibble(entry = read_lines("02_data/day_8.txt")) %>%
  separate(col = entry, into = c("NA","output"),sep = " \\| ") %>% 
  mutate(id = row_number()) %>%
  separate_rows(output, sep = " ") %>% 
  mutate(length = nchar(output)) %>% 
  count(length) %>%
  filter(length %in% c(2,3,4,7)) %>%
  summarise(sum = sum(n))
  

initial <-
  tibble(entry = read_lines("02_data/day_8.txt")) %>%
  separate(col = entry, into = c("signal","output"),sep = " \\| ") %>% 
  mutate(id = row_number()) %>%
  separate_rows(signal, sep = " ") %>% 
  rowwise() %>% 
  mutate(signal = str_c(sort(unlist(str_split(signal,""))), collapse = "")) %>% ungroup() %>% 
  select(-output)

output_tbl <-
  tibble(entry = read_lines("02_data/day_8.txt")) %>%
  separate(col = entry, into = c("signal","output"),sep = " \\| ") %>% 
  mutate(id = row_number()) %>%
  separate_rows(output, sep = " ") %>% 
  rowwise() %>% 
  mutate(output = str_c(sort(unlist(str_split(output,""))), collapse = "")) %>% ungroup() %>% 
  select(-signal) %>% 
  group_by(id) %>% 
  summarise(output = str_c(output, collapse = " ")) %>% 
  ungroup() %>% 
  separate_rows(output, sep = " ") %>% 
  mutate(order = row_number())


identify_obvious <- function(initial){
  out <- initial %>% 
    mutate(number = case_when(nchar(signal) == 2 ~ 1,
                              nchar(signal) == 3 ~ 7,
                              nchar(signal) == 4 ~ 4,
                              nchar(signal) == 7 ~ 8,
                              TRUE ~ 0
                              )) %>% 
    filter(number !=0) %>% 
    arrange(number)
  return(out)
}
  
identify_3 <- function(known, initial){
  seven <-
    known  %>% filter(number %in% c(7)) %>% 
    pull(signal) %>% str_split("") %>% unlist()
  three <-  
    initial %>% 
    filter(nchar(signal) == 5) %>% 
    mutate(split = signal) %>% 
    separate_rows(split, sep = "") %>% 
    filter(split %in% seven) %>% 
    group_by(signal) %>% filter(n() == 3) %>% ungroup(signal) %>% 
    distinct(signal, .keep_all = T) %>% 
    select(-split) %>% 
    mutate(number = 3) %>% 
    bind_rows(known) %>% 
    arrange(number)
  return(three)
}

identify_9 <- function(known, initial){
  three <-
    known  %>% filter(number %in% c(3)) %>% 
    pull(signal) %>% str_split("") %>% unlist()
  nine <-
    initial %>% 
    filter(nchar(signal) == 6) %>% 
    mutate(split = signal) %>% 
    separate_rows(split, sep = "") %>% 
    filter(split %in% three) %>% 
    group_by(signal) %>% filter(n() == 5) %>% ungroup(signal) %>% 
    distinct(signal, .keep_all = T) %>% 
    select(-split) %>% 
    mutate(number = 9) %>% 
    bind_rows(known) %>% 
    arrange(number)
  return(nine)
}

identify_0 <- function(known, initial){
  one <-
    known  %>% 
    filter(number %in% c(1)) %>% 
    pull(signal) %>% 
    str_split("") %>% unlist()
  
  zero <-
    initial %>% 
    anti_join(known, by = c("signal", "id")) %>% 
    filter(nchar(signal) == 6) %>%   
    mutate(split = signal) %>% 
    separate_rows(split, sep = "") %>% 
    filter(split %in% one) %>% 
    group_by(signal) %>% filter(n() == 2) %>% ungroup(signal) %>% 
    distinct(signal, .keep_all = T) %>% 
    select(-split) %>% 
    mutate(number = 0) %>% 
    bind_rows(known) %>% 
    arrange(number)
  return(zero)
}

identify_6 <- function(known, initial){
  known <-
    initial %>% 
    anti_join(known, by = c("signal", "id")) %>% 
    filter(nchar(signal) == 6) %>%
    mutate(number = 6) %>% 
    bind_rows(known) %>% 
    arrange(number)
  return(known)
}

identify_5_2 <- function(known, initial){
  part <-
    known %>% 
    filter(number %in% c(1,4)) %>% 
    separate_rows(signal, sep = "") %>% 
    group_by(signal) %>% filter(n()==1) %>% 
    pull(signal) %>% str_split("") %>% unlist()
  last <- 
    initial %>% 
    anti_join(known, by = c("signal", "id")) %>% 
    mutate(side = signal) %>% 
    separate_rows(side, sep = "")  %>% 
    filter(side %in% part) %>% 
    group_by(signal) %>% 
    mutate(number = if_else(n()==2,5,2)) %>% 
    ungroup() %>% select(-side) %>% distinct() %>% 
    bind_rows(known) %>% 
    arrange(number)
return(last)
}

all_together <- function(initial) {
  all <-
    initial %>%
    identify_obvious() %>%
    identify_3(initial = initial) %>%
    identify_9(initial = initial) %>%
    identify_0(initial = initial) %>%
    identify_6(initial = initial) %>%
    identify_5_2(initial = initial)
  return(all)
}

  initial %>% group_split(id) %>% 
    map_df(~all_together(initial = .x)) %>% 
    left_join(output_tbl, by = c("signal" = "output", "id" = "id")) %>% 
    filter(!is.na(order)) %>% arrange(order) %>% 
    group_by(id) %>% 
    summarise(signal = str_c(signal, collapse = " "), number = as.numeric(str_c(number, collapse = ""))) %>% 
    summarise(solution = sum(number))
  