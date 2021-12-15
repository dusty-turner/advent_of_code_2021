library(tidyverse)

## part 1
pair_insertion_rules <-
  tibble(loc = read_lines(file = "02_data/day_14.txt")) %>% 
  slice(-c(1:2)) %>% 
  separate(loc, into = c("polymer", "insert"))

polymer_template <-
  tibble(polymer = read_lines(file = "02_data/day_14.txt")) %>% slice(1) 

grow_polymer <- function(polymer = polymer_template, iter){
  out <-
    polymer %>%
    separate_rows(polymer, sep = "") %>%
    transmute(polymer = str_c(lag(polymer, 1), polymer)) %>%
    filter(!is.na(polymer)) %>%
    left_join(pair_insertion_rules, by = "polymer") %>%
    transmute(add = replace_na(str_c(
      str_sub(polymer, 1, 1), insert, str_sub(polymer, 2, 2)
    ), str_c(" ", polymer[1]))) %>% 
    mutate(add = str_sub(add, 2, 3)) %>%
    summarise(polymer = str_c(add, collapse = ""))
  return(out)
}

1:10 %>% reduce(.f = grow_polymer, .init = polymer_template) %>% 
  separate_rows(polymer, sep = "") %>% 
  count(polymer) %>% 
  filter(polymer !="") %>% 
  summarise(answer = max(n)-min(n))

## part 2
first <-
polymer_template %>% 
  separate_rows(polymer, sep = "") %>% 
  transmute(polymer = str_c(lag(polymer,1), polymer)) %>% 
  filter(nchar(polymer)==2) %>% 
  count(polymer)

iterate_step <-
  function(first = hold, iter) {
    out <-
      first %>%
      left_join(pair_insertion_rules, by = "polymer") %>%
      mutate(add = replace_na(str_c(
        str_sub(polymer, 1, 1), insert, str_sub(polymer, 2, 2)
      ), str_c(" ", polymer[1]))) %>% 
      separate_rows(add, sep = "") %>%
      mutate(add = str_c(lag(add, 1), add)) %>%
      filter(nchar(add) == 2) %>%
      group_by(add) %>%
      summarise(n = sum(n)) %>%
      select(polymer = add, n)
    return(out)
  }


reduce(1:40, .f = iterate_step, .init = first) %>% 
  separate_rows(polymer, sep = "") %>%
  filter(polymer != "") %>% 
  count(polymer, n) %>% 
  group_by(polymer) %>% 
  summarise(total = ceiling(sum(n*nn)/2)) %>% 
  ungroup() %>% 
  summarise(answer = max(total)-min(total))
  

