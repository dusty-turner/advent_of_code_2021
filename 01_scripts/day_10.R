library(tidyverse)

##1
input <-
  tibble(chunks = read_lines("02_data/day_10.txt"))

syntax_error_points <- ## points for each bracket type
  tibble(chunks = c(")", "]", "}", ">"),
         points = c(3, 57, 1197, 25137)
         )

input_list <- list(NULL, input) ## prepare list for iteration

i <- 2 ## initialize iterator 

while (!isTRUE(all.equal(input_list[i], input_list[i - 1]))) {
  input <-
    input %>%
    mutate(chunks = str_remove_all(string = chunks, pattern = "<>|\\[\\]|\\(\\)|\\{\\}"))
  i = i + 1
  input_list[[i]] <- input
} ## this iteratively removes all open/closed pairings until the df does not change between iterations

input_list %>% tail(1) %>% pluck(1) %>% ## select the last list
  mutate(chunks = str_remove_all(string = chunks, pattern = "<|\\[|\\(|\\{")) %>% ## remove openings
  filter(chunks != "") %>% ## remove extraneous lines
  mutate(chunks = str_sub(chunks, 1, 1)) %>% ## select the first opening
  count(chunks) %>% ## count them up!
  left_join(syntax_error_points, by = "chunks") %>%  ## join in points
  summarise(score = sum(n * points)) ## score the process

##2

incomplete_df <- ## identify chunks with incomplete strings
  input_list %>% tail(1) %>% pluck(1) %>% ## selects list element with all open/closed pairs removed
  mutate(chunks = str_remove_all(string = chunks, pattern = "<|\\[|\\(|\\{")) %>% ## removes opens
  mutate(incomplete_strings = row_number()) %>% ## ids each pattern
  filter(chunks == "") %>% ## removes extraneous lines
  select(id = chunks, incomplete_strings) ## now have df with incomplete strings identified


matching_df <- ## create scoring df for joining later
  tibble(
    chunks = c("(", "[", "{", "<"),
    right = c(")", "]", "}", ">"),
    points = c(1, 2, 3, 4)
  )

incomplete_list <-  ## creates list of points
  input %>%  mutate(incomplete_strings = row_number()) %>%
  semi_join(incomplete_df, by = "incomplete_strings") %>% ## only keep the chunks that are incomplete
  separate_rows(chunks, sep = "") %>% 
  left_join(matching_df, by = "chunks") %>% ## add in points
  filter(!is.na(right)) %>%
  group_by(incomplete_strings) %>% mutate(row_num = row_number()) %>% arrange(desc(row_num)) %>%
  select(-row_num) %>% ## puts the chunks in the right order
  group_split() %>% ## splits them for future map function
  map(.f = ~ pull(.data = .x, points)) ## only keeps the column we want

score_points <- function(current_vector = points, index) {
    current_vector[index] <- current_vector[index - 1] * 5 + current_vector[index]
    return(current_vector)
  } ## function to execute partial scoring function

score_everything <- function(data) {
  2:length(data) %>%
    reduce(.f = score_points, .init = data) %>%
    tail(1)
} ## function which scores entire vector

incomplete_list %>%
  map_dbl(.f = ~ score_everything(data = .x)) %>%
  sort() %>% median() ## maps all lists over scoring function
