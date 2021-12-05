library(tidyverse)

## 1
data <- readLines("02_data/day_4_temp.txt")
data <- read_lines("02_data/day_4.txt")

selections <-
  data[1] %>% str_split(",") %>% unlist() %>% str_pad(2,"left") %>% str_pad(3,"right")

boards <-
tibble(cards = data[-1]) %>% 
  filter(cards != "") %>%
  mutate(cards = str_squish(cards)) %>% 
  transmute(col = str_split_fixed(cards, pattern = " ", n = 5)) %>% 
  pull(col) %>% as_tibble() %>% 
  mutate(across(.cols = everything(),.fns = ~str_pad(str_pad(.,2,"left"),3,"right"))) %>% 
  mutate(card_number = sort(rep(1:(n()/5),5))) %>% 
  mutate(winner = 0)

boards_list <- list(boards = boards, win_card = integer(0))

mark_board <- function(boards_list = boards_list, selection = selections[1]){
  
  if(length(boards_list$win_card) > 0){stop(boards_list)}

  boards <- boards_list$boards
  
  marked <-
  boards %>% 
    mutate(across(.cols = 1:5, .fns = ~str_replace_all(string = ., pattern = selection, replacement = "X")))

  row_win_card <-
  marked %>%
    mutate(row_bingo = str_count(str_c(V1,V2,V3,V4,V5),"X")) %>% 
    filter(row_bingo==5)  %>% 
    pull(card_number)
  
  col_win_card <-
  marked %>% 
    mutate(across(.cols = V1:V5, .fns = ~ifelse(.=="X",1,0))) %>% 
    group_by(card_number) %>% 
    summarise(across(everything(), ~sum(.))) %>% 
    filter(V1 ==5|V2 ==5|V3 ==5|V4 ==5|V5 ==5) %>% 
    pull(card_number)
  
  card_number <- ifelse(length(row_win_card) > 0, row_win_card, ifelse(length(col_win_card) > 0, col_win_card, integer(0)))
  if(is.na(card_number)){card_number <- integer(0)}

  out <- list(boards = marked, win_card = card_number, selection = selection)
    
  return(out)
      
}

mark_board_poss <- possibly(mark_board, otherwise = NULL)

winning_card <- 
selections %>% 
  accumulate(.f = mark_board_poss, .init = boards_list) %>% 
  discard(is.null) %>% 
  pluck(length(.)) 

winning_card$boards %>% 
  filter(card_number == winning_card$win_card) %>% 
  select(-card_number) %>% 
  mutate(across(everything(), ~as.numeric(.))) %>% 
  as.matrix() %>% sum(na.rm = T) %>% prod(as.numeric(winning_card$selection))

## 2

boards_list <- list(boards = boards)

mark_board_2 <- function(boards_list = boards_list, selection = selections[41]){

  if(max(boards_list$boards$winner)==100){stop(boards_list)}
  
  boards <- boards_list$boards
  
  marked <-
    boards %>% 
    mutate(across(.cols = 1:5, .fns = ~str_replace_all(string = ., pattern = selection, replacement = "X")))
  
  row_win_card <-
    marked %>%
    mutate(row_bingo = str_count(str_c(V1,V2,V3,V4,V5),"X")) %>% 
    filter(row_bingo==5)  %>% 
    pull(card_number)
  
  col_win_card <-
    marked %>% 
    mutate(across(.cols = V1:V5, .fns = ~ifelse(.=="X",1,0))) %>% 
    group_by(card_number) %>% 
    summarise(across(everything(), ~sum(.))) %>% 
    filter(V1 ==5|V2 ==5|V3 ==5|V4 ==5|V5 ==5) %>% 
    pull(card_number)
  
  if(length(row_win_card)>0 & nrow(marked) == 5){
    marked <- 
      marked %>% 
      mutate(winner = 100)
    return(out <- list(boards = marked, selection = selection))
    } else if(length(row_win_card)>0){
    marked <- 
    marked %>% 
      filter(!card_number %in% row_win_card)
  }
  if(length(col_win_card)>0 & nrow(marked) == 5){
    marked <- 
    marked %>% 
      mutate(winner = 100)
    return(out <- list(boards = marked, selection = selection))
  } else if(length(col_win_card)>0){
    marked <- 
    marked %>% 
      filter(!card_number %in% col_win_card)
  }
  
  out <- list(boards = marked, selection = selection)
  
  return(out)
  
}

mark_board_2_poss <- possibly(mark_board_2, otherwise = NULL)

winning_card <-
selections %>% 
  accumulate(mark_board_2_poss, .init = boards_list) %>% 
  discard(is.null) %>% 
  pluck(length(.))

winning_card$boards %>% 
  select(V1:V5) %>% 
  mutate(across(everything(), ~as.numeric(.))) %>% 
  as.matrix() %>% sum(na.rm = T) %>% prod(as.numeric(winning_card$selection))

