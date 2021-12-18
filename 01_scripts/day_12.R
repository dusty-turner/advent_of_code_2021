library(tidyverse)

x <-
  tibble(paths = read_lines(file = "02_data/day_12.txt")) %>% 
  separate(col = paths, into = c("start", "end"), sep = "-") %>% 
  rename_with(toupper) %>% 
  mutate(start = if_else(END == "start" | START == "end", END, START)) %>% 
  mutate(end = if_else(start == END, START, END)) %>% 
  select(start,end) %>% 
  arrange(start) %>% 
  print(n = Inf)

upr_rev_paths <-
x %>% 
  filter(str_detect(start, '[:upper:]')|str_detect(end, '[:upper:]')) %>% 
  filter(end != "end" & start != "start") %>% 
  select(start = end, end = start)


remove_these <-
x %>% 
  pivot_longer(everything()) %>% 
  filter(!str_detect(value, '[:upper:]')) %>% count(value) %>% filter(n==1) %>% pull(value)

lwr_rev_paths <-
x %>% 
  filter(end != "end" & start != "start") %>% 
  filter(!str_detect(start, '[:upper:]')) %>% 
  filter(!str_detect(end, '[:upper:]')) %>% 
  select(start = end, end = start) %>% 
  filter(!start %in% remove_these | !end %in% remove_these)


y <- x %>% bind_rows(upr_rev_paths) %>% bind_rows(lwr_rev_paths) %>% distinct()

first <-
x %>% 
  filter(start == "start")

join_change_names <- function(x = out, iter_num = 1, part_2 = FALSE){
  print(iter_num)
  
  hold <- x %>% filter(end == "end")
  print(nrow(hold))
  progress <- x %>% filter(end != "end")
  print(nrow(progress))
  
  out_1 <-
  progress %>% 
  left_join(y, by = c("end" = "start")) %>% 
    rename_with(~str_c(.,"_x")) %>% 
    rename_with(~str_c("end"), .cols = last_col()) %>% 
    rename_with(~str_c("start"), .cols = starts_with("start")) %>% 
    unite(all, everything(), remove = F) 
    
  if(part_2==FALSE){
    out <-
    out_1 %>% 
      filter(str_count(all, "ws")<=1) %>%
      filter(str_count(all, "kq")<=1) %>%
      filter(str_count(all, "yr")<=1) %>%
      filter(str_count(all, "zo")<=1) %>%
      filter(str_count(all, "np")<=1) %>%
      filter(str_count(all, "xq")<=1) %>%
      filter(str_count(all, "ra")<=1) %>%
      select(-all) %>% 
      bind_rows(hold)
  }
  
  if(part_2==TRUE){
      out <-
      out_1 %>% 
    filter(str_count(all, "ws")<=2) %>%
    filter(str_count(all, "kq")<=2) %>%
    filter(str_count(all, "yr")<=2) %>%
    filter(str_count(all, "zo")<=2) %>%
    filter(str_count(all, "np")<=2) %>%
    filter(str_count(all, "xq")<=2) %>%
    filter(str_count(all, "ra")<=2) %>%
    mutate(ws = str_count(all, "ws")==2) %>%
    mutate(kq = str_count(all, "kq")==2) %>%
    mutate(yr = str_count(all, "yr")==2) %>%
    mutate(zo = str_count(all, "zo")==2) %>%
    mutate(np = str_count(all, "np")==2) %>%
    mutate(xq = str_count(all, "xq")==2) %>%
    mutate(ra = str_count(all, "ra")==2) %>%
    mutate(total = ws + kq + yr + zo + np + xq + ra) %>% 
    filter(total <= 1) %>% 
    select(-all,-ws,-kq,-yr,-zo,-np,-xq,-ra,-total) %>% 
    bind_rows(hold)
  }
  print(nrow(out))
  return(out)
}

1:20 %>% 
  reduce(.f = join_change_names, part_2 = TRUE, .init = first) 





join_change_names <- function(x = out, iter_num = 1){
  print(iter_num)
  
  hold <- x %>% filter(end == "end")
  
  progress <- x %>% filter(end != "end")
  # print(hold)
  # print(progress)
  
  out <-
  progress %>% 
  left_join(y, by = c("end" = "start")) %>% 
    rename_with(~str_c(.,"_x")) %>% 
    rename_with(~str_c("end"), .cols = last_col()) %>% 
    rename_with(~str_c("start"), .cols = starts_with("start")) %>% 
    unite(all, everything(), remove = F) %>% 
    filter(str_count(all, "ws")<=2) %>%
    filter(str_count(all, "kq")<=2) %>%
    filter(str_count(all, "yr")<=2) %>%
    filter(str_count(all, "zo")<=2) %>%
    filter(str_count(all, "np")<=2) %>%
    filter(str_count(all, "xq")<=2) %>%
    filter(str_count(all, "ra")<=2) %>%
    
    mutate(ws = str_count(all, "ws")==2) %>%
    mutate(kq = str_count(all, "kq")==2) %>%
    mutate(yr = str_count(all, "yr")==2) %>%
    mutate(zo = str_count(all, "zo")==2) %>%
    mutate(np = str_count(all, "np")==2) %>%
    mutate(xq = str_count(all, "xq")==2) %>%
    mutate(ra = str_count(all, "ra")==2) %>%
    
    # filter(str_count(all, "sl")<=1) %>%
    # filter(str_count(all, "pj")<=2) %>%
    # filter(str_count(all, "zg")<=2) %>%
    # filter(str_count(all, "fs")<=2) %>%
    # filter(str_count(all, "he")<=2) %>%
    # mutate(pj = str_count(all, "pj")==2) %>%
    # mutate(zg = str_count(all, "zg")==2) %>%
    # mutate(fs = str_count(all, "fs")==2) %>%
    # mutate(he = str_count(all, "he")==2) %>% 
    # rowwise() %>%
    # mutate(total = pj+zg+fs+he) %>% 
    mutate(total = ws + kq + yr + zo + np + xq + ra) %>% 
    # ungroup() %>%
    filter(total <= 1) %>% 
    select(-all,-ws,-kq,-yr,-zo,-np,-xq,-ra,-total) %>% 
    bind_rows(hold)
  print(nrow(out))
  return(out)
}





### from blog

x <- ## read in and clean data
  tibble(paths = read_lines(file = "02_data/day_12.txt")) %>% 
  separate(col = paths, into = c("start", "end"), sep = "-") %>% 
  rename_with(toupper) %>% 
  mutate(start = if_else(END == "start" | START == "end", END, START)) %>% 
  mutate(end = if_else(start == END, START, END)) %>% 
  select(start,end) 

upr_rev_paths <- x %>% ## large caves can go back and forth
  filter(str_detect(start, '[:upper:]')|str_detect(end, '[:upper:]')) %>%
  filter(end != "end" & start != "start") %>%
  select(start = end, end = start)

remove_these <- x %>% ## caves that are a dead end
  pivot_longer(everything()) %>% 
  filter(!str_detect(value, '[:upper:]')) %>% count(value) %>% filter(n==1) %>% pull(value)

lwr_rev_paths <- x %>% ## small caves reverse path
  filter(end != "end" & start != "start") %>% 
  filter(!str_detect(start, '[:upper:]')) %>% 
  filter(!str_detect(end, '[:upper:]')) %>% 
  select(start = end, end = start) %>% 
  filter(!start %in% remove_these | !end %in% remove_these)

y <- x %>% bind_rows(upr_rev_paths) %>% bind_rows(lwr_rev_paths) %>% distinct() ## all cave paths

first <- x %>% filter(start == "start") # from the start

join_change_names <- function(x = out, iter_num = 1, part_2 = FALSE){ 
  hold <- x %>% filter(end == "end") ## splits data to speed up code
  progress <- x %>% filter(end != "end") ## this is the only data needing processing
  out_1 <- progress %>% 
    left_join(y, by = c("end" = "start")) %>% 
    rename_with(~str_c(.,"_x")) %>% 
    rename_with(~str_c("end"), .cols = last_col()) %>% 
    rename_with(~str_c("start"), .cols = starts_with("start")) %>% 
    unite(all, everything(), remove = F) 
  if(part_2==FALSE){ ## part 1 criteria
    out <- out_1 %>% 
      filter(str_count(all, "ws")<=1) %>%
      filter(str_count(all, "kq")<=1) %>%
      filter(str_count(all, "yr")<=1) %>%
      filter(str_count(all, "zo")<=1) %>%
      filter(str_count(all, "np")<=1) %>%
      filter(str_count(all, "xq")<=1) %>%
      filter(str_count(all, "ra")<=1) %>%
      select(-all) %>% 
      bind_rows(hold)}
  if(part_2==TRUE){ ### part 2 criteria
    out <- out_1 %>% 
      filter(str_count(all, "ws")<=2) %>%
      filter(str_count(all, "kq")<=2) %>%
      filter(str_count(all, "yr")<=2) %>%
      filter(str_count(all, "zo")<=2) %>%
      filter(str_count(all, "np")<=2) %>%
      filter(str_count(all, "xq")<=2) %>%
      filter(str_count(all, "ra")<=2) %>%
      mutate(ws = str_count(all, "ws")==2) %>%
      mutate(kq = str_count(all, "kq")==2) %>%
      mutate(yr = str_count(all, "yr")==2) %>%
      mutate(zo = str_count(all, "zo")==2) %>%
      mutate(np = str_count(all, "np")==2) %>%
      mutate(xq = str_count(all, "xq")==2) %>%
      mutate(ra = str_count(all, "ra")==2) %>%
      mutate(total = ws + kq + yr + zo + np + xq + ra) %>% 
      filter(total <= 1) %>% 
      select(-all,-ws,-kq,-yr,-zo,-np,-xq,-ra,-total) %>% 
      bind_rows(hold)}
  return(out)}

1:20 %>% reduce(.f = join_change_names, .init = first) 

