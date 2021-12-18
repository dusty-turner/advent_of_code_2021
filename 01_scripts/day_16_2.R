code <-
  tibble(lines = read_lines("02_data/day_16_temp.txt")) %>% 
  separate(col = lines, into = c("letter","bin"))

# rest <- tibble(letter = c("8A004A801A8002F478")) %>% 
rest <- tibble(letter = c("EE00D40C823060")) %>% 
  separate_rows(letter, sep = "") %>% filter(letter!="") %>% 
  left_join(code) %>% 
  summarise(string = str_c(bin,collapse = "")) %>% pull(string)

parse_literal_value <- function(string = rest){
  
  id_nums <- strsplit(string,"")[[1]][rep(c(T,F,F,F,F), nchar(string)/5)]
  remaining <- strsplit(string,"")[[1]][!rep(c(T,F,F,F,F), nchar(string)/5)]
  
  rle <- rle(id_nums)
  iter <- rle$lengths[rle$values==1] + 1
  iter <- ifelse(length(iter)==0,1,iter)
  
  binary_out <- NA
  for (i in 1:iter) {
    binary <- str_sub(string,2,5)
    string <- str_sub(string,6,-1)  
    binary_out <- append(binary_out,binary)
  }
  
  rest <- sub_str(rest,1+iter*5,-1)
  
  literal_value <- strtoi(str_c(replace_na(binary_out,""),collapse = ""), base = 2)
  out <- list(literal_value, rest = rest)
  return(literal_value)
}


as_int <- function(x){
  s <- as.numeric(str_split(x,"")[[1]])
  sum(s * 2 ^ seq(length(s) -1,0))
}

parse_length_id_1 <- function(rest_vec){
  num_sub_packets <- str_sub(rest_vec$rest,1,11) %>% as_int()
  rest <- str_sub(rest_vec$rest,12,-1)
  
  out <- list(num_sub_packets = num_sub_packets, rest = rest)
  
  return(out)
}

####

determine_version_and_id <- function(rest, iter = 1){
  first_3 <- str_pad(str_sub(rest,1,3),width = 4, side = "left", pad = 0)
  second_3 <- str_pad(str_sub(rest,4,6),width = 4, side = "left", pad = 0)
  packet_version <- code[code$bin==first_3,]$letter
  type_id <- code[code$bin==second_3,]$letter
  if(type_id==4){
    parse_literal_value(string = rest)
    length_type_id <- NULL
  } else{
    length_type_id <- str_sub(rest,7,7)
  }
  
  rest <- str_sub(rest, 8,-1)
  
  out <- list(version = packet_version, type_id = type_id, length_type_id = length_type_id, rest = rest)
  
  return(out)
  
}

rest_vec <- determine_version_and_id(rest = rest)


route_to <- function(rest_vec){
  if (rest_vec$length_type_id == 0) {
    parse_length_id_0(rest = rest_vec$rest)
  }
  if (rest_vec$length_type_id == 1) {
    parse_length_id_1(rest_vec = rest_vec)
  }
}



route_to_out <- route_to(rest_vec = rest_vec)


reduce(1:route_to_out$num_sub_packets, determine_version_and_id, .init = route_to_out$rest)


route_to(rest_vec = rest_vec)

parse_length_id_0 <- function(rest_vec){
  strtoi(str_sub(rest_vec$rest,1,15), base = 2)


}