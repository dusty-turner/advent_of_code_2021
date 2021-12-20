library(tidyverse)

code <-
  tibble(lines = read_lines("02_data/day_16_code.txt")) %>% 
  separate(col = lines, into = c("letter","bin"))

# rest <- tibble(letter = c("8A004A801A8002F478")) %>%
# rest <- tibble(letter = c("EE00D40C823060")) %>%
# rest <- tibble(letter = c("620080001611562C8802118E34")) %>%
# rest <- tibble(letter = c("C0015000016115A2E0802F182340")) %>%
# rest <- tibble(letter = c("A0016C880162017C3686B18A3D4780")) %>%
# rest <- tibble(letter = c("EE00D40C823060")) %>%
rest <-
tibble(letter = read_lines("02_data/day_16.txt")) %>%
  separate_rows(letter, sep = "") %>% filter(letter!="") %>% 
  left_join(code) %>% 
  summarise(string = str_c(bin,collapse = "")) %>% pull(string)


parse <- function(rest = rest, LV = LV, PV = PV, TID = TID){
  if(nchar(rest) < 6){
    out <- list(LV = LV, PV = PV, TID = TID)
    return(out)
  }
  first_3 <- str_pad(str_sub(rest,1,3),width = 4, side = "left", pad = 0)
  second_3 <- str_pad(str_sub(rest,4,6),width = 4, side = "left", pad = 0)
  packet_version <- code[code$bin==first_3,]$letter
  type_id <- code[code$bin==second_3,]$letter
  rest <- str_sub(rest, 7,-1)
  PV <- append(PV, packet_version)  
  TID <- append(TID, type_id)
  if(type_id!=4){
    route <- str_sub(rest,1,1)
    rest <- str_sub(rest,2,-1)
    if(route ==1){
      num_iter <- as_int(str_sub(rest,1,11))
      rest <- str_sub(rest,12,-1)
      for (i in 1:num_iter) {
        return(parse(rest = rest, LV = LV, PV = PV, TID = TID))
      }
    } else if(route==0){
      lenths_string <- as_int(str_sub(rest,1,15))
      rest <- str_sub(rest,16,-1)
      return(parse(rest = rest, LV = LV, PV = PV, TID = TID))
    }
  } else if(type_id==4){
    id_nums <- strsplit(rest,"")[[1]][rep(c(T,F,F,F,F), nchar(rest)/5)]
    num_iter <- which(as.numeric(id_nums)==0)[1]
    temp_list <- vector()
    for (i in 1:num_iter) {
      temp <- str_sub(rest, 2, 5)
      rest <- str_sub(rest, 6, -1)
      temp_list <- str_c(temp_list,temp)
    }
    literal_value <- as_int(temp_list)
    LV <- append(LV, literal_value)
    return(parse(rest = rest, LV = LV, PV = PV , TID = TID))
  }
  out <- list(LV = LV, PV = PV, TID = TID)
  return(out)
}

final <- parse(rest = rest, LV = NULL, PV = NULL, TID = NULL)

final$PV %>% as.integer() %>% sum()


a <- list(test = "testa")

list(a, testb = "test")


as_int <- function(x){
  s <- as.numeric(str_split(x,"")[[1]])
  sum(s * 2 ^ seq(length(s) -1,0))
}



#####


parse <- function(rest = rest, LV = LV, PV = PV){
  if(nchar(rest) < 6){
    out <- list(LV = LV, PV = PV)
    return(out)
  }
  first_3 <- str_pad(str_sub(rest,1,3),width = 4, side = "left", pad = 0)
  second_3 <- str_pad(str_sub(rest,4,6),width = 4, side = "left", pad = 0)
  packet_version <- code[code$bin==first_3,]$letter
  type_id <- code[code$bin==second_3,]$letter
  print(str_c("pv: ", packet_version, " type id: ", type_id))
  rest <- str_sub(rest, 7,-1)
  PV <- append(PV, packet_version)

  if(type_id!=4){
    route <- str_sub(rest,1,1)
    rest <- str_sub(rest,2,-1)
    if(route ==1){
      num_iter <- as_int(str_sub(rest,1,11))
      rest <- str_sub(rest,12,-1)
      # while (num_iter>0) {
      for (i in 1:num_iter) {
        return(parse(rest = rest, LV = LV, PV = PV))
      }
      # }
    } else if(route==0){
      lenths_string <- as_int(str_sub(rest,1,15))
      rest <- str_sub(rest,16,-1)
      return(parse(rest = rest, LV = LV, PV = PV))
    }
    
  } else if(type_id==4){
    id_nums <- strsplit(rest,"")[[1]][rep(c(T,F,F,F,F), nchar(rest)/5)]
    num_iter <- which(as.numeric(id_nums)==0)[1]
    temp_list <- vector()
    for (i in 1:num_iter) {
      temp <- str_sub(rest, 2, 5)
      rest <- str_sub(rest, 6, -1)
      temp_list <- str_c(temp_list,temp)
    }
    literal_value <- as_int(temp_list)
    print(str_c("listeral value ", literal_value))
    LV <- append(LV, literal_value)
    return(parse(rest = rest, LV = LV, PV = PV))
  }
  out <- list(LV = LV, PV = PV)
  return(out)
  
}



recursive_add <- function(x, res_list=NULL) {
  if (is.null(res_list)) {
    res_list <- list()
  }
  if (x == 26) {
    return(res_list)
  }
  res_list[[x]] <- letters[x]
  res_list <- recursive_add(x + 1, res_list)
  return(res_list)
}
recursive_add(1)
