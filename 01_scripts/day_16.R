library(tidyverse)

code <-
tibble(lines = read_lines("02_data/day_16_temp.txt")) %>% 
  separate(col = lines, into = c("letter","bin"))


# string <- tibble(letter = c("6051639005B56008C1D9BB3CC9DAD5BE97A4A9104700AE76E672DC95AAE91425EF6AD8BA5591C00F92073004AC0171007E0BC248BE0008645982B1CA680A7A0CC60096802723C94C265E5B9699E7E94D6070C016958F99AC015100760B45884600087C6E88B091C014959C83E740440209FC89C2896A50765A59CE299F3640D300827902547661964D2239180393AF92A8B28F4401BCC8ED52C01591D7E9D2591D7E9D273005A5D127C99802C095B044D5A19A73DC0E9C553004F000DE953588129E372008F2C0169FDB44FA6C9219803E00085C378891F00010E8FF1AE398803D1BE25C743005A6477801F59CC4FA1F3989F420C0149ED9CF006A000084C5386D1F4401F87310E313804D33B4095AFBED32ABF2CA28007DC9D3D713300524BCA940097CA8A4AF9F4C00F9B6D00088654867A7BC8BCA4829402F9D6895B2E4DF7E373189D9BE6BF86B200B7E3C68021331CD4AE6639A974232008E663C3FE00A4E0949124ED69087A848002749002151561F45B3007218C7A8FE600FC228D50B8C01097EEDD7001CF9DE5C0E62DEB089805330ED30CD3C0D3A3F367A40147E8023221F221531C9681100C717002100B36002A19809D15003900892601F950073630024805F400150D400A70028C00F5002C00252600698400A700326C0E44590039687B313BF669F35C9EF974396EF0A647533F2011B340151007637C46860200D43085712A7E4FE60086003E5234B5A56129C91FC93F1802F12EC01292BD754BCED27B92BD754BCED27B100264C4C40109D578CA600AC9AB5802B238E67495391D5CFC402E8B325C1E86F266F250B77ECC600BE006EE00085C7E8DF044001088E31420BCB08A003A72BF87D7A36C994CE76545030047801539F649BF4DEA52CBCA00B4EF3DE9B9CFEE379F14608")) %>%
string <- tibble(letter = read_lines("02_data/day_16.txt")) %>%
  separate_rows(letter, sep = "") %>% filter(letter!="") %>% 
  left_join(code) %>% 
  summarise(string = str_c(bin,collapse = "")) %>% pull(string)
## type 1
rest <- tibble(letter = c("D2FE28")) %>% 
  separate_rows(letter, sep = "") %>% filter(letter!="") %>% 
  left_join(code) %>% 
  summarise(string = str_c(bin,collapse = "")) %>% pull(string)
##type 2
rest <- tibble(letter = c("38006F45291200")) %>% 
  separate_rows(letter, sep = "") %>% filter(letter!="") %>% 
  left_join(code) %>% 
  summarise(string = str_c(bin,collapse = "")) %>% pull(string)
## type 2
string <- tibble(letter = c("EE00D40C823060")) %>% 
  separate_rows(letter, sep = "") %>% filter(letter!="") %>% 
  left_join(code) %>% 
  summarise(string = str_c(bin,collapse = "")) %>% pull(string)

rest <- tibble(letter = c("8A004A801A8002F478")) %>% 
  separate_rows(letter, sep = "") %>% filter(letter!="") %>% 
  left_join(code) %>% 
  summarise(string = str_c(bin,collapse = "")) %>% pull(string)




first_3 <- str_pad(str_sub(rest,1,3),width = 4, side = "left", pad = 0)
rest <- str_sub(rest,4,-1)
second_3 <- str_pad(str_sub(rest,1,3),width = 4, side = "left", pad = 0)
rest <- str_sub(rest,4,-1)

## packet version
packet_version <- code[code$bin==first_3,]$letter
print(str_c("Package Version ", packet_version))
## type id
type_id <- code[code$bin==second_3,]$letter
print(str_c("type id ", type_id))
# parse_literal_value(string = rest)

if(type_id==4){parse_literal_value(string = rest)} 

if(type_id!=4){
  length_type_id <- str_split(rest,"")[[1]][1]
  
  if(length_type_id=="0"){
    strtoi(str_sub(rest,2,16), base = 2)
    
    first_3 <- str_pad(str_sub(rest,17,19),width = 4, side = "left", pad = 0)
    packet_version <- code[code$bin==first_3,]$letter
    type <- str_pad(str_sub(rest,20,22),width = 4, side = "left", pad = 0)
    type_id <- code[code$bin==type,]$letter
    print(packet_version)
    parse_literal_value(str_sub(rest,23,27))
    first_3 <- str_pad(str_sub(rest,28,30),width = 4, side = "left", pad = 0)
    packet_version <- code[code$bin==first_3,]$letter
    type <- str_pad(str_sub(rest,31,33),width = 4, side = "left", pad = 0)
    type_id <- code[code$bin==type,]$letter
    print(packet_version)
    parse_literal_value(str_sub(rest,34,43))
    
  } else if(length_type_id=="1"){
    num_sub_packets <- as_int(x = str_sub(rest,2,12))
    # num_sub_packets <- strtoi(str_sub(rest,2,12), base = 2)
    
    1:num_sub_packets %>% map_dbl(.f = ~parse_length_id_1(rest = rest, iter = .x))
    
    # rest <-
    # rest <- str_sub(rest,24+(num_sub_packets-1)*11,-1)
    
    rest <- str_remove(rest, "^0+")
    
  } else {message("broke")}
  
  
}

#######
  parse_literal_value <- function(string){
    
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
    
    literal_value <- strtoi(str_c(replace_na(binary_out,""),collapse = ""), base = 2)
    return(literal_value)
  }
  
  parse_length_id_1 <- function(rest, iter = 1){
    add <- ((iter-1)*11)
    first_3 <- str_pad(str_sub(rest,13+add,15+add),width = 4, side = "left", pad = 0)
    packet_version <- code[code$bin==first_3,]$letter
    type <- str_pad(str_sub(rest,16+add,18+add),width = 4, side = "left", pad = 0)
    type_id <- code[code$bin==type,]$letter
    print(str_c("packet version ", packet_version))
    print(str_c("type  ", type_id))
    
    rest <- rest %>% str_sub(19,-1)
    # literal_value <- parse_literal_value(string = str_sub(rest,19+add,23+add))
    return(rest)
  }
  
  
as_int <- function(x){
  s <- as.numeric(str_split(x,"")[[1]])
  sum(s * 2 ^ seq(length(s) -1,0))
}
