library(tidyverse)
library(igraph)

file_path <- "02_data/day_15.txt"

width <- read_lines(file = file_path)[1] %>% nchar()

l <-  matrix(1:(width * width), byrow = T, ncol = width)
w <- tibble(weights = read_lines(file = file_path)) %>%
  mutate(id = row_number()) %>%
  separate_rows(weights, sep = "", convert = T) %>%
  filter(str_detect(weights, "")) %>%
  pull(weights) %>% matrix(nrow = width, byrow = T)

my_graph <-
  tibble(x = map(.x = 0:(nrow(l) - 1), ~ l[1, ][-c(nrow(l))] + (.x * nrow(l))) %>% unlist(),
         y = map(.x = 0:(nrow(l) - 1), ~ l[1, ][-1] + (.x * nrow(l))) %>% unlist()) %>%
  mutate(weight = as.vector(t(w[, -1]))) %>%
  bind_rows(
    tibble(x = map(.x = 0:(ncol(l) - 1), ~ l[1, ][-1] + (.x * ncol(l))) %>% unlist(),
           y = map(.x = 0:(ncol(l) - 1), ~ l[1, ][-c(ncol(l))] + (.x * ncol(l))) %>% unlist()
           ) %>% 
      mutate(weight = as.vector(t(w[, -nrow(w)])))) %>%
  bind_rows(
    tibble(x = as.vector(l[-c(nrow(l)), ]),
           y = as.vector(l[-1, ])
           ) %>%
      mutate(weight = as.vector(w[-1, ]))) %>%
  bind_rows(
    tibble(x = as.vector(l[-1, ]),
           y = as.vector(l[-c(nrow(l)), ])) %>%
      mutate(weight = as.vector(w[-nrow(w), ]))
    ) 

g2 <- add_edges(make_empty_graph(n = width * width),
                as.matrix(t(my_graph[1:nrow(my_graph), 1:2])),
                weight = my_graph[, 3]$weight
                )
ans <- igraph::shortest_paths(g2, from = 1, to = width * width)

tibble(l = l %>% as.vector(),
       w = w %>% as.vector()) %>%
  filter(l %in% ans$vpath[[1]]) %>%
  summarise(sum = sum(w)) - w[1, 1]

## part 2

create_mat <- 
  function(mat, iter) {if_else(mat + 1 > 9, 1, mat + 1) %>%
    matrix(nrow = width, byrow  = F)}

mats <- 1:9 %>% accumulate(.f = create_mat, .init = w) %>% tail(9)

w <- rbind(
  cbind(w, mats[[1]], mats[[2]], mats[[3]], mats[[4]]),
  cbind(mats[[1]], mats[[2]], mats[[3]], mats[[4]], mats[[5]]),
  cbind(mats[[2]], mats[[3]], mats[[4]], mats[[5]], mats[[6]]),
  cbind(mats[[3]], mats[[4]], mats[[5]], mats[[6]], mats[[7]]),
  cbind(mats[[4]], mats[[5]], mats[[6]], mats[[7]], mats[[8]]
  )
)

l <-  matrix(1:(width * 5 * width * 5), byrow = T, ncol = width * 5)

my_graph <-
  tibble(x = map(.x = 0:(nrow(l) - 1), ~ l[1, ][-c(nrow(l))] + (.x * nrow(l))) %>% unlist(),
         y = map(.x = 0:(nrow(l) - 1), ~ l[1, ][-1] + (.x * nrow(l))) %>% unlist()
         ) %>%
  mutate(weight = as.vector(t(w[, -1]))) %>%
  bind_rows(
    tibble(x = map(.x = 0:(ncol(l) - 1), ~ l[1, ][-1] + (.x * ncol(l))) %>% unlist(),
           y = map(.x = 0:(ncol(l) - 1), ~ l[1, ][-c(ncol(l))] + (.x * ncol(l))) %>% unlist()
           ) %>%
      mutate(weight = as.vector(t(w[, -nrow(w)])))) %>%
  bind_rows(
    tibble(x = as.vector(l[-c(nrow(l)), ]),
           y = as.vector(l[-1, ])
           ) %>%
      mutate(weight = as.vector(w[-1, ]))) %>%
  bind_rows(
    tibble(x = as.vector(l[-1, ]),
           y = as.vector(l[-c(nrow(l)), ])
           ) %>%
      mutate(weight = as.vector(w[-nrow(w), ]))
    )

g2 <-  add_edges(make_empty_graph(n = width * 5 * width * 5),
                 as.matrix(t(my_graph[1:nrow(my_graph), 1:2])),
                 weight = my_graph[, 3]$weight
                 )
ans <- igraph::shortest_paths(g2, from = 1, to = width * 5 * width * 5)

tibble(l = l %>% as.vector(),
       w = w %>% as.vector()) %>%
  filter(l %in% ans$vpath[[1]]) %>%
  summarise(sum = sum(w)) - w[1, 1]
