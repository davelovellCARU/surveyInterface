
thisTib <- tibble(location = dir() %>% {.[str_detect(.,"\\.rdat$")]}) %>% 
  mutate(file = map(location, readRDS)) %>% 
  pull(file) %>% 
  reduce(rbind)
  