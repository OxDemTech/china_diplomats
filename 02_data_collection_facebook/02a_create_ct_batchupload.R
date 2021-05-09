# This file takes the manually created seed source list and translates it into
# a csv that can be batch-uploaded to CT to populate a list of pages. 

library(tidyverse)

facebooktargets <- readxl::read_xlsx("data/input_selectors/ap_targets_final.xlsx",sheet = 3)

statemedia <- 
  facebooktargets %>% 
  filter(type %in% c("M","NewM")) %>%
  pull(handle) %>% 
  .[!is.na(.)] %>% 
  sub("\\?.*$","",.) %>%
  sub("/$","",.) %>%
  data.frame(`Page or Account URL` = .) %>%
  mutate(List = "China_StateMedia")

write_csv(statemedia,file = "data/ct_lists/batchupload_statemedia_v2.csv")

diplomats <-
  facebooktargets %>% 
  filter(type %in% c("A","E","C","E","B")) %>%
  pull(handle) %>% 
  .[!is.na(.)] %>% 
  sub("\\?.*$","",.) %>%
  sub("/$","",.) %>%
  data.frame(`Page or Account URL` = .)%>%
  mutate(List = "China_Diplomats")

write_csv(diplomats,file = "data/ct_lists/batchupload_diplomats_v2.csv")
