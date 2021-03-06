# This file takes the manually created seed source list and translates it into
# a csv that can be batch-uploaded to CT to populate a list of pages. 

library(tidyverse)

facebooktargets <- readxl::read_xlsx("PATH TO SELECTOR FILE/ap_targets_final.xlsx",sheet = 3)

statemedia <- 
  facebooktargets %>% 
  filter(type %in% c("M","NewM")) %>%
  pull(handle) %>% 
  .[!is.na(.)] %>% 
  sub("\\?.*$","",.) %>%
  sub("/$","",.) %>%
  data.frame(`Page or Account URL` = .) %>%
  mutate(List = "China_StateMedia")

write_csv(statemedia,file = "PATH TO SELECTOR DIR WITH CT LISTS/batchupload_statemedia_v2.csv")

diplomats <-
  facebooktargets %>% 
  filter(type %in% c("A","E","C","E","B")) %>%
  pull(handle) %>% 
  .[!is.na(.)] %>% 
  sub("\\?.*$","",.) %>%
  sub("/$","",.) %>%
  data.frame(`Page or Account URL` = .)%>%
  mutate(List = "China_Diplomats")

write_csv(diplomats,file = "PATH TO SELECTOR DIR WITH CT LISTS/batchupload_diplomats_v2.csv")
