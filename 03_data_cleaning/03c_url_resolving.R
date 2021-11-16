library(tidyverse)

urls <- 
  bind_rows(
    read_csv("data/vault/url_list_scrapingversion.csv",
             col_types = cols(.default = col_character())),
    read_csv("data/vault/new_urls_feb21_fixedurls.csv",
             col_types = cols(.default = col_character()))
  )

targets <- readxl::read_xlsx("data/input_selectors/ap_targets_final.xlsx",sheet = 1)
urlreg <- 
  targets$url %>%  #url statt lang_identifier
  .[. != ""] %>% 
  .[!is.na(.)] %>% unique() %>%
  paste0("",.,"", collapse = "|") %>% 
  str_replace_all("\\.","\\\\.")

resolved <- 
  bind_rows(
    read_csv("data/vault/resolved.csv",
             col_types = cols(.default = col_character())),
    read_csv("data/vault/output_resolved_new.csv",
             col_types = cols(.default = col_character()))
  )

resolved$detected_outlet <- 
  resolved %>% pull(resolved) %>% 
  paste0(" ",.,"?") %>%
  str_extract(.,pattern = "^(.+?)\\?") %>% 
  str_remove("\\?") %>%
  str_extract(.,pattern =  paste0('(?<=https://|http://|https://www.|http://www.|\\s)(.*)(',urlreg,')')) 

resolved$detected_resolved_domain <- 
  resolved$detected_outlet %>%
  str_extract(.,pattern =  paste0('(',urlreg,')')) 

resolved <- 
  resolved %>% 
  left_join(targets %>% select(url,resolved_outlet = outlet),
            by = c("detected_resolved_domain" = "url")) %>% 
  distinct(url,.keep_all = T) %>% 
  rename(url_id = id) %>%
  left_join(urls %>% select(url_id,final_url,detected_url),
            by = "url_id")

saveRDS(resolved,"data/nogit/processed/resolved_feb.rds")
