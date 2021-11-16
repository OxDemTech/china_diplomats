zero2NA_char <- 
  function(x){
    ifelse(x == 0 | x == "0", NA,as.character(x))
  }

china_urls <- 
  read_csv("data/input_selectors/china-url-targets.csv") %>% 
  filter(is.na(exclude)) %>% 
  filter(!is.na(url)) %>% 
  pull(url) %>%
  paste0(.,collapse = "|")

media_targets <- 
  read_csv("data/input_selectors/china-targets.csv") %>% 
  rename(which = `A/E/C/S`) %>% 
  filter(which == "M") %>% 
  pull(twitter_handle)

media_matches <- 
  read_csv("data/input_selectors/china-targets.csv") %>% 
  rename(which = `A/E/C/S`) %>% 
  filter(which == "M") %>% 
  select(outlet = country,twitter_handle,which)

diplomat_matches <- 
  read_csv("data/input_selectors/china-targets.csv") %>% 
  rename(which = `A/E/C/S`) %>% 
  filter(which %in% c("A","E","C","S","B","O")) %>% 
  select(ISO2,outlet = country,twitter_handle,which)

diplomat_targets <- 
  diplomat_matches %>% pull(twitter_handle)


small <- function(x){
  paste0("{\\small ",x,"}")
}

tiny <- function(x){
  paste0("{\\tiny ",x,"}")
}

nl <- function(x){
  paste0(x,collapse = " \\newline ")
}

library(unicode)
mandarin_to_latex <- function(x){
  if(!is.na(x)){
    paste0('\\mandarin{"',str_sub(Unicode::as.u_char(utf8ToInt(x)),3,-1L),'}') %>%
      purrr::map_chr(~ gsub('\"', '', .x, fixed = TRUE))%>% 
      paste0(collapse = "") %>% 
      tiny()
  }else{NA}
  
}