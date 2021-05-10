## Pull CT information on all pages included in the list previously created by 
## batch upload

# install.packages(c("httr", "jsonlite"))

######### CT API Documentation
# CT API Cheatsheet: https://help.crowdtangle.com/en/articles/3443476-api-cheat-sheet
# CT API Wiki:       https://github.com/CrowdTangle/API/wiki

library(tidyverse)
library(httr)
library(jsonlite)

api_token = read.table("PATH TO CT TOKEN.txt")$V1
list_id_diplomats = read.table("PATH TO CT LIST ID")$V1
list_id_media = read.table("PATH TO CT LIST ID")$V1

out <- purrr::map2(
  .x = c(list_id_diplomats,list_id_media),
  .y = c("diplomats","statemedia"),
  .f = function(list_id,listlabel){
    
    Sys.sleep(5)
    
    list_endpoint = paste0("https://api.crowdtangle.com/lists/",list_id,"/accounts")
    
    params <- 
      list('token' = api_token,
           'count' = 100,
           'offset' = 0)
    
    res = GET(url = list_endpoint,
              query = params)
    
    res_load <- content(res)
    
    print(paste0("Request Status: ",res_load$status))
    
    rescontent <- res_load$result
    
    df <- 
      bind_rows(rescontent$accounts) %>% 
      mutate(listlabel = listlabel)
    
    while(!is.null(res_load$result$pagination$nextPage)){
      offset = 100
      params <- 
        list('token' = api_token,
             'count' = 100,
             'offset' = offset)
      
      res = GET(url = list_endpoint,
                query = params)
      
      res_load <- content(res)
      
      print(paste0("Request Status: ",res_load$status))
      
      rescontent <- res_load$result
      
      df2 <- 
        bind_rows(rescontent$accounts) %>% 
        mutate(listlabel = listlabel)
      
      df <- bind_rows(df,df2)
      
      offset = offset + 100
    }
    
    return(df)
  }
) %>% bind_rows()

fb_targets <- readxl::read_xlsx("PATH TO SELECTOR FILE/ap_targets_final.xlsx",sheet = 3) %>%
  rename(URL = handle)%>% 
  mutate(id = as.character(id)) 

fb_targets$handle <- 
  fb_targets$URL %>% paste0(.," ") %>%
  str_extract("(?<=https:\\/\\/www.facebook.com\\/).*?(?=/| |-[0-9])")

ct_pgdata <- 
  bind_rows(
    out %>%
      filter(is.na(handle)| handle == "")%>% 
      mutate(id = as.character(id)) %>%
      left_join(fb_targets %>% rename(handle_backup = handle,url_match = URL),
                by = c("id" = "id")) %>%
      mutate(handle = ifelse(is.na(handle)|handle == "",handle_backup,handle)) %>% 
      select(-handle_backup) ,
    out %>%
      filter((!is.na(handle)) & (handle != ""))%>%
      left_join(fb_targets %>% rename(url_match = URL) %>% select(-id),
                by = c("handle" = "handle"))  %>%
      mutate(id = as.character(id)) 
  ) %>% 
  select(-contains(".."))

write_csv(ct_pgdata,file = "PATH TO DIR WIH CT LIST IDS/returned_ct_page_ids_final2.csv")

