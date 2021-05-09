library(tidyverse)
library(httr)
library(jsonlite)
library(rtangle)
library(lubridate)

search_endpoint = "https://api.crowdtangle.com/posts/search"

ct_pgdata <- 
  read_csv(file = "data/ct_lists/returned_ct_page_ids_final2.csv",col_types = cols(.default = col_guess(),
                                                                     id = col_character(),
                                                                     platformId = col_character()))

api_token = read.table("PATH TO CT TOKEN.txt")$V1


  
  
  
## parse outlet

already_done <- 
  list.files("data/nogit/ct_postdata_raw/") %>% 
  str_extract("^(.+?)_") %>% 
  str_remove_all("_")

subdata <- 
  ct_pgdata %>%
  filter(!handle %in% already_done) %>%
  distinct(platformId,.keep_all = T)

print(lubridate::now())

purrr::map2(.x = subdata$platformId,
            .y = subdata$handle,
            .f = 
              function(x,y){
                print(which(x == subdata$platformId))
                print(y)
                
                postlist <-
                  rtangle:::posts(token = api_token,
                                  accounts = x, #CNN example
                                  searchTerm = "",
                                  startDate = "2020-06-09T00:00:00",
                                  endDate = "2021-02-23T23:59:59",
                                  count = 10000,
                                  sortBy = 'date',
                                  search10k = T,
                                  boolean_allowed = T,
                                  output_raw = TRUE)
                

                saveRDS(postlist,paste0("data/nogit/ct_postdata_raw/",
                                        y,"_",x,
                                        ".rds"))
                
                Sys.sleep(10)
              }
)
print(lubridate::now())



