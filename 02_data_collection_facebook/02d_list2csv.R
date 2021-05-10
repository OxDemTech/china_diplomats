library(tidyverse)

########################

path1a = "PATH/ct_linkdata_raw/"
path1b = "PATH/ct_postdata_raw/"

all_files <- 
  c(paste0(path1a,list.files(path = path1a)),
    paste0(path1b,list.files(path = path1b))
   ) #%>% rev()

wherefiles <- 
  all_files %>% 
  str_extract(pattern = '(?<=nogit/).*?(?=_raw)') %>% 
  paste0(.,"_parsed")

system.time({
  flatt <- purrr::map2(.x = all_files,
                      .y = wherefiles,
                      .f = function(f,w){
                       file <- readRDS(f)
                       print(lubridate::now())
                       print(which(f == all_files))
                       flat <- purrr::map(.x = file,
                                          .f = function(inner){
                                            
                                            inner["n_urls"] <- 
                                              map(.x = inner["expandedLinks"],
                                                  .f = ~ map(.x = .x,
                                                             .f = ~ .x[[1]]))[[1]] %>% length()
                                            
                                            inner["original_urls"] <- 
                                              map(.x = inner["expandedLinks"],
                                                  .f = ~ map(.x = .x,
                                                             .f = ~ .x[[1]])
                                              )[[1]] %>% paste0(.,collapse = " ;;||;; ")%>% paste0(.," ")
                                            
                                            inner["expanded_urls"] <- 
                                              map(.x = inner["media"],
                                                  .f = ~ map(.x = .x,
                                                             .f = ~ .x[[2]])
                                              )[[1]] %>% paste0(.,collapse = " ;;||;; ") %>% paste0(.," ")
                                            
                                            inner["expandedLinks"] <- NULL
                                            
                                            
                                            inner["n_media"] <- 
                                              map(.x = inner["media"],
                                                  .f = ~ map(.x = .x,
                                                             .f = ~ .x[[1]]))[[1]] %>% length()
                                            
                                            
                                            inner["media_type"] <- 
                                              map(.x = inner["media"],
                                                  .f = ~ map(.x = .x,
                                                             .f = ~ .x[[1]])
                                              )[[1]] %>% paste0(.,collapse = " ;;||;; ")
                                            
                                            inner["media_url"] <- 
                                              map(.x = inner["media"],
                                                  .f = ~ map(.x = .x,
                                                             .f = ~ .x[[2]])
                                              )[[1]] %>% paste0(.,collapse = " ;;||;; ")
                                            
                                            inner["media_height"] <- 
                                              map(.x = inner["media"],
                                                  .f = ~ map(.x = .x,
                                                             .f = ~ .x[[3]])
                                              )[[1]] %>% paste0(.,collapse = " ;;||;; ")
                                            
                                            inner["media_width"] <- 
                                              map(.x = inner["media"],
                                                  .f = ~ map(.x = .x,
                                                             .f = ~ .x[[4]])
                                              )[[1]] %>% paste0(.,collapse = " ;;||;; ")
                                            
                                            # inner["media_full"] <- 
                                            #   map(.x = inner["media"],
                                            #       .f = ~ map(.x = .x,
                                            #                  .f = ~ .x[[5]])
                                            #   )[[1]] %>% paste0(.,collapse = " ;;||;; ")
                                            
                                            inner["media"] <- NULL
                                          
                                            
                                          flattened <- rlist::list.flatten(x = inner)
                                            return(flattened)
                                          }
                                          
                                          
                                          
                       ) %>% bind_rows()
                       
                       
                       
                       saveRDS(flat,
                               paste0("data/nogit/",w,"/",
                                      f %>% str_extract(pattern = '(?<=raw/).*?(?=.rds)'),
                                      ".rds"))
                       return(NULL)
                     }) #%>% bind_rows()
})

#all_files[1] %>% str_extract(pattern = '(?<=raw/).*?(?=.rds)')

targets <- readxl::read_xlsx("PATH/ap_targets_final.xlsx",sheet = 1)
urlreg <- 
  targets$lang_identifier %>% 
  .[. != ""] %>% 
  .[!is.na(.)] %>%
  paste0("",.,"", collapse = "|") %>% 
  str_replace_all("\\.","\\\\.") 

path2b = "PATH/ct_postdata_parsed/"
system.time({
  fb_posts1 <- purrr::map(.x = paste0(path2b,list.files(path = path2b)),
                         .f = function(f){
                           file <- readRDS(f)
                           
                           print(f)
                           print(lubridate::now())
                           
                           file$detected_domain <-
                             file$original_urls %>%
                             paste0(" ",.," ")  %>%
                             str_extract(.,pattern = paste0('(?<=https://|http://|https://www.|http://www.|\\s)(.*)(',urlreg,')'))
                           
                           file$detected_outlet <-
                             file$original_urls %>%
                             paste0(" ",.," ")  %>%
                             str_extract(.,pattern = paste0('(',urlreg,')'))
                           
                           file$detected_url <-
                             file$original_urls %>%
                             paste0(" ",.," ") %>%
                             str_extract(.,pattern = paste0('(?<=https://|http://|https://www.|http://www.|\\s)(.*)(',urlreg,').*?(?=\\s)'))
                           
                           return(file)
                           
                         }
  ) %>% bind_rows() 
})
saveRDS(fb_posts1,"PATH/fb_posts_final.rds")


####

path2a = "PATH/ct_linkdata_parsed/"
system.time({
  fb_links <- purrr::map(.x = paste0(path2a,list.files(path = path2a)),
                          .f = function(f){
                            file <- readRDS(f)
                            
                            print(f)
                            print(lubridate::now())
                            
                            file$detected_domain <-
                              file$original_urls %>%
                              paste0(" ",.," ")  %>%
                              str_extract(.,pattern = paste0('(?<=https://|http://|https://www.|http://www.|\\s)(.*)(',urlreg,')'))
                            
                            file$detected_outlet <-
                              file$original_urls %>%
                              paste0(" ",.," ")  %>%
                              str_extract(.,pattern = paste0('(',urlreg,')'))
                            
                            file$detected_url <-
                              file$original_urls %>%
                              paste0(" ",.," ") %>%
                              str_extract(.,pattern = paste0('(?<=https://|http://|https://www.|http://www.|\\s)(.*)(',urlreg,').*?(?=\\s)'))
                            
                            return(file)
                            
                          }
  ) %>% bind_rows() 
})

table(fb_links$detected_outlet)
table(fb_links$detected_domain) %>% View()

fb_links2 <- fb_links %>% distinct(platformId,.keep_all = T)

saveRDS(fb_links2,"PATH/fb_links_final.rds")





