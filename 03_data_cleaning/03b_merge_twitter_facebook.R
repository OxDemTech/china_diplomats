library(tidyverse)

targets <- readxl::read_xlsx("data/input_selectors/ap_targets_final.xlsx",sheet = 1)

tweets <- purrr::map(.x = 
                       "data/nogit/final_publication_dataset_china_twitter_jun20_feb21.rds",
                     .f = ~ readRDS(.x)) %>% 
  bind_rows() %>% 
  mutate(where = "twitter") 

ct_pgdata <- 
  read_csv(file = "data/ct_lists/returned_ct_page_ids_final2.csv",col_types = cols(.default = col_guess(),
                                                                                   id = col_character(),
                                                                                   platformId = col_character()))

fb_posts <- purrr::map(.x = c("data/nogit/fb_links_final.rds",
                              "data/nogit/fb_posts_final.rds"),
                       .f = ~ readRDS(.x)) %>% 
  bind_rows() %>% 
  distinct(platformId,.keep_all = T) %>%
  mutate(where = "facebook") %>% 
  mutate(account.id = as.character(account.id)) %>%
  left_join(ct_pgdata %>% filter(type %in% c("A","B","C","E","S")) %>% select(post_dip = handle,
                                                                     id),
            by = c("account.id" = "id")) %>%
  left_join(ct_pgdata %>% filter(type %in% c("M","NewM")) %>% select(post_med = handle,
                                                                 id),
            by = c("account.id" = "id")) %>% 
  select(-n_media)


############

merge_tweets <- 
  tweets %>% 
  mutate(shares = quote_count + retweet_count,
         searchtext = paste0(full_text,";;",expanded_url)) %>%
  select(
    where,
    # Identifiers
    post_id = tweet_id_str,
    user_id = user_id,
    # Tweet Data
    text = full_text,
    searchtext = searchtext,
    created = tweet_created_at,
    is_quoting_tweet = is_quoting,
    is_retweeting_tweet = is_retweeting,
    # add urls
    detected_url = detected_url,
    detected_domain = detected_domain,
    detected_outlet = detected_outlet,
    original_urls = expanded_url,
    # User Data
    user_follower = user_followers_count,
    user_handle = user_screen_name,
    user_name = user_name,
    post_med = tweet_med,
    post_dip = tweet_dip,
    # other
    quoted_dip,
    retweet_dip,
    reply_dip,
    quoted_med,
    retweet_med,
    reply_med,
    reply_med_foreign,
    quoted_med_foreign,
    retweet_med_foreign,
    quoted_url,
    retweet_url,
    reply_url,
    # engagement
    likes = favorite_count,
    shares,
    replies = reply_count
  ) %>% 
  mutate(created = 
           lubridate::fast_strptime(as.character(created),
                                    "%Y-%m-%d %H:%M:%S",tz = "BST") %>% 
           as.character) %>% 
  mutate_at(vars(likes,shares,replies),
            funs(as.numeric))


merge_fb <- 
  fb_posts %>% 
  mutate(is_quoting_tweet = NA,
         is_retweeting_tweet = NA,
         quoted_dip = NA,
         retweet_dip = NA,
         reply_dip = NA,
         quoted_med = NA,
         retweet_med = NA,
         reply_med = NA,
         reply_med_foreign = NA,
         quoted_med_foreign = NA,
         retweet_med_foreign = NA,
         quoted_url = NA,
         retweet_url = NA,
         reply_url = NA,
         id = as.character(id),
         account.id = as.character(account.id),
         full_text = paste0(message,";;",caption,";;",description),
         searchtext = paste0(message,";;",caption,";;",description,";;",original_urls),
         all_reac = statistics.actual.likeCount + 
           statistics.actual.loveCount + 
           statistics.actual.wowCount+ 
           statistics.actual.hahaCount+ 
           statistics.actual.sadCount+ 
           statistics.actual.angryCount+ 
           statistics.actual.thankfulCount+ 
           statistics.actual.careCount) %>% 
  select(
    where,
    # Identifiers
    post_id = id,
    user_id = account.id,
    # Post Data
    text = full_text,
    searchtext = searchtext,
    created = date,
    is_quoting_tweet,
    # add urls
    detected_url = detected_url,
    detected_domain = detected_domain,
    detected_outlet = detected_outlet,
    original_urls = original_urls,
    # User Data
    user_follower = account.subscriberCount,
    user_handle = account.handle,
    user_name = account.name,
    post_med = post_med,
    post_dip = post_dip,
    quoted_dip,
    retweet_dip,
    reply_dip,
    quoted_med,
    retweet_med,
    reply_med_foreign,
    quoted_med_foreign,
    retweet_med_foreign,
    reply_med,
    quoted_url,
    retweet_url,
    reply_url,
    
    # Tweet performance
    likes = all_reac,
    shares = statistics.actual.shareCount,
    replies = statistics.actual.commentCount,
    # here add other reactions
  ) %>% 
  mutate(created = 
           lubridate::fast_strptime(as.character(created),"%Y-%m-%d %H:%M:%S",tz = "BST") %>% 
           as.character) 


merged <- 
  bind_rows(merge_tweets,
            merge_fb) %>% 
  distinct(where,post_id,.keep_all = T)

urlreg <- 
  targets$url %>%  #url statt lang_identifier
  .[. != ""] %>% 
  .[!is.na(.)] %>% unique() %>%
  paste0("",.,"", collapse = "|") %>% 
  str_replace_all("\\.","\\\\.")

mg_tg <- 
  targets %>% 
  select(country,lang,outlet,url,lang_identifier,prefix) %>% 
  filter(!is.na(lang_identifier))

merged2 <- 
  left_join(merged,
            mg_tg,
            by = c("detected_outlet" = "lang_identifier"))

merged_clean <- 
  merged2 %>% 
  mutate(text_clean = text %>% 
           # str_remove_all(pattern = "http\\S+\\s*") %>% 
           # str_remove_all(pattern = '[^\x01-\x7F]') %>%
           # str_remove_all(pattern = "@\\S+\\s*") %>%
           # str_replace_all(pattern = "#",replacement = " ")%>%
           # str_remove_all(pattern = "[1-9]\\.") %>% 
           str_to_lower()) %>%
  distinct(post_id,where,.keep_all = T)

saveRDS(merged_clean,"data/nogit/final_publication_dataset_jun.rds")

saveRDS(merged_clean %>% 
          filter(!is.na(post_dip) | !is.na(post_med)),
        "data/nogit/meddip_posts.rds")



#### already downloaded URLs

already <- 
  read_csv("data/vault/url_list_scrapingversion.csv")

######
produce_url_df <- FALSE
if(produce_url_df == T){
  set.seed(123)
  url_df <-
    merged_clean %>%
    mutate(detected_url = detected_url) %>%
    mutate(prefinal_url = 
             detected_url %>% 
             str_remove_all("(?<=\\.shtml|\\.html|\\.htm)[^l].*") %>% 
             sub(" ;;\\|\\|;; .*","",.) %>% 
             str_remove_all("http://|https://|https://www.|http://www.|www.")) %>%
    select(detected_url,prefinal_url,detected_outlet,outlet,prefix) %>%
    filter(!is.na(outlet)) %>%
    distinct(prefinal_url,.keep_all = TRUE) %>%
    mutate(final_url = paste0(prefix,prefinal_url)) %>%
    mutate(url_id = paste0("b",1:nrow(.))) %>%
    group_split(split = outlet == "unclear") %>%
    purrr::map(~ sample_frac(.x,1L)) %>%
    bind_rows() %>%
    mutate(order_id = 1:nrow(.)) %>%
    mutate(final_url = str_replace_all(final_url,"%2F","/"))
  
  
  newonly <- 
    url_df %>%
    filter(!prefinal_url %in% already$prefinal_url)
  
  write_csv(newonly,"data/vault/new_urls_feb21_fixedurls.csv")
  
}

