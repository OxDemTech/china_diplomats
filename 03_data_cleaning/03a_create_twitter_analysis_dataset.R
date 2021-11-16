library(RPostgreSQL)
library(DBI)
library(tidyverse)
library(dplyr)

################

targets <- readxl::read_xlsx("data/input_selectors/ap_targets_final.xlsx",sheet = 1)

db <- 'xxxxxxx'
host_db <- "xxxxx"
db_port <- 'xxxxxx'  
db_user <- 'xxxxxx'  
db_password <- 'xxxxxxx'
con <- dbConnect(RPostgres::Postgres(), 
                 dbname = db, host=host_db, port=db_port, 
                 user=db_user, password=db_password) 

dbListTables(con) 

dbListFields(con,"tweets_dynamic")

system.time({
  
  sql <- 
    "
SELECT * FROM tweets_static
  LEFT JOIN (
      SELECT DISTINCT ON (a.user_id) a.*
      FROM users_dynamic a
      INNER JOIN (
      SELECT user_id, MAX(user_observed_ts) user_observed_ts
      FROM users_dynamic
      GROUP BY user_id
      ) b ON a.user_id = b.user_id AND a.user_observed_ts = b.user_observed_ts
  ) u ON (tweets_static.user_id = u.user_id) 
  LEFT JOIN users_static ON (u.user_id = users_static.user_id)"
  
  data <- dbGetQuery(conn = con,
                     statement = sql)
})

object.size(data)

###################################

dbListFields(con,"tweets_dynamic")

system.time({
  
  sql <- 
    "
      SELECT DISTINCT ON (a.tweet_id) a.*
      FROM tweets_dynamic a
      INNER JOIN (
      SELECT tweet_id, MAX(timestamp_observed) timestamp_observed
      FROM tweets_dynamic
      GROUP BY tweet_id
      ) b ON a.tweet_id = b.tweet_id AND a.timestamp_observed = b.timestamp_observed
  "
  
  dynamic <- dbGetQuery(conn = con,
                        statement = sql)
})


system.time({
  dyn_all <- dbGetQuery(conn = con,
                     statement = "
  SELECT * FROM tweets_dynamic
  ")
})


zero2NA_char <- 
  function(x){
    ifelse(x == 0 | x == "0", NA,as.character(x))
  }


#################################

# Convert User IDs to Character and make NA if 0
data <- 
  data %>% mutate_at(vars(tweet_id,
                          user_id,
                          contains("in_reply"),
                          retweet_id,
                          quoted_id),
                     funs(zero2NA_char(.))) %>% select(-contains(".."))

dynamic <- 
  dynamic %>% mutate_at(vars(tweet_id,tweet_dynamic_id),
                        funs(zero2NA_char(.))) %>% select(-contains(".."))



# Query URLs
system.time({
  urls <- dbGetQuery(conn = con,
                     statement = "
  SELECT * FROM urls
  ")
})

# URL IDs as character
urls <- 
  urls %>% mutate_at(vars(tweet_id,url_id),
                     funs(zero2NA_char(.))) %>% select(-contains(".."))

# Create Regex Search String for all potential domains
urlreg <- 
  targets$url %>%  
  .[. != ""] %>% 
  .[!is.na(.)] %>%
  unique() %>%
  paste0("",.,"", collapse = "|") %>% 
  str_replace_all("\\.","\\\\.")

# Extract URLS + Outlets, allowing for potential domain prefixes via "(.*)" in regex
system.time({
  urls$detected_domain <-
    urls$expanded_url %>%
    paste0(" ",.," ")  %>%
    str_extract(.,pattern = paste0('(?<=https://|http://|https://www.|http://www.|\\s)(.*)(',urlreg,')'))
})

system.time({
  urls$detected_outlet <-
    urls$expanded_url %>%
    paste0(" ",.," ")  %>%
    str_extract(.,pattern = paste0('(',urlreg,')'))
})

system.time({
  urls$detected_url <-
    urls$expanded_url %>%
    paste0(" ",.," ") %>%
    str_extract(.,pattern = paste0('(?<=https://|http://|https://www.|http://www.|\\s)(.*)(',urlreg,').*?(?=\\s)'))
})

table(urls$detected_domain) %>% View()

# Join only if URL from source list was detected, and deduplicate (keep only first if multiple URLs per tweet)

urlsub <- urls %>% 
  filter((is.na(detected_outlet))|(!str_detect(detected_outlet,"usembassy"))) %>%
  distinct(tweet_id,.keep_all = TRUE)

#table(urlsub$detected_domain) %>% View()
#table(urlsub$detected_outlet,useNA = "always") %>% View()

system.time({
  full_tweets <- 
    full_join(data,
              urlsub, 
              by = "tweet_id") %>% 
    left_join(dynamic,
              by = "tweet_id")
})


###########################################################################

#full_tweets %>% head(10) %>%View()

twitter_targets <- 
  readxl::read_xlsx("data/input_selectors/ap_targets_final.xlsx",sheet = 2)


# redo with IDs?
tw_dips <- 
  twitter_targets %>% 
  filter(`A/E/C/S` %in% c("A","B","C","E","S")) %>%
  pull(twitter_handle) %>% 
  .[!is.na(.)] %>%
  unique()

tw_meds <- 
  twitter_targets %>% 
  filter(`A/E/C/S` == "M") %>%
  #filter(lang == "en") %>%
  pull(twitter_handle) %>% 
  .[!is.na(.)] %>%
  unique()

twmatch_foreign <-
  twitter_targets %>% 
  filter(`A/E/C/S` == "M") %>%
  filter(lang != "en") %>%
  pull(twitter_handle) %>% 
  .[!is.na(.)] %>%
  unique()

med_tweet_ids <- 
  full_tweets %>% 
  filter(user_screen_name %in% tw_meds) %>%
  select(tweet_id,user_screen_name) %>% 
  distinct()

foreign_med_tweet_ids<- 
  full_tweets %>% 
  filter(user_screen_name %in% twmatch_foreign) %>%
  select(tweet_id,user_screen_name) %>% 
  distinct()


dip_tweet_ids <- 
  full_tweets %>% 
  filter(user_screen_name %in% tw_dips) %>%
  select(tweet_id,user_screen_name) %>% 
  distinct()

url_tweet_ids <- 
  full_tweets %>% 
  filter(!is.na(detected_url))%>% 
  select(tweet_id,detected_outlet) %>% 
  distinct()


########################################################################

system.time({
  all_tweets_sub1 <- 
    full_tweets %>% 
    mutate(date = lubridate::as_datetime(tweet_created_at)) %>% 
    filter(date >= lubridate::as_datetime("2020-06-09 00:00:00"))%>% 
    left_join(dip_tweet_ids %>% rename(quoted_dip = user_screen_name),
              by = c("quoted_id" = "tweet_id")) %>% 
    left_join(dip_tweet_ids %>% rename(retweet_dip = user_screen_name),
              by = c("retweet_id" = "tweet_id"))%>% 
    left_join(dip_tweet_ids %>% rename(reply_dip = user_screen_name),
              by = c("in_reply_to_status_id" = "tweet_id")) %>% 
    left_join(med_tweet_ids %>% rename(quoted_med = user_screen_name),
              by = c("quoted_id" = "tweet_id")) %>% 
    left_join(med_tweet_ids %>% rename(retweet_med = user_screen_name),
              by = c("retweet_id" = "tweet_id"))%>% 
    left_join(med_tweet_ids %>% rename(reply_med = user_screen_name),
              by = c("in_reply_to_status_id" = "tweet_id")) %>% 
    left_join(foreign_med_tweet_ids %>% rename(quoted_med_foreign = user_screen_name),
              by = c("quoted_id" = "tweet_id")) %>% 
    left_join(foreign_med_tweet_ids %>% rename(retweet_med_foreign = user_screen_name),
              by = c("retweet_id" = "tweet_id"))%>% 
    left_join(foreign_med_tweet_ids %>% rename(reply_med_foreign = user_screen_name),
              by = c("in_reply_to_status_id" = "tweet_id")) %>% 
    ##
    left_join(url_tweet_ids %>% rename(quoted_url = detected_outlet),
              by = c("quoted_id" = "tweet_id")) %>%
    left_join(url_tweet_ids %>% rename(retweet_url = detected_outlet),
              by = c("retweet_id" = "tweet_id"))%>%
    left_join(url_tweet_ids %>% rename(reply_url = detected_outlet),
              by = c("in_reply_to_status_id" = "tweet_id")) %>%
    #
    left_join(dip_tweet_ids %>% rename(tweet_dip = user_screen_name),
              by = c("tweet_id" = "tweet_id")) %>% 
    left_join(med_tweet_ids %>% rename(tweet_med = user_screen_name),
              by = c("tweet_id" = "tweet_id"))
})

system.time({
  saveRDS(all_tweets_sub1,"data/nogit/final_publication_alldata_nofilter.rds")
})

all_tweets_sub <- 
  all_tweets_sub1 %>% 
  filter(
         (!is.na(detected_url))|
         (!is.na(tweet_dip))|
         (!is.na(tweet_med))|
         (!is.na(quoted_dip))|
         (!is.na(retweet_dip))|
         (!is.na(reply_dip))|
         (!is.na(quoted_med))|
         (!is.na(retweet_med))|
         (!is.na(reply_med_foreign))|
         (!is.na(quoted_med_foreign))|
         (!is.na(reply_med_foreign))|
         (!is.na(reply_med))|
         (!is.na(quoted_url))|
         (!is.na(retweet_url))|
         (!is.na(reply_url))
  )


system.time({
  saveRDS(all_tweets_sub,"data/nogit/final_publication_dataset_china_twitter_jun20_feb21.rds")
})


