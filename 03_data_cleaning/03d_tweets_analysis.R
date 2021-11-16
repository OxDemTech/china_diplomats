library(tidyverse)
library(lubridate)
data <- readRDS("data/nogit/final_publication_dataset_china_twitter_jun20_feb21.rds")

notfound <- readRDS("data/nogit/granular_statuses.rds")
found <- readRDS("data/nogit/found_statuses.rds")

notfound_new_may <- readRDS("data/nogit/granular_statuses_new_may.rds")
found_new_may <- readRDS("data/nogit/found_statuses_new_may.rds")


notfound$user_id %>% .[!. %in% notfound_new_may$user_id]
notfound_new_may$user_id %>% .[!. %in% notfound$user_id]

table(notfound_new_april$status)
table(notfound$status)

accs <- 
  readRDS("data/input_selectors/accs.rds")
targets <- readxl::read_xlsx("data/input_selectors/ap_targets_final.xlsx",sheet = 1)

mg_tg <- 
  targets %>% 
  select(country,lang,outlet,url,lang_identifier,prefix) %>% 
  filter(!is.na(lang_identifier))

table(data$detected_outlet) %>% View()

data <- 
  left_join(data,
            mg_tg %>% select(outlet,url) %>% distinct() ,
            by = c("detected_outlet" = "url"))

resolved <- readRDS("data/nogit/processed/resolved_feb.rds")

data2 <- 
  data %>%
  left_join(resolved %>% select(detected_url,resolved_url = resolved,resolved_outlet),
            by = c("detected_url" = "detected_url"))

data <-
  data2 %>% 
  mutate(user_id = as.character(user_id)) %>%
  left_join(found %>% select(user_id) %>% mutate(status1 = "200",
                                                 message1 = "User active on March 1st 2021"),
            by = "user_id") %>% 
  left_join(notfound %>% select(user_id,status2 = status,message2 = message),
            by = "user_id")  %>% 
  mutate(status = paste0(status1,status2) %>% str_remove("NA"),
         message = paste0(message1,message2) %>% str_remove("NA")) %>% 
  select(-message1,-message2,-status1,-status2)


datanew <- 
  data %>% 
  left_join(found_new_may %>% select(user_id) %>% mutate(statusnew1 = "200",
                                                 messagenew1 = "User active on May 5th 2021"),
            by = "user_id") %>% 
  left_join(notfound_new_may %>% select(user_id,statusnew2 = status,messagenew2 = message),
            by = "user_id")  %>% 
  mutate(status_new = paste0(statusnew1,statusnew2) %>% str_remove("NA"),
         message_new = paste0(messagenew1,messagenew2) %>% str_remove("NA")) %>% 
  select(-messagenew1,-messagenew2,-statusnew1,-statusnew2)


diprtsonly <- datanew %>% 
  filter(tweet_created_at <= as.Date("2021-01-31")) %>%
  filter(!is.na(retweet_dip))

table(may = diprtsonly$status_new,march = diprtsonly$status)


saveRDS(datanew,
        "data/nogit/final_publication_dataset_china_twitter_jun20_feb21_resolved_and_statuses_updated.rds")


#df <-datanew
df <- 
  readRDS("data/nogit/final_publication_dataset_china_twitter_jun20_feb21_resolved_and_statuses_updated.rds")

df <- 
  df %>% distinct(tweet_id,.keep_all = T)

susp_key <- c(`63` = "suspended",
              `50` = "deleted",
              `200` = "active")

df <- 
  df %>%
  mutate(final_status = recode(status,
                               !!!susp_key)) %>% 
  mutate(final_status_new = recode(status_new,
                               !!!susp_key)) %>% 
  arrange(tweet_created_at)

df_sub <-
  df %>% 
  filter(tweet_created_at <= as.Date("2021-01-31")) %>%
  mutate(day = floor_date(tweet_created_at,"day"),
         month = floor_date(tweet_created_at,"month"),
         week = floor_date(tweet_created_at,"week"),
         hour = tweet_created_at %>% round_date("hour") %>% format("%H") %>% as.numeric,
         wday = tweet_created_at %>% format("%a"))

df_sub$tweet_created_at <- 
  df_sub$tweet_created_at %>% 
  lubridate::as_datetime()

df_sub$tweet_created_chr <- 
  df_sub$tweet_created_at %>% 
  lubridate::as_datetime() %>% as.character()

df_sub$target <- NA
df_sub$target[!is.na(df_sub$retweet_dip)] <- df_sub$retweet_dip[!is.na(df_sub$retweet_dip)]
df_sub$target[!is.na(df_sub$quoted_dip)] <- df_sub$quoted_dip[!is.na(df_sub$quoted_dip)]
df_sub$target[!is.na(df_sub$reply_dip)] <- df_sub$reply_dip[!is.na(df_sub$reply_dip)]

df_sub$what <- NA
df_sub$what[!is.na(df_sub$retweet_dip)] <- "retweet"
df_sub$what[!is.na(df_sub$quoted_dip)] <- "quoted"
df_sub$what[!is.na(df_sub$reply_dip)] <- "reply"

saveRDS(df_sub,"data/nogit/final_df_sub_updated.rds")
