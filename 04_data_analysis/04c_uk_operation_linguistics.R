library(tidyverse)
library(lubridate)

foruk1a <- readRDS("data/processed/foruk1.rds")

foruka <- 
  foruk1a %>% 
  filter(n > 50| (user_screen_name %in%c("Crouchi27494110","HiddenD99075856"))) %>% #slightly below 25 because early suspended
  filter(perc_uk > 0.5 | (user_screen_name %in% c("reliable_young")))


cells2a <- readRDS("data/processed/cells2.rds") %>% filter(str_detect(cell,"cell"))

df <- 
  readRDS("data/nogit/newtweets_uk2.rds") %>%
  filter(how_included == "main") %>%
  distinct(tweet_id,.keep_all = T) 

names(df)

# Parse CT Date format correctly

df$tweet_created_at <-  lubridate::fast_strptime(df$tweet_created_at,
                                         format = "%Y-%m-%dT%H:%M:%S.000Z",
                                         tz = "CET",lt = F)  %>% unlist()
df$user_created_at <-  lubridate::fast_strptime(df$user_created_at,
                                         format = "%Y-%m-%dT%H:%M:%S.000Z",
                                         tz = "CET",lt = F)  %>% unlist()




df %>% 
  filter(user_screen_name %in% celllist$user_screen_name) %>% 
  select(user_screen_name, tweet_created_at,tweet_text) %>%
  arrange(tweet_created_at) %>% 
  filter(!str_detect(tweet_text,"RT @")) %>%
  View()



# Add date metadata variables
df <-
  df %>%
  mutate(datetime = as_datetime(tweet_created_at)) %>%
  mutate(min5 = round_date(tweet_created_at,unit = "5 minutes")) %>%
  mutate(day = lubridate::floor_date(tweet_created_at %>% as_datetime(),"day"),
         week = lubridate::floor_date(tweet_created_at%>% as_datetime(),"week"))%>%
  mutate(day_created = lubridate::floor_date(user_created_at %>% as_datetime(),"day") %>% format("%Y-%m-%d"))

colnames(df) <- colnames(df) %>% str_replace_all(" ","_")

df %>%
filter(user_screen_name %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina") | user_screen_name %in% celllist$c..Xiaojin05484077....Caterpi27848664....ladybug23758032....Bumbleb75459847...) %>%
  select(user_screen_name,user_created_at,source,tweet_created_at,tweet_text) %>%
  arrange(tweet_created_at) %>% #writexl::write_xlsx("uktarg.xlsx")
  View()

df %>%
  group_by(week,user_screen_name) %>%
  filter(user_screen_name %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina") | user_screen_name %in% celllist$c..Xiaojin05484077....Caterpi27848664....ladybug23758032....Bumbleb75459847...) %>%
  
  summarise(n = n()) %>%
  ggplot(aes(x = week,y = n,col = user_screen_name,group = user_screen_name)) + 
  geom_point()+
  geom_line() +
  facet_wrap(~ user_screen_name,ncol = 4,scales = "free_y")

#####################################################################

library(quanteda)

names(df)
df$quoted


df_aug <- 
  df %>%
  filter(datetime > as.Date("2020-01-01")) %>%
  mutate(tweet_text = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweet_text)) %>%
 filter(!str_detect(tweet_text,"RT")) %>%
  filter(str_detect(tweet_text,"@AmbLiuXiaoMing")) %>% mutate(tweet_text  = tweet_text %>% str_remove_all("@AmbLiuXiaoMing")) %>%
  filter(user_screen_name %in% foruka$user_screen_name) %>%
  group_by(user_screen_name) %>%
  mutate(n_user = n()) %>% filter(n_user > 5)


system.time({
  df_sub <- readRDS("data/nogit/final_df_sub.rds")
})

ukall <- 
  df_sub %>%
  filter(target %in% c("AmbLiuXiaoMing","ChineseEmbinUK"))
#saveRDS(ukall,"data/nogit/ukall.rds")

ukall$target_id <- NA
ukall$target_id[ukall$what == "reply"] <- ukall$in_reply_to_status_id[ukall$what == "reply"]
ukall$target_id[ukall$what == "retweet"] <- ukall$retweet_id[ukall$what == "retweet"]


ukrts1 <- 
  df_sub %>%
  filter(reply_dip %in% c("AmbLiuXiaoMing","ChineseEmbinUK") |
           user_screen_name %in% c("AmbLiuXiaoMing","ChineseEmbinUK"))


ukrts <-
  ukrts1 %>%
  left_join(cells2a %>% select(cell,member),
            by = c("user_screen_name" = "member")) %>%
  mutate(full_text = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", full_text)) %>%
  mutate(full_text = gsub("@[A-z,0-9]+", "", full_text)) %>%
  filter(!str_detect(full_text,"/")) %>%
  filter(!str_detect(full_text,"RT")) %>%
  mutate(tweet_text  = full_text %>% str_remove_all("@AmbLiuXiaoMing")) %>%
  group_by(user_screen_name) %>%
  mutate(n_user = n()) %>% filter(n_user >= 5)

table(ukrts$user_screen_name) %>% sort(decreasing = T) %>% length()


corp <- quanteda::corpus(df_aug %>% 
                         #  sample_n(10000) %>%
                               select(text = tweet_text,
                                      user_screen_name )
)

corp <- quanteda::corpus(ukrts %>% 
                           #  sample_n(10000) %>%
                           select(text = tweet_text,
                                  user_screen_name,
                                  cell)
)


summary(corp)

rem_manual <- c("said",
                month.abb %>% str_to_lower(),
                month.name %>% str_to_lower(),
                c("mon","tue","wed","thu","fri","sat","sun"),
                c("monday","tuesday","wednesday","thursday","friday","saturday","sunday"),
                "global times",
                "people's daily",
                "china daily",
                "beijing review","beijing_review",
                "video","player","close","source","editor","huaxia",
                "video_player","editor_huaxia","photo",
                "plus","china plus","sixth tone","people's daily",
                "one","can","said","two","three","also","make","gt",
                "photo:xinhua")



cntokens <- 
  tokens(corp,
         what = "word",
         remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE,
         remove_separators = TRUE,
         #split_hyphens = FALSE,
         include_docvars = TRUE,
         verbose = T
  ) %>%
  tokens_replace(x = .,pattern = "U.S",replacement = "US") %>% 
  tokens_tolower(x = ., keep_acronyms = T) %>%
  tokens_select(x = ., 
                pattern = c(#,stopwords("en")
                  rem_manual), 
                selection = "remove") %>%
  tokens_remove(., pattern = "[\\w\\d]+\\.[\\w\\d]+", valuetype = "regex") %>%
  tokens_keep(., min_nchar = 2)


cntokens_stemmed <- 
  cntokens %>% 
 # tokens_wordstem() %>%
  tokens_ngrams(n = c(#3,
    4,5,6))


cncorpus_dfm_3gram <- dfm(cntokens_stemmed,
                          tolower = F,
                          stem =  F,
                         # ngrams = c(5),
                          verbose = TRUE
)



cncorpus_dfm_3gram_trimmed <- 
  cncorpus_dfm_3gram %>% 
  dfm_trim(.,
           min_termfreq = 5, 
           #max_termfreq = 0.9,
           termfreq_type = "count",
           min_docfreq = 5,
           #max_docfreq = 0.9, 
           docfreq_type = "count",
           verbose = T) 

colnames(cncorpus_dfm_3gram_trimmed)

#stat_2grams <- textstat_frequency(cncorpus_dfm_3gram_trimmed, n = 50000)
#remove_2grams <- colnames(cncorpus_dfm_3gram)[!colnames(cncorpus_dfm_3gram) %in% colnames(cncorpus_dfm_3gram_trimmed)]

class(cncorpus_dfm_3gram_trimmed)

conv <-
  cncorpus_dfm_3gram_trimmed %>%
  dfm_group(groups = "user_screen_name")
  
docvars(cncorpus_dfm_3gram_trimmed)

long <- 
  convert(conv,
          to = "data.frame") %>% 
  gather(word,n,-doc_id) %>%
  filter(n > 0) %>%
  group_by(word) %>%
  mutate(n_word = n()) %>%
  filter(n_word > 3) %>% filter(n_word < 10)

ambwords <- 
  c(long %>% filter(doc_id %in% c("AmbLiuXiaoMing",
                                  "ChineseEmbinUK")) %>% 
      pull(word) %>%
      unique)


noamblong <- 
  left_join(
    long,
    data.frame(word = ambwords,
               amb = T),
    by = "word"
  )  #%>% mutate(amb = ifelse(!is.na(amb),T,F)) %>% filter(amb == F)


edges <- 
  noamblong %>%
  ungroup() %>% select(Source = doc_id,Target = word, Weight = n) %>%
  write_csv("data/gephi/words36.csv")

nodes <- 
  bind_rows(
    edges %>% ungroup() %>% 
      select(Id = Source,Label = Source) %>% mutate(type = "user") %>%
      left_join(cells2a %>% select(cell,member), by = c("Id" = "member"))%>% 
      mutate(Label = ifelse(Id %in% c("AmbLiuXiaoMing","ChineseEmbinUK")|!is.na(cell),Label,"")) %>%
      mutate(type = ifelse(Id %in% c("AmbLiuXiaoMing","ChineseEmbinUK"),"ambs",type)),
    edges %>% ungroup() %>% select(Id = Target,Label = Target) %>% mutate(type = "wod",cell = "word") 
  ) %>%
  distinct() %>%
  write_csv("data/gephi/word_nodes36.csv")

################################################################
liutweets <- 
  df_sub %>%
  filter(tweet_created_at > as.Date("2020-08-01")) %>%
 # filter(as.character(month) %in% c("2020-10-01","2020-11-01")) %>%
  filter(tweet_dip %in% c("AmbLiuXiaoMing","ChineseEmbinUK")) %>% 
  mutate(is_thread_reply_to_self = tweet_dip == reply_dip) %>%
  mutate(is_thread = ifelse(is.na(as.character(is_thread_reply_to_self)),"noreply","threadreply")) %>%
  filter(is_thread == "threadreply")

edges <- 
  ukall %>%
  filter(target_id %in% liutweets$tweet_id) %>%
  filter(what == "retweet") %>%
  filter(tweet_created_at > as.Date("2020-08-01")) %>%
 #filter(as.character(month) %in% c("2020-10-01","2020-11-01")) %>%
  group_by(Source = user_screen_name,Target = target_id,edgetype = what) %>% 
  #select(Source = user_screen_name,Target = target_id, what) %>%
  summarise(n = n()) %>%
#  group_by(Source) %>% filter(n() > 5) %>%
  write_csv("data/gephi/target_net_edges.csv")

nodes <- 
  bind_rows(
    edges %>% ungroup() %>% 
      select(Id = Source,Label = Source,edgetype) %>% mutate(type = "user") %>%
      left_join(cellsall %>% select(cell,member), by = c("Id" = "member"))%>% 
      mutate(Label = ifelse(!is.na(cell),Id,"")) %>%
      mutate(node_color = ifelse(!is.na(cell),"CELL Account","nocell")) %>%
      distinct(),
    liutweets %>% ungroup() %>% select(Id = tweet_id,is_thread)  %>%
      mutate(what = is_thread,Label = "",node_color = is_thread)
  ) %>%
  distinct() %>%
  write_csv("data/gephi/target_net_nodes.csv")



########################################################################

sel <- 
  df %>% 
  select(user_screen_name,min5) 

system.time({
  merged <- 
    full_join(
      sel %>% select(user1 =user_screen_name,min5) %>%group_by(user1) %>% mutate(n1 = n()),
      sel %>% select(user2 =user_screen_name,min5)%>%group_by(user2) %>% mutate(n2 = n()),
      by = c("min5")
    ) %>%
    filter(user1 != user2)
})

system.time({
 same <-
   merged %>% 
    ungroup() %>%
    #slice(1:100000) %>%
    group_by(user1,n1,user2,n2) %>%
    rowwise() %>%
    mutate(both = paste0(sort(c(user1,user2)),collapse = "--")) %>%
    distinct(min5,both,.keep_all = T) %>%
    group_by(user1,n1,user2,n2,both) %>%
    summarise(n = n()) %>% 
    mutate(rate = (n/(2*(n1+n2))))
})

same%>%ungroup() %>%
  filter(user1 %in% (cells2a %>% filter(str_detect(cell,"cell")) %>%pull(member))) %>%
  View()
#sort(c("a","b")) %>% paste0(collapse = "|")


df %>%
  filter(user_screen_name %in% (cells2a %>% filter(str_detect(cell,"cell4")) %>%pull(member))) %>%
  filter(user_screen_name %in% foruka$user_screen_name) %>%
 # filter(day_created != "2020-08-11") %>%
  filter(user_created_at > as.Date("2020-10-01")) %>%
  select(user_screen_name,user_created_at,source,datetime,day,tweet_text) %>%
  filter(!str_detect(user_screen_name,"america")) %>%
  filter(!str_detect(tweet_text,"RT")) %>%
  group_by(day)%>%
  arrange(day,tweet_text) %>%
  View() 

df %>%
  #filter(str_detect(paste0(" ",tweet_text)," Hope ")) %>%
   filter(user_screen_name %in% (cells2a %>% filter(str_detect(cell,"cell3")) %>%pull(member))) %>%
  filter(user_screen_name %in% foruka$user_screen_name) %>%
  # filter(day_created != "2020-08-11") %>%
 # filter(datetime > as.Date("2020-10-01")) %>%
  select(user_screen_name,user_created_at,source,datetime,day,tweet_text) %>%
  filter(!str_detect(user_screen_name,"america")) %>%
  filter(!str_detect(tweet_text,"RT|@AmbLiuXiaoMing")) %>%
  group_by(day)%>%
  arrange(desc(day),tweet_text) %>%
  View() 

df %>%
  mutate(HOPE = str_detect(paste0(" ",tweet_text)," Hope ")) %>%
  group_by(user_screen_name,HOPE) %>% 
  summarise(n = n()) %>%
  group_by(user_screen_name) %>% 
  mutate(perc = n()/sum(n),
         sum = sum(n)) %>%
  filter(HOPE == T) %>% 
  View()


df %>%
  filter(str_detect(tweet_text,"Hope ")) %>%
  filter(user_screen_name %in% (cells2a %>% filter(str_detect(cell,"cell4")) %>%pull(member))) %>%
  #filter(user_screen_name %in% foruka$user_screen_name) 
  ggplot(aes(x = datetime,y = user_screen_name)) + 
  geom_point()






########################################################################
df %>%
 filter(user_screen_name %in% (cells2a %>% filter(str_detect(cell,"cell")) %>%pull(member))) %>%
  filter(user_screen_name %in% foruka$user_screen_name) %>%
  filter(day_created == "2020-08-11") %>%
  select(user_screen_name,user_created_at,source,datetime,tweet_text) %>%
  filter(!str_detect(tweet_text,"RT")) %>%
  View() 

df %>%
  filter(user_screen_name %in% (cells2a %>% filter(str_detect(cell,"cell3")) %>%pull(member))) %>%
  filter(user_screen_name %in% foruka$user_screen_name) %>%
 # filter(day_created == "2020-08-11") %>%
  select(user_screen_name,user_created_at,source,datetime,tweet_text) %>%
 # filter(!str_detect(tweet_text,"RT")) %>%
  View() 


xx <- 
  df %>%
  filter(user_screen_name %in% (cells2a$member)) %>%
  filter(user_screen_name %in% foruka$user_screen_name) %>%
  select(user_screen_name,user_created_at,source,datetime,tweet_text) %>%
  arrange(datetime) %>%
  mutate(lag = as.numeric(datetime - lag(datetime,1))) %>%
  mutate(laguser = lag(user_screen_name)) %>%
  group_by(user_screen_name) %>%
  mutate(nuser = n()) %>%
  filter(lag < 60,
         laguser != user_screen_name) %>%

  group_by(user_screen_name,user_created_at,laguser,nuser) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  mutate(perc = n/nuser) %>%
  ungroup()

xxxx <-
    left_join(xx,
            xx%>% select(user_screen_name2 = user_screen_name,laguser2 = laguser,
                         n2 = n,
                         user_created_at2 = user_created_at) ,
            by = c("laguser" = "user_screen_name2","user_screen_name" = "laguser2")) %>%
  mutate(created_dif = abs(user_created_at - user_created_at2))


################################################

when <- 
  df %>% 
 # filter(!is.na(retweet_dip))%>%
  filter(user_screen_name %in% (cells2 %>% filter(str_detect(cell,"cell")) %>% pull(member))) %>%
  filter(user_screen_name %in% foruka$user_screen_name) %>%
  
 # filter(final_status %in% c("active","suspended")) %>%
  mutate(hour = format((tweet_created_at %>% floor_date("hour")) + hours(0),"%H") %>% as.numeric,
         weekday = format(tweet_created_at %>% floor_date("day"),"%a") ) %>%
  group_by(#final_status, 
           user_screen_name,hour) %>% 
  summarise(n = n()) %>%
  full_join(expand_grid(hour =(0:23),
                        user_screen_name = unique(.$user_screen_name)),
            by = c("hour","user_screen_name")) %>% 
  mutate(n =ifelse(is.na(n),0,n))%>%
  group_by(user_screen_name) %>%
  mutate(total_n = sum(n)) %>%
  mutate(perc = n/total_n)  %>%
  left_join(cells2 %>% select(member,cell),
            by = c("user_screen_name" = "member"))


ggho <- 
  ggplot(when  %>%
           filter(user_screen_name %in% (cells2 %>%
                                           filter(str_detect(cell,"cell")) %>% pull(member))) )  +
  geom_tile(aes(x = hour %>% as.numeric,
                y = user_screen_name,
                fill = perc )) +
  scale_fill_viridis_c(name = "Percent of\nTweets in hour",
                       option = "A",labels = percent,trans = "log1p")+
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = c(0:23)) +
  labs(x = "Tweets made in hour of the day - Time Zone: GMT (British Standard Time)",
       y = NULL,
       title = "Cell 1: Corresponding Time Patterns",
       subtitle = "Share of Tweets Made in Each Hour British Standard Time BST/GMT")+
  theme(legend.position = "right",
        plot.title = ggplot2::element_text(family="Roboto",
                                           size=22,
                                           face="bold",
                                           color = oxblue),
        strip.text = ggplot2::element_text(family="Roboto",
                                           size=15,
                                           face="bold",
                                           color = "black"),
        #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
        plot.subtitle = ggplot2::element_text(family="Roboto",
                                              size=16,
                                              face = "bold",
                                              color= "#007fc8",#oxblue,
                                              margin=ggplot2::margin(0,0,5,0)),
        plot.caption = ggplot2::element_blank()) 

ggho


datee <- 
  df %>% 
  # filter(!is.na(retweet_dip))%>%
  filter(user_screen_name %in% (cells2 %>% filter(str_detect(cell,"cell")) %>% pull(member))) %>%
  filter(user_screen_name %in% foruka$user_screen_name) %>%
  group_by(day,user_screen_name,user_created_at) %>%
  summarise(n = n()) %>%
  mutate(active = ifelse(n>0,T,F))

ggplot(datee,
       aes(x = day,
           y = paste0(user_created_at,user_screen_name))) + 
  geom_tile()


 