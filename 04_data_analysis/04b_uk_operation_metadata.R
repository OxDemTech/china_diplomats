library(tidyverse)
#library(ggoxford)
library(tidyverse)
library(ggtext)
#install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
library(ggoxford)
library(stringr)
library(emojifont)
load.fontawesome()
library(magrittr)
#extrafont::font_import()
extrafont::loadfonts(device = "pdf",quiet = T)
extrafont::loadfonts(device = "win",quiet = T)
extrafont::loadfonts(device = "postscript",quiet = T)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")
library(lubridate)
library(scales)
library(xtable)
library(lubridate)


library(quanteda)

system.time({
  df_sub <- readRDS("data/nogit/final_df_sub.rds")
})

df_sub$target_id <- NA
df_sub$target_id[!is.na(df_sub$retweet_dip)] <- df_sub$retweet_id[!is.na(df_sub$retweet_dip)]
df_sub$target_id[!is.na(df_sub$quoted_dip)] <- df_sub$quoted_id[!is.na(df_sub$quoted_dip)]
df_sub$target_id[!is.na(df_sub$reply_dip)] <- df_sub$in_reply_to_status_id[!is.na(df_sub$reply_dip)]


all_lags <- readRDS("data/processed/all_rt_lags.rds")

accs <- 
  readRDS("data/input_selectors/accs.rds")

dips <- accs %>% filter(platform == "twitter" & type == "diplomat") %>% pull(handle)
meds <- accs %>% filter(platform == "twitter" & type == "statemedia") %>% pull(handle)

###################


## start by all tweets liu ro emb made after June 9th

uks <- c("ChineseEmbinUK", 
         "AmbLiuXiaoMing")


## 

celllist <- 
  rbind.data.frame(
    c("userhandle_1","consec1","",""),
    # ... 
    c("userhandle_n","","lang","")
  ) %>% set_names(c("user_screen_name","x1","x2","x3"))


###################

uks <- c("AmbLiuXiaoMing","ChineseEmbinUK")

uktweets1 <- 
  df_sub %>%
  filter(user_screen_name %in% uks) %>%
  filter(tweet_created_at >= as.Date("2020-06-09"))%>%
  filter(tweet_created_at <= as.Date("2021-01-31"))

uktweets <- 
  df_sub %>%
  filter(is_retweeting == F) %>%
  filter(user_screen_name %in% uks) %>%
  filter(tweet_created_at >= as.Date("2020-06-09"))%>%
  filter(tweet_created_at <= as.Date("2021-01-31")) %>%
  mutate(is_thread = ifelse(user_screen_name == in_reply_to_screen_name,"thread","nothread")) %>%
  mutate(is_thread = ifelse(is.na(is_thread),"nothread",is_thread))

uktweets %>%
  summarise(li = sum(favorite_count,na.rm = T),
            rep = sum(reply_count,na.rm = T),
            ret = sum(retweet_count,na.rm = T))

uktweets %>%
  group_by(user_screen_name) %>% 
  summarise(n = n()) %>%
  ungroup() %>% mutate(s = sum(n))

ukreps <- 
  df_sub %>%
  filter(!user_screen_name %in% uks) %>%
  filter(reply_dip %in% uks) %>%
  filter(tweet_created_at >= as.Date("2020-06-09"))%>%
  filter(tweet_created_at <= as.Date("2021-01-31"))

ukrts <- 
  df_sub %>%
  filter(!user_screen_name %in% uks) %>%
  filter(retweet_dip %in% uks) %>%
  filter(tweet_created_at >= as.Date("2020-06-09"))%>%
  filter(tweet_created_at <= as.Date("2021-01-31"))

uktarg <- 
  df_sub %>%
  filter(is_quoting == F) %>%
  filter(target %in% c("AmbLiuXiaoMing","ChineseEmbinUK"))


fortwitter <-
  uktarg %>% 
  group_by(user_screen_name,user_id,final_status,what) %>% 
  summarise(n = n())%>%
  pivot_wider(names_from = what, values_from = n) %>%
  select(user_screen_name,user_id,status_march1 = final_status,
         n_retweets = retweet,n_replies = reply) %>% 
  ungroup() %>%
  mutate(sum_rt_reply = rowSums(select(.,n_retweets,n_replies),na.rm = T)) %>%
  distinct() %>%
  filter(user_screen_name %in% celllist$user_screen_name) %>% 
  arrange(desc(sum_rt_reply))

#writexl::write_xlsx(fortwitter,"ap_62_accounts.xlsx")


uktarg %>% 
  group_by(target,
           what,
           network = user_screen_name %in% celllist$user_screen_name) %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = network,values_from = n) %>%
  mutate(perc = `TRUE`/(`TRUE`+`FALSE`)) %>% 
  rename("62accounts" = `TRUE`,
         "control" = `FALSE`)


############

maingroup <- celllist$user_screen_name %>% as.character()

controlgroup <-
  df_sub %>%
  filter(!is.na(retweet_dip)) %>%
  group_by(user_screen_name,what) %>%
  summarise(n = n()) %>% 
  group_by(user_screen_name) %>%
  mutate(n_user = n()) %>%
  filter(n_user >= 1) %>% ungroup() %>%
  pull(user_screen_name) %>% 
  .[! .%in% maingroup]

controlgroup_uk <-
  controlgroup %>% 
  .[. %in% unique(uktarg$user_screen_name)]

controlgroup_uk %>% length()

#################

sharedf <- readRDS("data/uk/sharedf.rds")

sharedf[is.na(sharedf)] <- 0

sharedf %>% 
  mutate(total_mean = mean(rep_perc_nothread),
         total_sd = sd(rep_perc_nothread)) %>%
  group_by(network = user_screen_name %in% celllist$user_screen_name,
           total_mean,
           total_sd) %>%
  summarise(n = n(),
            meanrep = mean(rep_perc_nothread,na.rm = T),
            grp_sd = sd(rep_perc_nothread))


sharedf %>% 
  mutate(total_mean = mean(rt_perc_thread),
         total_sd = sd(rt_perc_thread)) %>%
  group_by(network = user_screen_name %in% celllist$user_screen_name,
           total_mean,
           total_sd) %>%
  summarise(n = n(),
            meanrep = mean(rt_perc_thread,na.rm = T),
            grp_sd = sd(rt_perc_thread))


library(ggbeeswarm)
library(ggrepel)

outpath <- "....."


ggrep<-
  ggplot(sharedf %>% filter(rep_nothread >= 1)) + 
  aes(x = 1,y = rep_perc_nothread,
      col = user_screen_name %in% maingroup) +
  #geom_hline(yintercept = 0.15,linetype = "dashed")+
  geom_quasirandom(alpha = 0.5)+
  #geom_beeswarm(alpha = 0.5,priority='density')+
  scale_color_manual(name = "Account",
                     values = c("grey","red"),
                     labels = c("Reference Group",
                                "Inauthentic Network")) +
  geom_text_repel(data = function(x) x %>% 
                    filter( user_screen_name %in% maingroup,
                            rep_perc_nothread > 0.5),
                  aes(label = user_screen_name),direction = "both",force = 1)+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits = c(0,1),labels = scales::percent_format(suffix = "")) +
  theme_minimal() + 
  theme(legend.position = c(0.15,0.8),
        axis.text.x = element_blank()) +
  labs(y = NULL,x = NULL)

ggrep

ggsave(filename = paste0(outpath,"figures/","08_repliedtoshare.pdf"),
       ggrep,
       width = 9,
       height = 6)


ggth<-
  ggplot(sharedf %>% filter((rt_thread+no_rt_thread) >= 10)) + 
  aes(x = 1,y = rt_perc_thread,
      col = user_screen_name %in% maingroup) +
  geom_quasirandom(alpha = 0.5)+
  #geom_beeswarm(alpha = 0.5,priority='density')+
  scale_color_manual(name = "Account",
                     values = c("grey","red"),
                     labels = c("Reference Group",
                                "Inauthentic Network")) +
  geom_text_repel(data = function(x) x %>% 
                    filter( user_screen_name %in% maingroup,
                            rt_perc_thread > 0.5),
                  aes(label = user_screen_name),direction = "both",force = 1)+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits = c(0,1),labels = scales::percent_format(suffix = "")) +
  theme_minimal() + 
  theme(legend.position = c(0.15,0.8),
        axis.text.x = element_blank()) +
  labs(y = NULL,x = NULL)

ggth

ggsave(filename = paste0(outpath,"figures/","05_thread_rt.pdf"),
       ggth,
       width = 9,
       height = 6)

ggnoth<-
  ggplot(sharedf %>% filter((rt_nothread+no_rt_nothread) >= 25)) + 
  aes(x = 1,y = rt_perc_nothread,
      col = user_screen_name %in% maingroup) +
  geom_quasirandom(alpha = 0.5)+
  #geom_beeswarm(alpha = 0.5,priority='density')+
  scale_color_manual(name = "Account",
                     values = c("grey","red"),
                     labels = c("Reference Group",
                                "Inauthentic Network")) +
  geom_text_repel(data = function(x) x %>% 
                    filter( user_screen_name %in% maingroup,
                            rt_perc_nothread > 0.75),
                  aes(label = user_screen_name),direction = "both",force = 1)+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits = c(0,1),labels = scales::percent_format(suffix = "")) +
  theme_minimal() + 
  theme(legend.position = c(0.15,0.8),
        axis.text.x = element_blank()) +
  labs(y = NULL,x = NULL)

ggnoth

ggsave(filename = paste0(outpath,"figures/","07_nothread_rt.pdf"),
       ggnoth,
       width = 9,
       height = 6)



################################################################

foruk1 <- 
  df_sub %>% 
  filter(!is.na(retweet_dip)|!is.na(reply_dip)) %>%
  filter(!is.na(target)) %>%
  left_join(uktweets %>% select(tweet_id,is_thread),
            by = c("retweet_id" = "tweet_id")) %>% 
  group_by(user_screen_name) %>%
  mutate(n_user = n()) %>%
  #filter(n_user >= 100) %>%
  mutate(minday = min(day),
         maxday = max(day)) %>%
  mutate(duration = as.numeric(maxday-minday)) %>%
  group_by(user_screen_name,
           user_id,
           final_status,
           user_created_at,
           n_user,
           duration,
           maxday,
           minday,
           uk = target %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina"),
           what
  )%>%
  summarise(n = n()) %>%
  pivot_wider(names_from = what,
              values_from = n)

foruk2 <- 
  foruk1 %>%
  group_by(user_screen_name) %>%
#  mutate(retweet = ifelse(is.na(retweet),0,retweet),
 #        reply = ifelse(is.na(reply),0,reply)) %>%
  mutate(perc_rep_uk = reply/sum(reply,na.rm = T),
         perc_rt_uk = retweet/sum(retweet,na.rm = T) ) %>%
 filter(uk == TRUE) %>% 
  arrange(desc(perc_rt_uk)) %>%
  ungroup() %>%
  mutate(n_uk = rowSums(select(.,retweet,reply),na.rm = T))

foruk <- 
  foruk2 %>% 
  filter(n_user > 25 & perc_rt_uk > 0.5| (user_screen_name %in% maingroup)) 


#####################


#######################


load("data/uk/sum_matched3.rdata")

out <-
  data.frame(docvars(user_subset) ,
             before = sum_matched %>% unlist(),
             after = sum_matched_after %>% unlist(),
             before_cell = sum_matched_celluser %>% unlist(),
             after_cell = sum_matched_after_celluser %>% unlist(),
             map_chr(ngramlist,~paste0(.x,collapse = ";"))) %>%
  mutate(incell = ifelse(user_screen_name %in% celllist$user_screen_name,"cell","nocell"))%>% 
  group_by(incell) %>%
  mutate (mean_cellgroup = mean(before > 0 | after > 0)) %>%
  group_by(user_screen_name,mean_cellgroup) %>%
  summarise(
    avb = mean(before),
    ava = mean(after),
    m_any = mean(before > 0 | after > 0),
    sum_any = sum(before > 0 | after > 0),
    mb =mean(before > 0),
    ma =  mean(after > 0),
    n = n()
  )  

########################

tab <-
  celllist %>% 
  select(user_screen_name,x1) %>%
  left_join(sharedf %>% select(user_screen_name,
                               rt_perc_thread,
                               rt_perc_nothread,
                               rep_perc_nothread),
            by = c("user_screen_name")) %>%
  left_join(all_lags %>% ungroup() %>%select(user_screen_name,medlagdif),
            by = c("user_screen_name")) %>% 
  left_join(foruk,
            by = c("user_screen_name")) %>%
  left_join(out %>% select(user_screen_name,m_any),
            by = c("user_screen_name"))


tab %>% 
  mutate(rt_thread_thresh = rt_perc_thread > 0.15,
         rt_nothread_thresh = rt_perc_nothread > 0.25,
         rep_nothread_thresh = rep_perc_nothread > 0.15,
         m_any_thresh = m_any > 0.25) %>%
  mutate(fulfil = rowSums(select(.,rt_thread_thresh,rt_nothread_thresh,rep_nothread_thresh,m_any_thresh),na.rm = T)) %>%
  # filter(fulfil >= 1) %>% 
  View()


#######################

# 2.1. 


crea1 <- 
  tab %>% 
  filter(format(floor_date(user_created_at),"%Y-%m-%d") %in% c("2020-08-26","2020-08-11","2020-08-04","2020-04-23","2020-04-21")) %>%
  mutate(handle = paste0("@",user_screen_name) %>% cell_spec(.,format = "html",bold = T)) %>%
  mutate(reply = ifelse(is.na(reply),0,reply)) %>%
  select(handle,user_created_at,final_status,retweet,reply) %>% 
  arrange(user_created_at) %>%
  mutate(user_created_at = format(user_created_at,"%Y-%m-%d %H:%M"))
colnames(crea1) <- c("Handle","Created","Account Status 01-03-2021","Retweets*","Replies")

library(kableExtra)
crea1 %>%
  kbl(escape = FALSE) %>%
  save_kable(file = paste0(outpath,"tables/","01_crea_table.html"), self_contained = T)


###

## started aug 12/13

aug1213 <-
  tab %>% 
  filter(as.character(minday) %in% c("2020-08-12","2020-08-13"))

cumulative_uk1 <- 
  uktarg %>% 
  filter(!is.na(target)) %>%
  arrange(tweet_created_at) %>%
  mutate(day = floor_date(tweet_created_at,"day"))%>%
  group_by(user_screen_name) %>%
  mutate(n_total = n())%>% 
  filter(user_screen_name %in% maingroup) 

cumulative_uk <- 
  left_join(cumulative_uk1,
            cumulative_uk1 %>%
              ungroup() %>%
              select(user_id,n_total) %>% 
              distinct() %>% 
              mutate(global_rank = min_rank(desc(n_total))) ,
            by =c("user_id","n_total")) %>% 
  group_by(day,user_screen_name,user_description,user_created_at,n_total,final_status,global_rank) %>%
  summarise(n_day = n()) %>% 
  arrange(day) %>%
  group_by(user_screen_name) %>%
  mutate(cumsum = cumsum(n_day)) %>%
  mutate(label = if_else(cumsum == max(cumsum),paste0("@",user_screen_name) , NA_character_))%>% 
  mutate(min_date = min(day),
         max_date = max(day)) %>% ungroup()  %>%
  mutate(cumsum = ifelse(day == min_date,0,cumsum)) %>% 
  mutate(activelong = (max_date - min_date ) %>% as.duration() %>% as.numeric) 


library(ggrepel)
ggcu <-
  ggplot(cumulative_uk %>%
           filter(user_screen_name %in% aug1213$user_screen_name), 
         aes(x = day ,
             y = cumsum,
             col = final_status))+
  geom_line(aes(group = user_screen_name)) +
  geom_point(data = function(x) x %>% group_by(user_screen_name) %>%
               filter(cumsum == max(cumsum)),
             pch = 13,size = 2)+
  geom_label_repel(aes(label = label),
                   direction = "y",
                   na.rm = TRUE,max.iter = 5000,force = 1.5,
                   size =2,segment.size = 0.3)+
  scale_y_continuous(breaks = c(0,500,1000,1500),limits = c(0,1500),labels = scales::comma) +
  scale_x_datetime(date_breaks = "1 month",
                   date_labels = "%b",limits = c(as_datetime("2020-06-09"),as_datetime("2021-01-31"))) +
  scale_color_manual(name = "Account Status",
                     values = c("blue","red"),
                     labels = c("Suspended after reported to Twitter by authors on 28th of April 2021",
                                "Suspended by Twitter before March 1st 2021"))+
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        legend.key.size = unit(1,"line"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)) +
  labs(x = NULL,
       y = NULL)+
  theme(legend.position=c(.3,.8))

ggcu

ggsave(filename = paste0(outpath,"figures/","01_cumulative_aug12.pdf"),
       ggcu,
       width = 9,
       height = 4.5,dpi = 2000)


### still active old accounts here

df <- 
  readRDS("data/nogit/newtweets_uk2.rds") %>%
  filter(how_included == "main") %>%
  distinct(tweet_id,.keep_all = T) 

# Parse CT Date format correctly
df$tweet_created_at <-  lubridate::fast_strptime(df$tweet_created_at,
                                                 format = "%Y-%m-%dT%H:%M:%S.000Z",
                                                 tz = "CET",lt = F)  %>% unlist()
df$user_created_at <-  lubridate::fast_strptime(df$user_created_at,
                                                format = "%Y-%m-%dT%H:%M:%S.000Z",
                                                tz = "CET",lt = F)  %>% unlist()
df <- df %>%
  mutate(day = lubridate::floor_date(tweet_created_at %>% as_datetime(),"day"),
       week = lubridate::floor_date(tweet_created_at%>% as_datetime(),"week"))%>%
  mutate(day_created = lubridate::floor_date(user_created_at %>% as_datetime(),"day") %>% format("%Y-%m-%d"))

colnames(df) <- colnames(df) %>% str_replace_all(" ","_")

alltheirs <- 
  df %>% 
  filter(user_screen_name %in% celllist$user_screen_name)
names(alltheirs)

alltheirs %>%
  filter(!is.na(retweeted)) %>%
  group_by(user_screen_name,
           uk = str_detect(tweet_text,
                           paste0(c("@AmbLiuXiaoMing","@ChineseEmbinUK","@MahuiChina"),
                                  collapse = "|"))) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = uk,values_from = n) %>%
  mutate(`TRUE` = ifelse(is.na(`TRUE`),0,`TRUE`)) %>%
  mutate(`FALSE` = ifelse(is.na(`FALSE`),0,`FALSE`)) %>%
  mutate(perc = `TRUE`/(`TRUE`+`FALSE`)) %>% View()

alltheirs %>%
  filter(!is.na(replied_to)) %>%
  group_by(user_screen_name,
           uk = str_detect(tweet_text,
                           paste0(c("@AmbLiuXiaoMing","@ChineseEmbinUK","@MahuiChina"),
                                  collapse = "|"))) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = uk,values_from = n) %>%
  mutate(`TRUE` = ifelse(is.na(`TRUE`),0,`TRUE`)) %>%
  mutate(`FALSE` = ifelse(is.na(`FALSE`),0,`FALSE`)) %>%
  mutate(perc = `TRUE`/(`TRUE`+`FALSE`)) %>% View()


df_aug <- 
  df %>% 
  filter(user_screen_name %in% aug1213$user_screen_name) %>%
  group_by(user_screen_name,user_created_at,day) %>%
  summarise(n = n()) %>%
  full_join(expand_grid(data.frame(
    user_screen_name = aug1213 %>% filter(final_status == "active") %>% pull(user_screen_name),
    user_created_at = aug1213 %>% filter(final_status == "active") %>% pull(user_created_at) ),
                        day = seq(as_datetime("2016-11-20"),
                                 as_datetime("2021-04-17"),
                                 "1 day")),
    by = c("user_screen_name","day","user_created_at")) %>%
  mutate(n = ifelse(user_created_at < day & is.na(n),0,n)) %>%
  group_by(user_screen_name,user_created_at,week = round_date(day,"1 week")) %>%
  summarise(n = sum(n,na.rm = F)) %>%
  mutate(n_week = ifelse(user_created_at < week & is.na(n),0,n)) %>%
  mutate(act = ifelse(n_week > 0,1,0) ) %>% 
  ungroup() %>%
  arrange(desc(user_created_at)) %>% 
  mutate(act = ifelse(is.na(act),2,act)%>% as.factor())
  
ggact <-
  ggplot(df_aug,
       aes(x = week,
           y = as.character(paste0("@",user_screen_name,"\n(",user_created_at %>% format("%Y-%m-%d"),")")) %>% fct_inorder(),
           fill = act)) + 
  geom_tile()+
  labs(x = NULL,y = NULL)+
  scale_fill_manual(name = "Account Activity",
                    values = c("blue","red","grey"),
                    labels = c("Week in Which Account Was Created But Did Not Tweet",
                               "Week in Which Account Tweeted",
                               "Account Not Yet Created"))+
  theme_minimal() +
  theme(legend.position=c(.3,.25))+
  scale_x_datetime(date_breaks = "3 month",
                   date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
        legend.key = element_rect(fill = "white"),
        legend.title = element_text(face = "bold"))

ggact

ggsave(filename = paste0(outpath,"figures/","02_activity.pdf"),
       ggact,
       width = 9,
       height = 4.5,
       dpi = 2000)



###############################

#temporal 

userincluded <-
  foruk2 %>%  
  filter(n_user > 1 & perc_rt_uk > 0.25 & 
    (user_screen_name %in% maingroup | user_screen_name %in% controlgroup_uk)) %>%
  pull(user_screen_name)
  

sel1 <- 
  uktarg %>% 
  select(user_screen_name,tweet_created_at) %>%
  filter(user_screen_name %in% userincluded) %>%
  mutate(min = round_date(tweet_created_at,unit = "10 minutes")) 

sel2 <- 
  uktarg %>% 
  select(user_screen_name,tweet_created_at) %>%
  filter(user_screen_name %in% userincluded) %>%
  mutate(min = floor_date(tweet_created_at,unit = "10 minutes")) 

merged <- 
  bind_rows(
    full_join(
      sel1 %>% select(user1 =user_screen_name,min,tweet_created_at1 = tweet_created_at) %>% group_by(user1) %>% mutate(n1 = n()),
      sel1 %>% select(user2 =user_screen_name,min,tweet_created_at2 = tweet_created_at) %>% group_by(user2) %>% mutate(n2 = n()),
      by = c("min")
    ),
    full_join(
      sel2 %>% select(user1 =user_screen_name,min,tweet_created_at1 = tweet_created_at) %>% group_by(user1) %>% mutate(n1 = n()),
      sel2 %>% select(user2 =user_screen_name,min,tweet_created_at2 = tweet_created_at) %>% group_by(user2) %>% mutate(n2 = n()),
      by = c("min")
    )
  )%>%
  filter(user1 != user2) %>%
  mutate(dist = abs(as.numeric(tweet_created_at1 - tweet_created_at2))) %>%
  filter(dist <= 60) %>%
  distinct(user1,user2,tweet_created_at1,tweet_created_at2,.keep_all = T)


merged2 <- 
  merged %>%
  distinct(user1,user2,tweet_created_at1,.keep_all = T) %>%
  distinct(user1,user2,tweet_created_at2,.keep_all = T)
  

system.time({
  same <-
    merged2 %>% 
    ungroup() %>%
    #slice(1:100000) %>%
    group_by(user1,n1,user2,n2) %>%
    rowwise() %>%
    mutate(both = paste0(sort(c(user1,user2)),collapse = "--")) %>%
    distinct(tweet_created_at1,tweet_created_at2,both,.keep_all = T) %>%
    group_by(user1,n1,user2,n2,both) %>%
    summarise(n = n()) %>% 
    mutate(rate = (n/((n1+n2))))
})

same2 <-
  same %>%
  #filter(n1 >= 10,n2 >= 10) %>%
  filter(n >= 1) %>%
  ungroup() %>%
  distinct(both,.keep_all = T)


edges <- 
  same2 %>%
  #filter(as.character(month) %in% c("2020-10-01","2020-11-01")) %>%
  group_by(Source = user1,Target = user2) %>% 
  #select(Source = user_screen_name,Target = target_id, what) %>%
  summarise(Weight = sum(n,na.rm =T)) %>%
  #  group_by(Source) %>% filter(n() > 5) %>%
  write_csv("data/gephi/same_edges_1min.csv")

nodes <- 
  bind_rows(
    edges %>% ungroup() %>% 
      select(Id = Source,Label = Source) %>% mutate(type = "user") %>%
      left_join(celllist %>% select(user_screen_name,x1)%>% mutate(cell = "xx"), by = c("Id" = "user_screen_name"))%>% 
      mutate(Label = ifelse(!is.na(cell),Id,"")) %>%
      mutate(node_color = ifelse(!is.na(cell),"CELL Account","nocell")) %>%
      distinct(),
    edges %>% ungroup() %>% 
      select(Id = Target,Label = Target) %>% mutate(type = "user") %>%
      left_join(celllist %>% select(user_screen_name,x1) %>% mutate(cell = "xx"), by = c("Id" = "user_screen_name"))%>% 
      mutate(Label = ifelse(!is.na(cell),Id,"")) %>%
      mutate(node_color = ifelse(!is.na(cell),"CELL Account","nocell")) %>%
      distinct()
  ) %>%
  distinct() %>%
  write_csv("data/gephi/same_nodes_1min.csv")


############# hour plots

when <- 
  uktarg %>% 
  filter(user_screen_name %in% (celllist %>% filter(x1 %in% c("consec1")) %>% pull(user_screen_name) )) %>%
  mutate(hour = format((tweet_created_at %>% floor_date("hour")) + hours(0),"%H") %>% as.numeric,
         weekday = format(tweet_created_at %>% floor_date("day"),"%a") ) %>%
  group_by(final_status, user_screen_name,hour) %>% 
  summarise(n = n()) %>%
  full_join(expand_grid(hour =(0:23),
                        user_screen_name = unique(.$user_screen_name)),
            by = c("hour","user_screen_name")) %>% 
  mutate(n =ifelse(is.na(n),0,n))%>%
  group_by(user_screen_name) %>%
  mutate(total_n = sum(n)) %>%
  mutate(perc = n/total_n)  


ggho <- 
  ggplot(when   )  +
  geom_tile(aes(x = hour %>% as.numeric,
                y = paste0("@",user_screen_name),
                fill = perc )) +
  scale_fill_viridis_c(name = "Percent of\nTweets in Hour",
                       option = "A",labels = percent_format(suffix = "",accuracy = 1),trans = "log1p")+
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank()) +
  scale_x_continuous(breaks = c(0:23)) +
  labs(x = "Tweets Made in Hour of the Day - Time Zone: British Standard Time",
       y = NULL)+
  theme(legend.position = "right") 

ggho

ggsave(filename = paste0(outpath,"figures/","03_hours.pdf"),
       ggho,
       width = 9,
       height = 2)


#################################################################

test <- 
  uktarg %>% 
  filter(!is.na(retweet_dip)) %>%
  mutate(hour = format(tweet_created_at,"%H")) %>% 
  mutate(day = floor_date(tweet_created_at,"day")) %>% 
  filter(user_screen_name %in% (celllist %>% filter(x1 %in% c("consec1")) %>% pull(user_screen_name) )) 

test %>% 
  mutate(hour = format(tweet_created_at,"%H")) %>% 
  group_by(day,hour) %>% 
  summarise(n = length(unique(user_screen_name))) %>%
  arrange(desc(n)) %>%
  group_by(day) %>%
  mutate(nn = n()) %>%
  View()

#2020-06-15 19 uhr

zz <- 
  test %>% 
  mutate(hour = format(tweet_created_at,"%H")) %>% 
  group_by(day,hour) %>% 
  summarise(total = n(),
            n = length(unique(user_screen_name))) %>%
  arrange(desc(n)) %>%
  group_by(day) %>%
  filter(n >=5) %>%
  mutate(nn = n()) %>%
  filter(nn >= 1) %>%
  mutate(comb = paste0(as.character(day),"__",hour)) 

topl1 <- 
  test %>% 
  select(user_screen_name,tweet_created_at,day,hour,contains("lag"))

topl1$user_screen_name <- factor(topl1$user_screen_name,
                                levels = c("Xiaojin05484077",
                                           "Caterpi27848664",
                                           "ladybug23758032",
                                           "Bumbleb75459847",
                                           "Hushpup16240621"))




# topl$user_screen_name <- factor(topl$user_screen_name,
#                                 levels = c("springer000111",
#                                            "axer97964843"))

topl <-
  topl1 %>%
  group_by(user_screen_name,day,hour)%>%
  mutate(n_chain = n(),
         duration_chain = as.numeric(max(tweet_created_at) - min(tweet_created_at))) %>%
  mutate(chain_lab = paste0(n_chain," RTs\nin ",round(duration_chain),"s"))  %>%
  mutate(chain_mid = max(tweet_created_at)- seconds(0.5*duration_chain)) %>%
  group_by(day,hour) %>%
  mutate(lag_x = lag(tweet_created_at,1)) %>%
  mutate(lagdif = as.numeric(tweet_created_at - lag_x)) %>%
  mutate(lagmid = max(tweet_created_at) - seconds(0.5*lagdif)) %>%
  mutate(laguser = lag(user_screen_name,1)) %>%
  mutate(ypos = recode(user_screen_name,
                       !!! c("Xiaojin05484077" = 1,
                             "Caterpi27848664" =2,
                             "ladybug23758032" =3,
                             "Bumbleb75459847" =4,
                             "Hushpup16240621" =5))) %>%
  mutate(lag_ypos = lag(ypos,1)) %>%
 # dplyr::filter(paste0(as.character(day),"__",hour) %in% zz$comb) %>%  filter(day >= as.Date("2020-08-30"))  %>%
  filter(!(as.character(day) == "2020-09-02" & hour == 19))%>%
  filter(!(as.character(day) == "2020-09-07" & hour == 18))%>%
  filter(!(as.character(day) == "2020-09-09" & hour == 13))%>%
  filter(!(as.character(day) == "2020-09-02" & hour == 15))%>%
  filter(!(as.character(day) == "2020-09-01" & hour == 20))%>%
  filter((as.character(day) == "2020-07-22" & hour == "08")) 

library(ggrepel)

ggchain<- 
  ggplot(topl) + 
  facet_wrap(~ as.character(day) + paste0(hour,"h") ,
             scales = "free_x",ncol = 3)+ 
  geom_point(aes(x = tweet_created_at,y = user_screen_name),
             alpha = 0.5)  +
  geom_text(data = topl %>% 
              distinct(user_screen_name,chain_lab,chain_mid,hour,day),
            aes(x = chain_mid,
                y = user_screen_name,
                label = chain_lab),
            size = 3,
            #label.padding = unit(0.1, "lines"),
            nudge_y = -0.4)+
  geom_curve(data = function(df){df %>% 
      filter(user_screen_name != laguser) },
      aes(x = lag_x,xend = tweet_created_at,y = laguser,yend = user_screen_name),
      curvature = -0.3,
      linetype = "dashed",
      arrow = arrow(length = unit(0.5, "mm")),show.legend = FALSE
  ) +
  geom_label(data = function(df){df %>% 
      filter(user_screen_name != laguser) },
      aes(x = lag_x + seconds(0.5*lagdif),y = (ypos + lag_ypos)/2,label = lagdif,
          fill = "Lag Seconds (Account Switch)"),
      size = 3,nudge_x = -5
  ) +
  
  scale_fill_discrete(name = NULL)+
  labs(x = "Time",
       y = NULL) +
  # theme_minimal()+
  theme(axis.text.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        #axis.ticks.y = element_blank() ,
        legend.position = "top",
        plot.title = ggplot2::element_text(family="Roboto",
                                           size=22,
                                           face="bold",
                                           color = oxblue),
        #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
        plot.subtitle = ggplot2::element_text(family="Roboto",
                                              size=16,
                                              face = "bold",
                                              color= "#007fc8",#oxblue,
                                              margin=ggplot2::margin(0,0,5,0)),
        plot.caption = ggplot2::element_blank()) +
  expand_limits(y= c(-0.2, 0))


ggchain

ggsave(filename = paste0(outpath,"figures/","04_chain.pdf"),
       ggchain,
       width = 10,
       height = 10)


##############################

ggchain<- 
  ggplot(topl) + 
  facet_wrap(~ as.character(day) + paste0(hour,"h") ,
             scales = "free_x",ncol = 3)+ 
  geom_point(aes(x = tweet_created_at,y = user_screen_name),
             alpha = 0.5)  +
  geom_text(data = topl %>% 
              distinct(user_screen_name,chain_lab,chain_mid,hour,day),
            aes(x = chain_mid,
                y = user_screen_name,
                label = chain_lab),
            size = 3,
            #label.padding = unit(0.1, "lines"),
            nudge_y = -0.4)+
  geom_curve(data = function(df){df %>% 
      filter(user_screen_name != laguser) },
      aes(x = lag_x,xend = tweet_created_at,y = laguser,yend = user_screen_name),
      curvature = -0.3,
      linetype = "dashed",
      arrow = arrow(length = unit(0.5, "mm")),show.legend = FALSE
  ) +
  geom_label(data = function(df){df %>% 
      filter(user_screen_name != laguser) },
      aes(x = lag_x + seconds(0.5*lagdif),y = (ypos + lag_ypos)/2,label = lagdif,
          fill = "Lag Seconds (Account Switch)"),
      size = 3,nudge_x = -5
  ) +
  
  scale_fill_discrete(name = NULL)+
  labs(x = "Time",
       y = NULL) +
  # theme_minimal()+
  theme(axis.text.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        #axis.ticks.y = element_blank() ,
        legend.position = "top",
        plot.title = ggplot2::element_text(family="Roboto",
                                           size=22,
                                           face="bold",
                                           color = oxblue),
        #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
        plot.subtitle = ggplot2::element_text(family="Roboto",
                                              size=16,
                                              face = "bold",
                                              color= "#007fc8",#oxblue,
                                              margin=ggplot2::margin(0,0,5,0)),
        plot.caption = ggplot2::element_blank()) +
  expand_limits(y= c(-0.2, 0))


ggchain

ggsave(filename = paste0(outpath,"figures/","04_chain.pdf"),
       ggchain,
       width = 10,
       height = 10)





###########################

rem_manual <- c(#"said",
  #month.abb %>% str_to_lower(),
  #month.name %>% str_to_lower(),
  #c("mon","tue","wed","thu","fri","sat","sun"),
  #c("monday","tuesday","wednesday","thursday","friday","saturday","sunday"),
)


# ambassador old
uk_dips_hist <- 
  readRDS("data/nogit/newtweets_uk_dips.rds") %>%
  filter(user_screen_name %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina")) %>%
  filter(str_sub(tweet_text,1,4) != "RT @") %>%
  select(tweet_id,user_screen_name,tweet_created_at,tweet_text)  %>%
  distinct(tweet_id,.keep_all = T)

uk_dips_hist$tweet_created_at <-  lubridate::fast_strptime(uk_dips_hist$tweet_created_at,
                                                           format = "%Y-%m-%dT%H:%M:%S.000Z",
                                                           tz = "CET",lt = F)  %>% unlist()
users <- c(maingroup,controlgroup) %>% unique

replies <-
  uktarg %>%
  filter(!is.na(reply_dip)) %>%
  filter(user_screen_name %in% users) %>%
  filter(!user_screen_name %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina")) %>%
  mutate(full_text = gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "", full_text)) %>%
  mutate(full_text = gsub("@[A-z,0-9]+", "", full_text)) %>%
  #filter(!str_detect(full_text,"/")) %>%
  #filter(!str_detect(full_text,"RT")) %>%
  mutate(tweet_text  = full_text %>% str_remove_all("@AmbLiuXiaoMing|@ChineseEmbinUK|@MahuiChina"))

users_who_replied <- replies$user_screen_name %>% unique

comb <- bind_rows(
  uk_dips_hist %>%   select(tweet_id,
                            text = tweet_text,
                            user_screen_name,
                            tweet_created_at),
  replies %>%   select(tweet_id,
                       text = tweet_text,
                       user_screen_name,
                       tweet_created_at)
) %>%   mutate(text = text %>% str_to_lower())




comb$day_chr <- round_date(comb$tweet_created_at,"1 day") %>% format("%Y-%m-%d")
comb$day <- round_date(comb$tweet_created_at,"1 day") 


dip_corp <- quanteda::corpus(comb  %>%
                               #sample_n(10000) %>%
                               select(tweet_id,
                                      text ,
                                      user_screen_name,
                                      tweet_created_at,
                                      day,
                                      day_chr)
)

hunspell::dictionary(lang = "en_US")


english_words <- 
  readLines("C:/Users/ms/Documents/R/win-library/4.0/hunspell/dict/en_US.dic") %>% 
  # the vector contains extra information on the words, which is removed
  gsub("/.+", "", .)

diptokens <- 
  tokens(dip_corp,
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
  tokens_keep(c(english_words,"is"), valuetype = "fixed") %>% 
  tokens_replace(x = .,pattern = "U.S",replacement = "US") %>% 
  tokens_tolower(x = ., keep_acronyms = T) %>%
  tokens_select(x = ., 
                pattern = c(#,stopwords("en")
                  rem_manual), 
                selection = "remove") %>%
  tokens_remove(., pattern = "[\\w\\d]+\\.[\\w\\d]+", valuetype = "regex") %>%
  tokens_keep(., min_nchar = 1)


diptokens_stemmed <- 
  diptokens %>% 
  # tokens_wordstem() %>%
  tokens_ngrams(n = c(6))


dipcorpus_dfm_ngram <- dfm(diptokens_stemmed,
                           tolower = F,
                           stem =  F,
                           verbose = TRUE
)

dipcorpus_dfm_ngram_trimmed_bygroup <- 
  dipcorpus_dfm_ngram %>% 
  dfm_group(groups = "user_screen_name") %>%
  dfm_trim(.,
           #min_termfreq = 5, 
           #max_termfreq = 0.9,
           #termfreq_type = "count",
           min_docfreq = 5,
           #max_docfreq = 0.9, 
           docfreq_type = "count",
           verbose = T) 


dipcorpus_dfm_ngram_trimmed <-
  dipcorpus_dfm_ngram %>%
  dfm_select(pattern = colnames(dipcorpus_dfm_ngram_trimmed_bygroup),
             valuetype = "fixed") 

colnames(dipcorpus_dfm_ngram_trimmed)

stat_2grams <- textstat_frequency(dipcorpus_dfm_ngram_trimmed, n = 50000)


###### 

dip_subset <- 
  dipcorpus_dfm_ngram_trimmed %>%
  dfm_subset(user_screen_name %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina"))

user_subset <- 
  dipcorpus_dfm_ngram_trimmed %>%
  dfm_subset(!user_screen_name %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina"))

cell_subset <- 
  dipcorpus_dfm_ngram_trimmed %>%
  dfm_subset(user_screen_name %in% maingroup)

#####################################################################


conv <-
  dipcorpus_dfm_ngram_trimmed %>%
  dfm_group(groups = c("user_screen_name"))

long1 <- 
  convert(conv,
          to = "data.frame") %>% 
  gather(word,n,-doc_id) %>%
 # filter(n > 0) %>%
  group_by(word) %>%
  mutate(n_user_per_word = sum(n > 0)) %>% 
  mutate(ngram = str_count(word,"_")+1) %>% 
  group_by(user_screen_name = doc_id) %>%
  mutate(n_user = sum(n)) %>% 
  ungroup() %>%
  filter(ngram %in% c(6)) %>%
  filter(n_user_per_word >= 5) %>%
  mutate(userlabel = ifelse(user_screen_name %in% c(celllist$user_screen_name,c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina")), paste0("@",user_screen_name),"") ) 


long1$type <- "other"
long1$type[long1$user_screen_name %in% celllist$user_screen_name] <- "cell"
long1$type[long1$user_screen_name %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina")] <- "dips"


long1b <- long1

match_df <-
  map(long1b$word %>% unique,
          .f = ~ data.frame(word = .x,
                            sub13 = str_split(.x,"_")%>% .[[1]] %>% .[1:4]  %>% paste0(.,collapse = "_"),
                            sub24 = str_split(.x,"_")%>% .[[1]] %>% .[2:5]  %>% paste0(.,collapse = "_"),
                            sub35 = str_split(.x,"_")%>% .[[1]] %>% .[3:6]  %>% paste0(.,collapse = "_"))) %>%
  bind_rows()

x1 <- 
  full_join(match_df %>% select(word,sub13),
            match_df %>% select(word2 = word,sub24),
            by = c("sub13" = "sub24")) %>%
  left_join(.,
            match_df %>% select(word3 = word,sub35),
            by = c("sub13" = "sub35")) %>%
  gather(w,v,word:word3,-sub13) %>% 
  select(-w) %>% filter(!is.na(v)) %>%
  group_by(v) %>% 
  mutate(n_v = n()) %>% arrange(n_v) %>%
  distinct(v,.keep_all = T)


long1c <- 
  long1b %>%
  filter(type != "dips") %>%
  group_by(cell = user_screen_name %in% celllist$user_screen_name,
           word) %>%
  summarise(word_per_cell = sum(n>0))%>%
  group_by(word) %>%
  mutate(perc = word_per_cell / sum(word_per_cell)) %>%
  filter(cell == T) %>%
  filter(perc >= 0.75) 

long1c$word %>% recode(!!!vec)

vec <- 
  c("a_community_with_a_future_for" = "a community with a shared future for mankind",
    "a_global_community_of_health_for"  = "a global community of health for all",
    "and_the_rest_of_the_world"   = "china and the rest of the world",
    "and_will_continue_to_do_so"   = "and will continue to do so",
    "china_and_the_rest_of_the"    = "china and the rest of the world",
    "community_with_a_future_for_mankind"     = "a community with a shared future for mankind",
    "friend_in_need_is_a_friend"            = "a friend in need is a friend indeed",
    "from_cooperation_and_lose_from_confrontation" = "gain from cooperation and lose from confrontation",
    "gain_from_cooperation_and_lose_from"         = "gain from cooperation and lose from confrontation",
    "global_community_of_health_for_all"         = "a global community of health for all",
    "in_need_is_a_friend_indeed"               = "a friend in need is a friend indeed",
    "a_friend_in_need_is_a"               = "a friend in need is a friend indeed",
    "in_the_joint_declaration_the_uk"         = "in the joint declaration the uk",
    "is_in_the_interest_of_both"              = "is in the interest of both",
    "is_safe_until_everyone_is_safe"    = "no one is safe until everyone is safe",
    "national_security_law_for_hong_kong" = "national security law for hong kong",
    "no_one_is_above_the_law"             = "no one is above the law",
    "no_one_is_safe_until_everyone"        = "no one is safe until everyone is safe",
    "one_is_safe_until_everyone_is"        = "no one is safe until everyone is safe",
    "people_of_all_ethnic_in_xinjiang"    = "people of all ethnic in xinjang",
    "plenary_session_of_the_central_committee" = "plenary session of the central committee",
    "still_a_long_way_to_go"                 = "still a long way to go",
    "the_global_initiative_on_data_security"    = "the global initiative on data security",
    "the_national_security_law_for_hong"      = "national security law for hong kong",
    "this_year_marks_the_anniversary_of"      = "this year marks the global anniversary of",
    "to_the_global_fight_against_the"        = "the global fight against the",
    "will_play_an_important_role_in"          = "will play an important role in",
    "year_marks_the_anniversary_of_the"   = "this year marks the global anniversary of")



#table(long1$type)




long <- 
  long1 %>% 
  filter(n > 0) %>%
 # filter(word %in% long1c$word) %>% 
  mutate(word = recode(word, !!!vec)) %>% 
 # mutate(word = ifelse(str_detect(word,"_"),"",word)) %>%
  distinct(user_screen_name,word, .keep_all = T) 

unique(long$word)

unique_7 <- long %>% filter(n_user_per_word >= 6) %>% pull(word) %>% .[!str_detect(.,"_")] %>% unique()


long <- long %>% filter(word %in% c(unique_7,"is in the interest of both","the global initiative on data security","no one is above the law"))

edges <- 
  long %>%
  mutate(usedit = ifelse(n>0,1,0)) %>%
  select(Source = user_screen_name,Target = word,
         Weight = usedit,
         whofrom = type) %>%
  write_csv("data/gephi/ngram_edges_final3e.csv")

nodes <- 
  bind_rows(
    long %>% select(Id = word,Label = word,ngram,n_user = n_user_per_word,type) %>% mutate(what = "word",type = "word") %>% distinct() %>% mutate(ngram = 0,Labelsize = 10) %>%
      mutate(Label = ifelse( Id %in% c(unique_7,"is in the interest of both","the global initiative on data security","no one is above the law"),Label,"")),
    long1 %>% select(Id = user_screen_name,Label = userlabel,
                     n_user,type) %>% 
      mutate(what = "user",ngram =  ifelse(type == "cell" | Id %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina"),10,5)) %>% distinct()  %>% 
      mutate(Label = ifelse(Id %in% c(maingroup,"AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina"),Label,""),
             Labelsize = ifelse(Id %in% c(maingroup,"AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina"),5,0))
  ) %>%
  distinct(Id, .keep_all = T) %>%
  write_csv("data/gephi/ngram_nodes_final3e.csv")

#################################

## share reply with ngram
library(ggbeeswarm)

out %>% 
  #filter(n > 10) %>%
  filter(user_screen_name %in% maingroup) %>% 
  View()

ggre<-
  ggplot(out %>% filter(n >= 5)) +
  aes(x = 1,y = m_any,
      col = user_screen_name %in% maingroup) +
  geom_quasirandom(alpha = 0.5)+
  #geom_beeswarm(alpha = 0.5,priority='density')+
  scale_color_manual(name = "Account",
                     values = c("grey","red"),
                     labels = c("Reference Group",
                                "Inauthentic Network")) +
  geom_text_repel(data = function(x) x %>% 
                    filter( user_screen_name %in% maingroup,
                            m_any > 0.5),
                  aes(label = user_screen_name),direction = "both",force = 1)+
  scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1),limits = c(0,1),labels = scales::percent_format(suffix = "")) +
  theme_minimal() + 
  theme(legend.position = c(0.2,0.85),
        axis.text.x = element_blank()) +
  labs(y = NULL,x = NULL)


ggsave(filename = paste0(outpath,"figures/","06_rep_overlap.pdf"),
       ggre,
       width = 9,
       height = 6)





## did reply-quote-tweeting

qtrep <- 
  df_sub %>%
  filter(quoted_dip %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina")|
           reply_dip %in% c("AmbLiuXiaoMing","ChineseEmbinUK","MahuiChina")) %>% 
  filter((!is.na(in_reply_to_status_id) & is.na(quoted_id))|
           (is.na(in_reply_to_status_id) & !is.na(quoted_id))) %>%
  mutate(at_id = ifelse(!is.na(in_reply_to_status_id) & is.na(quoted_id),
                        in_reply_to_status_id,
                        quoted_id))

q2 <- 
  qtrep %>%
  group_by(user_screen_name,at_id) %>%
  mutate(n = n()) %>%
  filter(n >= 2) %>% 
  mutate(cleaned_text = full_text %>% str_remove_all("@[A-z0-9]*\\s")) %>%
  group_by(user_screen_name,at_id) %>%
  distinct(user_screen_name,in_reply_to_status_id,quoted_id,.keep_all = T) %>%
  distinct(user_screen_name,in_reply_to_status_id,quoted_id,cleaned_text,.keep_all = T) %>%
  group_by(user_screen_name,at_id,cleaned_text) %>%
  mutate(n = n()) %>%
  filter(n >= 2) %>% 
  arrange(cleaned_text) 

q3 <- 
  q2 %>%
  arrange(at_id) %>% 
  ungroup()%>%
  filter(user_screen_name %in% unique(ukrts$user_screen_name)) %>%
  filter(!user_screen_name %in% c("UyghurAli1","HawkishLeft","Sciiamchiamens1","JiJingPong1",#"epicchina1",
                                  "MIDO39163419","AOnewith","fierce_now")) %>%
  arrange(at_id,user_screen_name,tweet_created_at)%>%
  left_join(df_sub %>% select(tweet_id,at = user_screen_name, original_tweet = full_text),
            by = c("at_id" = "tweet_id")) %>%
  mutate(type_at = ifelse(!is.na(in_reply_to_status_id),"reply","quote-tweet")) %>%
  select(user_screen_name,final_status,tweet_created_at,
         user_created_at,type_at,cleaned_text,at,original_tweet,in_reply_to_status_id,quoted_id,at_id)  %>% 
  mutate_at(vars(contains("created_at")),funs(as.character(.)))%>%
  arrange(at_id,user_screen_name) %>% 
  group_by(at_id) %>%
  mutate(howoften = n()/2) %>%
  #filter(howoften >= 5) %>%
  filter(!user_screen_name %in% c("RealNews3362"))

q3$user_screen_name %>% unique() %>% .[. %in% maingroup]
q3$user_screen_name %>% unique() %>% .[!. %in% maingroup]



######################################################

tab2 <-
  celllist %>% 
  left_join(sharedf %>% select(user_screen_name,
                               rt_perc_thread,
                               rt_perc_nothread,
                               rep_perc_nothread),
            by = c("user_screen_name")) %>%
  left_join(all_lags %>% ungroup() %>%select(user_screen_name,medlagdif),
            by = c("user_screen_name")) %>% 
  left_join(foruk,
            by = c("user_screen_name")) %>%
  left_join(out %>% select(user_screen_name,m_any),
            by = c("user_screen_name")) %>%
  mutate(perc_uk = round(100*n_uk/n_user)) %>%
  mutate(rt_perc_thread = round(100*rt_perc_thread)) %>%
  mutate(rt_perc_nothread = round(100*rt_perc_nothread)) %>%
  mutate(medlagdif = ifelse(medlagdif > 3600,">3600",medlagdif)) %>%
  mutate(user_created_at = user_created_at %>% format("%Y-%m-%d %H:%M")) %>%
  mutate(rep_perc_nothread = round(100*rep_perc_nothread)) %>%
  mutate(m_any = round(100*m_any)) %>%
  mutate(lang = ifelse(user_screen_name %in% (celllist %>% filter(x2 == "lang") %>% pull(user_screen_name)),
                       "LANG","")) %>%
  mutate(qrep = ifelse(user_screen_name %in% (q3$user_screen_name),
                       "QREP","")) %>%
  mutate(user_screen_name = paste0("@",user_screen_name)) %>%
  select(user_screen_name,
         user_created_at,
         final_status,
         n_user,
         perc_uk,
         retweet,
         rt_perc_nothread,
         rt_perc_thread,
         medlagdif,
         consec = x1,
         reply,
         rep_perc_nothread,
         m_any,
         lang,
         qrep) %>% 
  mutate(other = "") 

colnames(tab2) <- 
  c("Handle","Created","Status 1 March","Times Amplified PRC Diplomat",
    "Share Dedicated to UK-based (in%)","UK-based Dip. Retweets","Share Main Tweets Retweeted (in %)**",
    "Share Thread Tweets Retweeted** (in %)",
    "Med. Lag between Tweets(in s)","Temporal Patterns","UK-based Dip. Replies","Share Replied to (in %)" ,
    "Share Overlapping Language (in %)",
    "Language Patters","Verbatim Quote-Replying", "Other")

writexl::write_xlsx(tab2,"data_exchange/AP01_63users.xlsx")


#######################
#  cell_spec(.,format = "html",bold = T))
# 2.1. 

library(kableExtra)
tab2 %>%
  kbl(escape = FALSE) %>%
  save_kable(file = paste0(outpath,"tables/","02_all_table.html"), self_contained = T)

########

## impact


by_guys <- 
  df_sub %>% 
#  filter(final_status == "deleted") %>%
  mutate(final_status = ifelse(final_status == "deleted","suspended",final_status)) %>%
  mutate(cellstatus = ifelse(user_screen_name %in% celllist$user_screen_name,
                             "cell","nocell")) %>%
  mutate(comb_status = paste0(cellstatus,"-",final_status)) %>%
  filter(what == "retweet") %>%
  filter(target %in% c("AmbLiuXiaoMing"
                       ,"ChineseEmbinUK"#,"MahuiChina"
  )) %>%
  group_by(comb_status,
           target,
           what,
           week
  ) %>%
  summarise(n = n()) %>%
  group_by(target,week) %>%
  mutate(n_user = sum(n)) %>%
  mutate(perc = n/n_user) %>%
  #filter(!is.na(cell)) %>% 
  arrange(desc(perc)) %>%
   full_join(expand_grid(target = unique(.$target),
                        week = unique(.$week),
                        what = "retweet",
                        comb_status = unique(.$comb_status)),
            by = c("target","week","what","comb_status")) %>% 
  mutate(n =ifelse(is.na(n),0,n),
       #  n_user =ifelse(is.na(n_user),0,n_user)
         perc =ifelse(is.na(perc),0,perc)) %>% 
  mutate(comb_status = ifelse(comb_status == "nocell-suspended","nocell-suspended_deleted",comb_status)) %>% 
  mutate(comb_status = ifelse(comb_status == "cell-suspended","cell-suspended_deleted",comb_status)) %>% 
  mutate(comb_status = factor(comb_status,levels = c("nocell-active","nocell-suspended_deleted","cell-suspended_deleted","cell-active"))) %>%
  filter(what == "retweet") %>%
  arrange(target,week)

writexl::write_xlsx(by_guys,"data_exchange/AP_02_weekly_share_retweets_final_62.xlsx")

by_guys %>% 
  filter(target == "AmbLiuXiaoMing") %>%
  filter(comb_status == "cell-active") %>%
  ungroup()%>%
  summarise(n = sum(n))

#####################

erikaaa <- 
  df_sub %>% 
  #  filter(final_status == "deleted") %>%
  mutate(final_status = ifelse(final_status == "deleted","suspended",final_status)) %>%
  mutate(cellstatus = ifelse(user_screen_name %in% celllist$user_screen_name,
                             "cell","nocell")) %>%
  mutate(comb_status = paste0(cellstatus,"-",final_status)) %>%
  filter(what == "retweet") %>%
  filter(target %in% c("AmbLiuXiaoMing"
                       ,"ChineseEmbinUK"#,"MahuiChina"
  )) %>%
  group_by(network62 = cellstatus,
           status_march1 = final_status,
           target,
           what,
  ) %>%
  summarise(n = n()) %>%
  group_by(target) %>%
  mutate(n_user = sum(n)) %>%
  mutate(perc = n/n_user) 

writexl::write_xlsx(erikaaa,"data_exchange/liu_tweets_from_where_final62.xlsx")




## raw data for ap

ap_split <- 
  df_sub %>% 
  filter(target %in% c("AmbLiuXiaoMing"
                       ,"ChineseEmbinUK"#,"MahuiChina"
  )) %>%
  filter(what == "retweet") %>% 
  select(tweet_created_at,user_screen_name,final_status,full_text,target,what) %>%
  arrange(tweet_created_at) %>%
  group_split(target,
              network = user_screen_name %in% celllist$user_screen_name) %>%
  set_names(c("Amb_other","Amb_network","Emb_other","Emb_network"))

writexl::write_xlsx(ap_split,"data_exchange/AP03_split_retweets_by_ambemb_networkstatus.xlsx")


scale_color_manual(name = "Account Status",
                   values = c("blue","red"),
                   labels = c("Suspended after reported to Twitter by authors on 28th of April 2021",
                              "Suspended by Twitter before March 1st 2021"))

ggsh<-
  ggplot(by_guys %>% filter(target == "AmbLiuXiaoMing"),
         aes(x = week,
             y = perc,
             alpha = comb_status,
             fill = comb_status)) +
  geom_area(pos = "stack")+
 # facet_wrap(~target,ncol = 1,scales = "free") +
  scale_fill_manual(name = NULL,
                    values = c("nocell-active" = "grey","nocell-suspended_deleted" = "salmon", 
                               "cell-suspended_deleted" = "red", "cell-active" = "blue"),
                    labels = c("No Attribution - Still active",
                               "No Attribution - Suspended/Deleted",
                               "Inauthentic Network - Suspended by Twitter before 1st of March",
                               "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  scale_color_manual(name =NULL,
                    values = c("nocell-active" = "grey","nocell-suspended_deleted" = "salmon", 
                               "cell-suspended_deleted" = "red", "cell-active" = "blue"),
                    labels = c("No Attribution - Still active",
                               "No Attribution - Suspended/Deleted",
                               "Inauthentic Network - Suspended by Twitter before 1st of March",
                               "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  scale_alpha_manual(name = NULL,
                    values = c("nocell-active" = 0.5,"nocell-suspended_deleted" = 0.5, 
                               "cell-suspended_deleted" = 1, "cell-active" = 1),
                    labels = c("No Attribution - Still active",
                               "No Attribution - Suspended/Deleted",
                               "Inauthentic Network - Suspended by Twitter before 1st of March",
                               "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  labs(x = NULL,
       y = NULL)+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %Y")+
  geom_hline(yintercept = 0.5,linetype = "dashed",alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(suffix = "")) +
  theme_minimal()+
  theme(legend.position = c(0.3,0.85),
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

ggsh

# ggsave(filename = paste0("xx_impact_retweets.pdf"),
#        ggsh,
#        width = 10,
#        height = 10)

ggsave(filename = paste0(outpath,"figures/","xx_impact_replies.pdf"),
       ggsh,
       width = 10,
       height = 6)



#######

ggsh<-
  ggplot(by_guys %>% filter(target == "ChineseEmbinUK"),
         aes(x = week,
             y = perc,
             alpha = comb_status,
             fill = comb_status)) +
  geom_area(pos = "stack")+
  # facet_wrap(~target,ncol = 1,scales = "free") +
  scale_fill_manual(name = NULL,
                    values = c("nocell-active" = "grey","nocell-suspended_deleted" = "salmon", 
                               "cell-suspended_deleted" = "red", "cell-active" = "blue"),
                    labels = c("No Attribution - Still active",
                               "No Attribution - Suspended/Deleted",
                               "Inauthentic Network - Suspended by Twitter before 1st of March",
                               "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  scale_color_manual(name =NULL,
                     values = c("nocell-active" = "grey","nocell-suspended_deleted" = "salmon", 
                                "cell-suspended_deleted" = "red", "cell-active" = "blue"),
                     labels = c("No Attribution - Still active",
                                "No Attribution - Suspended/Deleted",
                                "Inauthentic Network - Suspended by Twitter before 1st of March",
                                "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  scale_alpha_manual(name = NULL,
                     values = c("nocell-active" = 0.5,"nocell-suspended_deleted" = 0.5, 
                                "cell-suspended_deleted" = 1, "cell-active" = 1),
                     labels = c("No Attribution - Still active",
                                "No Attribution - Suspended/Deleted",
                                "Inauthentic Network - Suspended by Twitter before 1st of March",
                                "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  labs(x = NULL,
       y = NULL)+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %Y")+
  geom_hline(yintercept = 0.5,linetype = "dashed",alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(suffix = "")) +
  theme_minimal()+
  theme(legend.position = c(0.3,0.85),
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

ggsh

# ggsave(filename = paste0("xx_impact_retweets.pdf"),
#        ggsh,
#        width = 10,
#        height = 10)

ggsave(filename = paste0(outpath,"figures/","xx_impact_replies_embassy.pdf"),
       ggsh,
       width = 10,
       height = 6)














##### replies:

by_guys_replies <- 
  df_sub %>% 
  #  filter(final_status == "deleted") %>%
  mutate(final_status = ifelse(final_status == "deleted","suspended",final_status)) %>%
  mutate(cellstatus = ifelse(user_screen_name %in% celllist$user_screen_name,
                             "cell","nocell")) %>%
  mutate(comb_status = paste0(cellstatus,"-",final_status)) %>%
  filter(what == "reply") %>%
  filter(target %in% c("AmbLiuXiaoMing"
                       ,"ChineseEmbinUK"#,"MahuiChina"
  )) %>%
  group_by(comb_status,
           target,
           what,
           week
  ) %>%
  summarise(n = n()) %>%
  group_by(target,week) %>%
  mutate(n_user = sum(n)) %>%
  mutate(perc = n/n_user) %>%
  #filter(!is.na(cell)) %>% 
  arrange(desc(perc)) %>%
  full_join(expand_grid(target = unique(.$target),
                        week = unique(.$week),
                        what = "reply",
                        comb_status = unique(.$comb_status)),
            by = c("target","week","what","comb_status")) %>% 
  mutate(n =ifelse(is.na(n),0,n),
         #  n_user =ifelse(is.na(n_user),0,n_user)
         perc =ifelse(is.na(perc),0,perc)) %>% 
  mutate(comb_status = ifelse(comb_status == "nocell-suspended","nocell-suspended_deleted",comb_status)) %>% 
  mutate(comb_status = ifelse(comb_status == "cell-suspended","cell-suspended_deleted",comb_status)) %>% 
  mutate(comb_status = factor(comb_status,levels = c("nocell-active","nocell-suspended_deleted","cell-suspended_deleted","cell-active"))) %>%
  filter(what == "reply") %>%
  arrange(target,week)

writexl::write_xlsx(by_guys_replies,"data_exchange/AP_06_weekly_share_replies.xlsx")



####################

ggsh<-
  ggplot(by_guys_replies %>% filter(target == "AmbLiuXiaoMing"),
         aes(x = week,
             y = perc,
             alpha = comb_status,
             fill = comb_status)) +
  geom_area(pos = "stack")+
  # facet_wrap(~target,ncol = 1,scales = "free") +
  scale_fill_manual(name = NULL,
                    values = c("nocell-active" = "grey","nocell-suspended_deleted" = "salmon", 
                               "cell-suspended_deleted" = "red", "cell-active" = "blue"),
                    labels = c("No Attribution - Still active",
                               "No Attribution - Suspended/Deleted",
                               "Inauthentic Network - Suspended by Twitter before 1st of March",
                               "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  scale_color_manual(name =NULL,
                     values = c("nocell-active" = "grey","nocell-suspended_deleted" = "salmon", 
                                "cell-suspended_deleted" = "red", "cell-active" = "blue"),
                     labels = c("No Attribution - Still active",
                                "No Attribution - Suspended/Deleted",
                                "Inauthentic Network - Suspended by Twitter before 1st of March",
                                "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  scale_alpha_manual(name = NULL,
                     values = c("nocell-active" = 0.5,"nocell-suspended_deleted" = 0.5, 
                                "cell-suspended_deleted" = 1, "cell-active" = 1),
                     labels = c("No Attribution - Still active",
                                "No Attribution - Suspended/Deleted",
                                "Inauthentic Network - Suspended by Twitter before 1st of March",
                                "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  labs(x = NULL,
       y = NULL)+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %Y")+
  geom_hline(yintercept = 0.5,linetype = "dashed",alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(suffix = "")) +
  theme_minimal()+
  theme(legend.position = c(0.3,0.85),
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

ggsh

# ggsave(filename = paste0("xx_impact_retweets.pdf"),
#        ggsh,
#        width = 10,
#        height = 10)

ggsave(filename = paste0(outpath,"figures/","xx_impact_replies_replies.pdf"),
       ggsh,
       width = 10,
       height = 6)



#######

ggsh<-
  ggplot(by_guys_replies %>% filter(target == "ChineseEmbinUK"),
         aes(x = week,
             y = perc,
             alpha = comb_status,
             fill = comb_status)) +
  geom_area(pos = "stack")+
  # facet_wrap(~target,ncol = 1,scales = "free") +
  scale_fill_manual(name = NULL,
                    values = c("nocell-active" = "grey","nocell-suspended_deleted" = "salmon", 
                               "cell-suspended_deleted" = "red", "cell-active" = "blue"),
                    labels = c("No Attribution - Still active",
                               "No Attribution - Suspended/Deleted",
                               "Inauthentic Network - Suspended by Twitter before 1st of March",
                               "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  scale_color_manual(name =NULL,
                     values = c("nocell-active" = "grey","nocell-suspended_deleted" = "salmon", 
                                "cell-suspended_deleted" = "red", "cell-active" = "blue"),
                     labels = c("No Attribution - Still active",
                                "No Attribution - Suspended/Deleted",
                                "Inauthentic Network - Suspended by Twitter before 1st of March",
                                "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  scale_alpha_manual(name = NULL,
                     values = c("nocell-active" = 0.5,"nocell-suspended_deleted" = 0.5, 
                                "cell-suspended_deleted" = 1, "cell-active" = 1),
                     labels = c("No Attribution - Still active",
                                "No Attribution - Suspended/Deleted",
                                "Inauthentic Network - Suspended by Twitter before 1st of March",
                                "Inauthentic Network - Suspended after reported to Twitter 28th of April")
  ) +
  labs(x = NULL,
       y = NULL)+
  scale_x_datetime(date_breaks = "1 month",date_labels = "%b %Y")+
  geom_hline(yintercept = 0.5,linetype = "dashed",alpha = 0.5) +
  scale_y_continuous(labels = scales::percent_format(suffix = "")) +
  theme_minimal()+
  theme(legend.position = c(0.3,0.85),
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

ggsh

# ggsave(filename = paste0("xx_impact_retweets.pdf"),
#        ggsh,
#        width = 10,
#        height = 10)

ggsave(filename = paste0(outpath,"figures/","xx_impact_replies_embassy_replies.pdf"),
       ggsh,
       width = 10,
       height = 6)


## raw data for ap

ap_split <- 
  df_sub %>% 
  filter(target %in% c("AmbLiuXiaoMing"
                       ,"ChineseEmbinUK"#,"MahuiChina"
  )) %>%
  filter(what == "reply") %>% 
  select(tweet_created_at,user_screen_name,final_status,full_text,target,what) %>%
  arrange(tweet_created_at) %>%
  group_split(target,
              network = user_screen_name %in% celllist$user_screen_name) %>%
  set_names(c("Amb_other","Amb_network","Emb_other","Emb_network"))

writexl::write_xlsx(ap_split,"data_exchange/AP07_split_replies_by_ambemb_networkstatus.xlsx")


