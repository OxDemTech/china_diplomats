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

library(ggthemes)
palll <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
# palll[[1]]
# palll[[1]]$value[1]

theme_fl <- function(){
  theme_minimal() +
    theme(legend.position = "none") +
    theme(axis.text.y = element_text(size = 12,color = "black",face = "bold"),
          title = element_text(color = "black",face = "bold",size = 15),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0.2,0.2,0.2,0.2,0.2), "cm"),
          axis.line.x = element_blank(),
          strip.text.x = element_text(size = 14, face = "bold", angle = 0),
          panel.grid.major.x = element_blank())
}

# Load DATa

resolved <- readRDS("data/nogit/processed/resolved_feb.rds")

df <- 
  readRDS("data/nogit/final_publication_dataset_jun.rds") %>%
  left_join(resolved %>% select(detected_url,resolved_url = resolved,resolved_outlet),
            by = c("detected_url" = "detected_url"))

df$user_handle[(df$user_handle == "" | is.na(df$user_handle)) & (!is.na(df$post_dip))] <- 
  df$post_dip[(df$user_handle == "" | is.na(df$user_handle)) & (!is.na(df$post_dip))]

df$user_handle[(df$user_handle == "" | is.na(df$user_handle)) & (!is.na(df$post_med))] <- 
  df$post_med[(df$user_handle == "" | is.na(df$user_handle)) & (!is.na(df$post_med))]


###

twitter_seedlist <- 
  readxl::read_excel("data/input_selectors/ap_targets_final.xlsx",sheet = 2) %>% 
  filter(!`A/E/C/S` %in% c("NewM","NewMP")) %>%
  select(handle  = twitter_handle,dipgroup = `A/E/C/S`,ISO2,country,lang) %>% distinct()%>% 
  filter(!is.na(handle)) %>%
  mutate(type = ifelse(dipgroup %in% c("A","E","B","S","C"),"diplomat",
                       ifelse(dipgroup %in% c("M"),"statemedia",NA))) %>%
  data.frame( .,
              platform = "twitter")  %>%
  filter(lang != "cn" | is.na(lang)) %>% 
  filter(dipgroup %in% c("A","E","B","S","C","M")) %>% 
  filter(!handle %in% c("AmbassadeurLiLi"))

fb_seedlist <- 
  readxl::read_excel("data/input_selectors/ap_targets_final.xlsx",sheet = 3) %>% 
  rename(original_url = handle) %>%
  mutate(handle = 
           original_url%>%
           paste0(.," ") %>%
           str_extract(.,"(?<=.com/).*?(?=/| |\\-[0-9])") %>% 
           str_replace_all(.,"(?=\\?).+","") %>%
           str_remove_all("/")) %>%
  mutate(fb_url = paste0("https://m.facebook.com/",handle)) %>% 
  filter(!is.na(original_url)) %>%
  select(handle,type,ISO2,country,lang) %>% distinct()%>% 
  mutate(dipgroup = type) %>%
  mutate(type = ifelse(type %in% c("A","E","B","S","C"),"diplomat",
                       ifelse(type %in% c("M","NewM"),"statemedia",NA))) %>%
  data.frame(.,
             platform = "facebook")%>%
  filter(lang != "cn" | is.na(lang)) %>% 
  filter(dipgroup %in% c("A","E","B","S","C","M","NewM"))


accs1 <- 
  bind_rows(twitter_seedlist,
            fb_seedlist)

###

m <- 
  df %>%
  select(user_handle,
         where) %>% 
  distinct() %>% 
  mutate(here = "has_posted")

accs <- 
  accs1 %>% 
  left_join(m,
            by = c("handle" = "user_handle",
                   "platform" = "where")) %>%
  filter(!is.na(here))

saveRDS(accs,"data/input_selectors/accs.rds")

accs_grp <- 
  accs %>% 
  group_by(type,platform) %>%
  summarise(n = n())


###
n_dip <- accs %>% filter(type == "diplomat") %>% nrow()
n_sbm <- accs %>% filter(type == "statemedia") %>% nrow()

##############################################################
library(rtweet)


#########################

system.time({
  users_found <- 
    lookup_users(users = twitter_seedlist %>% 
                   filter(platform == "twitter") %>% 
                   pull(handle) %>% unique(), 
                 parse = TRUE, 
                 token = ox_token)
})


merged_twitter <- 
  left_join(twitter_seedlist %>% 
              mutate(match = handle %>% 
                       str_to_lower) %>%
              filter(platform == "twitter"),
            users_found %>% mutate(match = screen_name %>% str_to_lower) %>% select(-country,-lang),
            by = "match") %>% 
  filter(!is.na(account_created_at)) %>% 
  arrange(account_created_at) %>% 
  select(1:7,account_created_at,name,match,followers_count,verified) %>% 
  group_by(type) %>%
  mutate(x = 1,
         cumsum = cumsum(x)) %>% 
  select(-x)  %>% 
  filter(handle %in% (accs %>% filter(platform == "twitter") %>% pull(handle)))


###
oldtable <- readRDS("data/nogit/for_dip_twitter_viz.rds")

merged_twitter %>%
  group_by(type,platform) %>%
  summarise(n = n())


gg1 <- 
  ggplot(merged_twitter,
         aes(x = account_created_at,
             y = cumsum,
             col = type)) +
  geom_step(size = 1.02)+
  labs(x = NULL,y = "Number of Accounts",
       title = "Cumulative Total Number of Active Twitter Accounts ",
       subtitle = "All Chinese diplomats and most relevant state media accounts on Twitter")+
  scale_y_continuous(breaks = c(seq(0,200,50)))+
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y") + 
  theme_minimal()+ 
  scale_color_manual(name = "Account Type",
                     values = c("red","blue"),
                     labels = c("Diplomat Accounts","State-Backed Media")) + 
  theme(legend.position = "right",
        plot.title = ggplot2::element_text(family="Roboto",
                                           size=22,
                                           face="bold",
                                           color = oxblue),
        #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
        plot.subtitle = ggplot2::element_text(family="Roboto",
                                              size=16,
                                              face = "bold",
                                              color= "#007fc8",#oxblue,
                                              margin=ggplot2::margin(0,0,5,0.5)),
        plot.caption = ggplot2::element_blank()) + 
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(title = NULL,
       subtitle = NULL)

gg1
##############################

row1 <- "Diplomatic Accounts who tweeted at least once between June 2020 and Mid-February 2021"
row2 <- "Diplomats include Embassy Accounts, Diplomats, Consulates, and Key Embassy Staffers"
row3 <- paste0("Included Accounts: ",
               accs%>%filter(platform == "twitter",type == "diplomat") %>%nrow,
               " Diplomatic accounts and ",
               accs%>%filter(platform == "twitter",type == "statemedia") %>%nrow,
               " State Media Accounts (See Appendix for full list)")



oii_finalise_plot(gg1, 
                  source_name = paste0(row1,"
",row2,"
",row3,""), 
                  save_filepath = "manuscript/word/000a_creation.pdf",
                  width_pixels = 1200/1.4, 
                  height_pixels = 700/1.4,  
                  height_footer = 0.081,
                  dpi = 1000,
                  scale = 1,
                  footer_start = 0.9)


browseURL("manuscript/word/000a_creation.pdf")



map <- 
  merged_twitter %>% 
  filter(handle %in% (accs %>% 
                        pull(handle))) %>%
  filter(type == "diplomat") %>% 
  ungroup() 

map$ISO2[map$dipgroup == "B"] <- "CN"

table(map$ISO2) %>% length()


map <- map%>% select(ISO2,which = dipgroup,handle)

accsmap <- accs %>% filter(type == "diplomat") %>%select(ISO2,handle) %>% mutate(which = "diplomat")

map <- bind_rows(accsmap,#map,
                            data.frame(country = "China",
                            ISO2 = "CN",
                            which = "diplomat",
                            twitter_handle = "lala"),
                 data.frame(country = "Greenland",
                            ISO2 = "GL",
                            which = "diplomat",
                            twitter_handle = "xxx"))
map %<>% 
  filter(which %in% c("diplomat")) %>%
  pivot_wider(names_from = which, 
              values_from = handle) 

map %<>% 
  mutate(diplomat = ifelse(!is.na(diplomat) ,TRUE,FALSE))

map$diplomat[map$ISO2 %in% c("CN","GL")] <- TRUE

# Thanks to the power of sf, a geom_sf nicely handles varying projections
# setting the aspect ratio correctly.
library(maps)

world1 <- 
  sf::st_as_sf(map('world', plot = FALSE, fill = TRUE)) %>%
  mutate(ISO = iso.alpha(ID))%>% 
  left_join(map,by = c("ISO" = "ISO2"))

library(extrafont)

loadfonts(device = "postscript")
loadfonts(device = "pdf")
loadfonts(device = "win")

world1$diplomat[is.na(world1$diplomat)] = FALSE

gg2 <- 
  ggplot() + 
  geom_sf(data = world1,
          aes(fill = diplomat),size = 0.1) +
  coord_sf(ylim = c(-55,85)) +
  theme_minimal() + 
  labs(x = NULL,
       y = NULL,
       title = "Countries where Chinese Diplomats are active on Twitter",
       subtitle  = paste0(accs %>% filter(type == "diplomat",platform == "twitter") %>% nrow(),
       " diplomatic Twitter accounts in ",nrow(map)," countries"))+ 
  theme(legend.position = "none",
        axis.text.x = element_blank()) +
  scale_fill_manual(name = NULL,values = c("grey","red")) + 
  theme(legend.position = "none",
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
        plot.caption = ggplot2::element_blank()) 

gg2

row1 <- "Diplomatic Accounts who tweeted at least once between June 2020 and Mid-February 2021"
row2 <- "Including Embassy Accounts, Diplomats, Consulates, and Key Embassy Staffers (Full list in Online Appendix)"
row3 <- "Ministry of Foreign Affairs Employees are Classified as based in Beijing"


oii_finalise_plot(gg2, 
                  source_name = paste0(row1,"
",row2,"
",row3,""), 
                  save_filepath = "manuscript/word/000b_map.pdf",
                  width_pixels = 1200/1.4, 
                  height_pixels = 600/1.4,  
                  height_footer = 0.081,
                  dpi = 1000,
                  scale = 1,
                  footer_start = 0.9)


browseURL("manuscript/word/000b_map.pdf")



############

d1 <- 
  df %>% 
  filter((!is.na(post_med))|
           (!is.na(post_dip))
  ) %>% 
  filter(user_handle %in% accs$handle) 

dip_posts <- 
  d1 %>%
  filter(!is.na(post_dip))


dip_posts %>% 
  filter(user_handle == "zlj517") %>% View()

med_posts <- 
  d1 %>%
  filter(!is.na(post_med))

d1b <-
  bind_rows(
    dip_posts %>%
      group_by(platform = where, is_retweeting_tweet, is_quoting_tweet,
               is_retquot = is_retweeting_tweet|is_quoting_tweet) %>%
      summarise(n = n()) %>%
      mutate(who = "diplomat"),
    med_posts %>% 
      filter(user_handle %in% (accs %>% 
                                 filter(lang == "en" & (!is.na(lang))) %>% 
                                 pull(handle)))%>%
      group_by(platform = where, is_retweeting_tweet, is_quoting_tweet,
               is_retquot = is_retweeting_tweet|is_quoting_tweet) %>%
      summarise(n = n())%>%
      mutate(who = "state_media_en"),
    med_posts%>% 
      filter(user_handle %in% (accs %>% 
                                 filter(lang != "en" & (!is.na(lang))) %>% 
                                 pull(handle))) %>%
      group_by(platform = where, is_retweeting_tweet, is_quoting_tweet,
               is_retquot = is_retweeting_tweet|is_quoting_tweet) %>%
      summarise(n = n())%>%
      mutate(who = "state_media_oth")
  ) %>% ungroup %>%
  mutate(is_ret = ifelse((is.na(is_retquot)) | is_retquot == F,
                         "N",
                         ifelse(is_retweeting_tweet == T,"R","SQ"))) %>%
  mutate(comb = paste(platform,is_ret,sep = "-")) %>%
  left_join(data.frame(comb = c("facebook-N","twitter-N","twitter-SQ","twitter-R"),
                       sublab = c("FB Posts","Tweets","Quote Tweets","Retweets"))) %>%
  mutate(sublabfinal = paste0(scales::comma(n),"\n",sublab))


d1b

d1b$xx <- NA
d1b$xx[d1b$comb == "twitter-SQ"] <- 18
d1b$xx[d1b$comb == "twitter-R"] <- 11.5
d1b$xx[d1b$comb == "twitter-N"] <- 7
d1b$xx[d1b$comb == "facebook-N"] <- 2

d1b$lab <- NA
d1b$lab[d1b$who == "state_media_en"] <- "State-Backed Media\n(English)"
d1b$lab[d1b$who == "state_media_oth"] <- "State-Backed Media\n(Other Languages)"
d1b$lab[d1b$who == "diplomat"] <- "Diplomat Accounts"

############

nrow = 20

d1b$comb <- factor(d1b$comb,
                   levels = c("facebook-N","twitter-N","twitter-R","twitter-SQ"))

gg1 <- 
  ggplot(d1b) +
  geom_pictogram(
    aes(colour = comb,label = comb ,values = n/1000),
    n_rows = nrow, size = 5,  flip = TRUE,
    family = "fontawesome-webfont"
  ) +
  scale_label_pictogram(
    name = NULL,
    values = c("facebook", "twitter", "twitter","twitter"),
    labels = c("Facebook\nPosts", "Tweets","Retweeting","Quote Tweets")
  ) +
  scale_color_manual(
    name = NULL,
    values = c(alpha("#002147",1),#"#4267B2", 
               alpha("#1DA1F2",1),
               alpha("grey",1),
               alpha("purple",0.6)), #0277bf
    labels = c("Facebook\nPosts", "Tweets","Retweeting","Quote Tweets")
  ) +
  
  #coord_equal() + 
  
  labs(x = NULL,y = NULL,
       title = "Total Activity by Chinese Diplomats and State Media Accounts",
       subtitle = "1 Icon representing 1000 posts/tweets between June 9th 2020 and February 23rd 2021")+
  scale_y_continuous(breaks = seq(0,22,2),
                     labels = paste0(scales::comma(seq(0,22*1*20,1*20*2)),""),
                     limits = c(0,22))+
  
  theme_minimal()+ 
  theme(legend.position = "right",
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
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 16,family = "Roboto")) + 
  facet_wrap(~ lab, scales = "fixed",
            # space = 'fixed',
            drop = T,strip.position="bottom") 

gg1
##############################


row1 <- "Observation Window: 9 June 2020 - 23 February 2021"
row2 <- paste0("Twitter: ",
               accs%>%filter(platform == "twitter",type == "diplomat") %>%nrow,
               " Diplomatic accounts and ",
               accs%>%filter(platform == "twitter",type == "statemedia") %>%nrow,
               " (",accs%>%filter(platform == "twitter",type == "statemedia",lang == "en") %>%nrow," EN | ",
               accs%>%filter(platform == "twitter",type == "statemedia",lang != "en") %>%nrow," oth. lang)",
               " State Media Accounts")

row3 <- paste0("Facebook: ",
               accs%>%filter(platform == "facebook",type == "diplomat") %>%nrow,
               " Diplomatic accounts and ",
               accs%>%filter(platform == "facebook",type == "statemedia") %>%nrow,
               " (",accs%>%filter(platform == "facebook",type == "statemedia",lang == "en") %>%nrow," EN | ",
               accs%>%filter(platform == "facebook",type == "statemedia",lang != "en") %>%nrow," oth. lang)",
               " State Media Accounts")


oii_finalise_plot(gg1, 
                  source_name = paste0(row1,"
",row2,"
",row3,""), 
                  save_filepath = "manuscript/word/001_posts.pdf",
                  width_pixels = 1200/1.4, 
                  height_pixels = 700/1.4,  
                  height_footer = 0.081,
                  dpi = 1000,
                  scale = 1,
                  footer_start = 0.87)


browseURL("manuscript/word/001_posts.pdf")


#### Engagement With

d2 <- d1

en_med <- 
  accs%>%filter(type == "statemedia",lang == "en") %>% 
  pull(handle)

oth_med <- 
  accs%>%filter(type == "statemedia",lang != "en") %>% 
  pull(handle)

dips <- 
  accs %>%filter(type == "diplomat") %>% 
  pull(handle)

d2$who <- NA
d2$who[!is.na(d2$post_med) & (d2$user_handle %in% en_med)] <- "med_en"
d2$who[!is.na(d2$post_med) & (d2$user_handle %in% oth_med)] <- "med_oth"
d2$who[!is.na(d2$post_dip)] <- "dip"
table(d2$who)

d2 %>% 
  distinct(where,who,user_handle) %>% 
  group_by(who,platform = where) %>% 
  summarise(n = n()) %>% ungroup 

d2b <- 
  d2 %>% 
  gather(key,val,likes,shares,replies) %>%
  group_by(who,platform = where,key) %>% 
  summarise(n = n(),
            sum_rea = sum(val,na.rm = T)) %>% 
  mutate(avg_rea = sum_rea/n)


## engagement table

d2c <- 
  d2 %>% 
  gather(key,val,likes,shares,replies) %>%
  group_by(who,platform = where,key) %>% 
  filter(is_retweeting_tweet == F | platform == "facebook") %>%
  summarise(n = n(),
            sum_rea = sum(val,na.rm = T),
            mean_rea = mean(val,na.rm = T),
            median_rea = median(val,na.rm = T)) 


d2d <- 
  d2 %>% 
  gather(key,val,likes,shares,replies) %>%
  group_by(is_retweeting_tweet,who,platform = where,key) %>% 
  summarise(n = n(),
            sum_rea = sum(val,na.rm = T),
            mean_rea = mean(val,na.rm = T))%>%
  mutate(text = paste0("\\makecell[r]{\\textbf{",scales::comma(sum_rea),"}\\\\(",round(mean_rea,1),")    }"))#  %>% 


to_match <- 
  d2d  %>% 
  ungroup%>%
  select(who, platform,n) %>% 
  distinct() %>% 
  mutate(xx = c("v1","v1","v1","v2","v2","v2","v1","v1","v1")) %>%
  spread(xx,n) %>%
  mutate(v2_chr = ifelse(is.na(v2),"\\ ",paste0("(",scales::comma(v2)," of which RTs)"))) %>%
  mutate(v2 = ifelse(is.na(v2),0,v2)) %>%
  mutate(posts = paste0("\\makecell[r]{\\textbf{",
                        scales::comma(v1+v2),"}\\\\",v2_chr,"}"))  %>% 
  ungroup() 

uniacc <- 
  d2 %>% 
  distinct(where,who,user_handle) %>% 
  group_by(who,platform = where) %>% 
  summarise(n = n()) %>% ungroup 


d2d_res <- 
  d2d %>%
  ungroup %>%
  filter(is.na(is_retweeting_tweet) | is_retweeting_tweet == FALSE) %>% 
  select(-sum_rea,-mean_rea,-is_retweeting_tweet) %>%
  spread(key,text) %>%
  left_join(to_match,by = c("who","platform")) %>% 
  select(-v1,-v2_chr,-v2,-n) %>%
  ungroup %>% 
  data.frame(.,uniacc %>% select(nn = n) %>%
               mutate(nn = paste0("\\makecell[r]{\\textbf{",
                                  nn,"}}"))) %>% 
  select(who,platform,nn,posts,likes,replies,shares)

d2_sums <- 
  d2d %>% 
  ungroup%>%  
  select(who,platform,key,n,sum_rea) %>%
  spread(key,sum_rea) %>% 
  ungroup() %>%
  summarise_at(vars(n:shares),
               funs(paste0("\\makecell[r]{\\textbf{",scales::comma(sum(.)),"}}"))) %>% 
  rename(posts = n) %>%
  data.frame(who = "\\textbf{Total Sum}",
             platform = "",
             nn =  uniacc %>% select(n) %>% 
               summarise(nn = sum(n)) %>%
               mutate(nn = paste0("\\makecell[r]{\\textbf{",
                                  nn,"}}")) %>% 
               pull(nn) ,
             .)


level_key <- c(med_en = "State Media",
               med_oth = "State Media",
               dip = "Diplomat")

tab_df <- 
  bind_rows(d2d_res,
            d2_sums) %>% 
  mutate(platform = platform %>% str_to_title(),
         who2 = recode(who,!!!level_key))

tab_df$lang <- ""
tab_df$lang[tab_df$who == "med_en"] <- "English"
tab_df$lang[tab_df$who == "med_oth"] <- "Other Lang"

tab_df <- 
  tab_df%>% 
  select(who2,platform,lang,nn,posts,likes,replies,shares) %>% 
  slice(c(1,2,3,5,4,6,7))

colnames(tab_df) <- 
  c("Account","Platform","Language","Users*","Total Active Posts**","Likes*** \\^,","Replies***","Shares***") %>%
  paste0("\\textbf{",.,"}")


tt <- xtable(tab_df,
             align = "l||p{2.2cm}|p{2.0cm}|p{1.9cm}|p{1.4cm}|p{3.8cm}|p{2.3cm}|p{2.0cm}|p{2.3cm}|"
)

comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(tab_df))
comment$command[1]  <- paste0("\\hline \n \\multicolumn{8}{l}",
                              "{\\makecell[l]{\\footnotesize{* Accounts who have posted at least once between June 2020 and February 2021}}}")

comment$pos[[2]] <- c(nrow(tab_df))
comment$command[2]  <- paste0("\\\\  \n \\multicolumn{8}{l}",
                              "{\\makecell[l]{\\footnotesize{** For Twitter, the number of active posts includes both active text posts and quote tweets as well as retweets that diplomats send themselves}}}")


comment$pos[[3]] <- c(nrow(tab_df))
comment$command[3]  <- paste0("\\\\ \n \\multicolumn{8}{l}",
                              "{\\makecell[l]{\\footnotesize{*** Average likes, replies \\& shares per post are denoted in Parentheses. Averages are calculated on FB-Posts, Tweets, \\& Quoted Tweets only, }}}")
comment$pos[[4]] <- c(nrow(tab_df))
comment$command[4]  <- paste0("\\\\  \n \\multicolumn{8}{l}",
                              "{\\makecell[l]{\\footnotesize{    but not on Retweets, as retweets cannot accumulate engagement themselves.}}}")


comment$pos[[5]] <- c(nrow(tab_df))
comment$command[5]  <- paste0("\\\\  \n \\multicolumn{8}{l}",
                              "{\\makecell[l]{\\footnotesize{\\^ Reactions include Twitter Favourites and Facebook Reactions (Like, Wow, Care, Love, etc.)}}}")



hlines <- c(-1,0,2,2,4,6,6)


print(tt, 
      caption.placement = "top",
      # tabular.environment="tabular",
      include.rownames = F, 
      floating = F,
      booktabs = T,
      sanitize.text.function = identity,
      add.to.row = comment,     # this is where you actually make the substitution
      hline.after = hlines,
      file="manuscript/results/table1.tex")


###################################################

### APPENDIX TABBELLEN

old <- read_csv("data/input_selectors/old/china-targets.csv")

added_later <- 
  accs %>% filter(platform == "twitter",type == "diplomat")%>%
  pull(handle) %>% .[! . %in% old$twitter_handle] %>% 
  .[! . %in% c("CaoYi_MFA")]

rts <- 
  bind_rows(
    df %>% 
      filter(where == "twitter" & (retweet_dip %in% dips)) %>% 
      group_by(user_handle = retweet_dip) %>% 
      summarise(n_rts = n())
  )

names(df)

all_rts <-  df %>% 
  filter(where == "twitter" & (retweet_dip %in% dips))


dip_rts_cumulative_bydip2 <- 
  all_rts %>%
  group_by(target = retweet_dip,user_id,user_handle) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  group_by(target) %>%
  mutate(cumsum = cumsum(n)) %>%
  arrange(desc(n)) %>% 
  mutate(rev_cumsum = cumsum(n))  %>%
  arrange(n)  %>%
  group_by(target) %>%
  mutate(perc = n/sum(n,na.rm = T)) #


library(DescTools)

ginidf2 <-
  map(.x = dips,
      .f = function(dip){
        
        #dip = "chinaembcongobz"
        
        gi <-
          dip_rts_cumulative_bydip2 %>%
          filter(target == dip) 
        
        # gi <- cumulative
        
        
        #print(length(gi))
        gini <- data.frame(user_screen_name = dip,
                           gini = Gini(gi$n),
                           n_total = sum(gi$n,na.rm = T),
                           u_total = nrow(gi),
                           perc_01 = round(100*(1-quantile(gi$cumsum,0.99,type = 1)/sum(gi$n)),1),
                           n_01 = -(quantile(gi$cumsum,0.99,type = 1) - sum(gi$n)),
                           q_01 = quantile(gi$n,0.99,type = 1),
                           u_01 =ceiling(0.01*nrow(gi)),
                           perc_001 = round(100*(1-quantile(gi$cumsum,0.999,type = 1)/sum(gi$n)),1),
                           n_001 = -(quantile(gi$cumsum,0.999,type = 1) - sum(gi$n)),
                           q_001 = quantile(gi$n,0.999,type = 1),
                           u_001 =ceiling(0.001*nrow(gi))
        )
        
        return(gini)
      }) %>% bind_rows()
rownames(ginidf2) <- NULL

saveRDS(ginidf2,"data/processed/gini2.rds")


dip_twi <- 
  df %>% 
  filter(where == "twitter" & (post_dip %in% dips))

# system.time({
#   dips_found <- 
#     lookup_users(dip_twi %>% pull(user_id) %>% unique(), 
#                  parse = TRUE, 
#                  token = ox_token)
# })


oldtable <- readRDS("data/nogit/for_dip_twitter_viz.rds")

keys = c("A" = "Ambassador",
         "B" = "Beijing/MFA",
         "S" = "Staffer",
         "E" = "Embassy",
         "C" = "Consul(ate)")

merged_twitter_new <-
  oldtable  %>% 
  select(ISO2,handle = api_screen_name,dipgroup) %>%
  left_join(twitter_seedlist %>% select(ISO2,country) %>% mutate(country = ifelse(ISO2 == "OTH","Beijing/Int. Organiz.",country)) %>% distinct(),
            by  = "ISO2") %>%
  mutate(diptype = recode(dipgroup,!!!keys))

dips_found <- oldtable %>% select(user_id,api_screen_name,name,account_created_at,followers_count)

dip_tot <- 
  dip_twi %>% 
  group_by(user_id,user_handle) %>% 
  summarise(n_tweets = n())

dip_med <- 
  dip_twi %>% 
  filter((!is.na(detected_outlet)) | 
           (!is.na(retweet_med))| 
           (!is.na(quoted_med)) | 
           (!is.na(retweet_med_foreign))| 
           (!is.na(quoted_med_foreign))| 
           (!is.na(retweet_url))) %>% 
  group_by(user_id,user_handle) %>% 
  summarise(med_tweets = n())

d3 <- 
  readRDS("data/nogit/granular_statuses.rds")

rts_susp <- 
    df %>% 
      left_join(d3 %>% filter(status == 63),
                by = "user_id") %>%
      filter(status == 63) %>%
      filter(where == "twitter" & (retweet_dip %in% dips)) %>% 
      group_by(user_handle = retweet_dip) %>% 
      summarise(n_rts_susp = n())

twi_labs <- 
  readRDS("data/results/twitter_labels_final.rds")%>%
  select(handle,gov_label,verified)

dip_table <- 
  left_join(#dips_found %>% select(user_id,api_screen_name = screen_name,name,account_created_at,followers_count),
            dips_found %>% select(user_id,api_screen_name,name,account_created_at,followers_count),
            dip_tot,
            by = c("user_id")) %>%
  left_join(dip_med,
            by = c("user_id","user_handle")) %>% 
  left_join(rts_susp,
            by = c("user_handle")) %>% 
  left_join(twi_labs,
            by = c("user_handle" = "handle")) %>% 
  left_join(#merged_twitter %>% filter(platform == "twitter",type == "diplomat") %>% select(ISO2,handle,dipgroup),
            merged_twitter_new %>% select(-dipgroup),
            by = c("user_handle" = "handle")) %>%
  left_join(rts,
            by = "user_handle") %>%
  arrange(desc(n_tweets))



dip_table$susp_perc  <- paste0(round(100*dip_table$n_rts_susp/dip_table$n_rts ,1),"%")
dip_table$susp_perc  <- paste0(round(100*dip_table$n_rts_susp/dip_table$n_rts ,0),"")

dip_table$susp_perc[dip_table$susp_perc  == "NA%"] <- "0%"
dip_table$susp_perc[dip_table$susp_perc  == "NA"] <- "0"

dip_table <- dip_table %>%
  mutate(med_tweets = paste0(scales::comma(med_tweets,accuracy = 1),
                             " (",round(100*as.numeric(med_tweets)/as.numeric(n_tweets),1),"%)")) %>%
  mutate_at(vars(n_tweets,n_rts),
            funs(scales::comma(.,accuracy = 1))) 

dip_table$followers_count

dip_table$verified3  <- ifelse(dip_table$verified == FALSE,"","\\ding{52}")
dip_table$verified2  <- ifelse(dip_table$verified == FALSE,"","VERIFIED")

#dip_table$gov_label  <- ifelse(is.na(dip_table$gov_label),"-","GOV")
#dip_table$account_created_at <- format(dip_table$account_created_at,"%b %Y")
dip_table$med_tweets[dip_table$med_tweets  == "NA (NA%)"] <- "0 (0%)"
dip_table$n_rts[dip_table$n_rts  == "NA" | is.na(dip_table$n_rts)] <- "0"
dip_table$susp_perc[dip_table$n_rts  == "0"] <- "-"

dip_table2 <-
  dip_table %>%
  left_join(
    readRDS("data/processed/gini2.rds") %>% 
      select(user_handle = user_screen_name,gini) %>%
      mutate(gini = round(gini,2)) %>%
      mutate(gini = ifelse(is.na(gini),"-",gini)),
    by = "user_handle"
  )  

dip_table2$user_handle <- ifelse(dip_table2$user_handle %in% added_later,
                                paste0(dip_table2$user_handle,"*"),
                                dip_table2$user_handle)



dips_word <- 
  dip_table2 %>% 
  mutate(user_handle = paste0("@",user_handle)) %>%
  select(country,diptype,
         user_handle,#name,
         #dipgroup,#ISO2,
         account_created_at,followers_count,n_tweets,#med_tweets,
         n_rts,gini,n_rts_susp,susp_perc,gov_label,verified2) %>% 
  group_split(split = country == "Beijing/Int. Organiz.") %>%
  map(.x = .,
      .f =~ .x %>% arrange(country)) %>% 
  bind_rows() %>%
  select(-split)

dips_word_erika <- 
  dips_word %>% 
  mutate_at(vars(followers_count:n_rts,gini,susp_perc),
            funs(as.numeric(str_remove_all(.,","))))


colnames(dips_word_erika) <- 
  c("Country","Type","Handle","Created","Followers","Total Tweets",
    #"Amplify State Media",
    "Total Retweets","Gini RTs","N Suspended RTs",
    "Suspended RTs (in%)","GOV Label","Verified")

writexl::write_xlsx(dips_word_erika,"manuscript/word/appendixtable_diplomats_twitter_erikafinal_june9_feb23.xlsx")


dips_sorted <- 
  dip_table2 %>% 
  mutate(user_handle = paste0("@",user_handle)) %>%
  select(user_handle,#name,
         dipgroup,ISO2,account_created_at,followers_count,n_tweets,med_tweets,
         n_rts,gini,susp_perc,gov_label,verified)



colnames(dips_sorted) <- 
  paste0("\\textbf{",c("Handle",#"Name",
                       "Type","Country","Created","Followers",
                       "N Tweets","Amplify SBM",
                       "N RTs", "RTs by Suspended","GOV Lab","Verified"),"}")


View(dips_sorted %>% 
       filter(`\\textbf{Country}` == "GB"))

saveRDS(dip_table,"data/nogit/for_dip_twitter_viz_new.rds")


library(xtable)
library(tidyverse)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

tab <-
  dips_sorted %>%
  mutate_all(funs(gsub("%","\\%",.,fixed = T))) %>% 
  mutate_all(funs(gsub("_","\\_",.,fixed = T))) %>%
  mutate_all(funs(gsub("#","\\#",.,fixed = T)))

colnames(tab) <-gsub("_","\\_",colnames(tab),fixed = T)

tab$`\\textbf{Country}`[tab$`\\textbf{Country}` == "OTH"] <- ""

hlines <- seq(-1,nrow(tab))


tt <- xtable(tab,
             label = "appendixtab1",
             caption = "List of All Twitter Diplomat Accounts with Activity and Engagement Statistics",
             align = "l|p{2.5cm}|p{0.6cm}|p{1cm}|p{1.3cm}|p{1.2cm}|p{1.2cm}|p{1.7cm}|p{1.1cm}|p{1.2cm}|p{1.1cm}|p{1.1cm}|"
)

addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste(
  "\\hline \n",
  "\\endhead \n",
  "\\hline \n",
  #"{\\footnotesize Continued on next page} \n",
  "\\endfoot \n",
  "\\endlastfoot \n",
  sep=""))


print(tt, 
      caption.placement = "top",
      tabular.environment="longtable",
      include.rownames = F, 
      floating = F,
      booktabs = T,
      sanitize.text.function = identity,
      add.to.row = addtorow,     # this is where you actually make the substitution
      hline.after = hlines,
      file="manuscript/results/diplomatappendix.tex")



#########

### for state media twitter 
old <- read_csv("data/input_selectors/old/china-targets.csv")

added_later <- 
  accs %>% filter(platform == "twitter",type == "statemedia")%>%
  pull(handle) %>% .[! . %in% old$twitter_handle] %>% 
  .[! . %in% c("CaoYi_MFA")]

rts_med <- 
  bind_rows(
    df %>% 
      filter(where == "twitter" & (retweet_med %in% c(en_med,oth_med))) %>% 
      group_by(user_handle = retweet_med) %>% 
      summarise(n_rts = n())
  )

med_twi <- 
  df %>% 
  filter(where == "twitter" & (post_med %in% c(en_med,oth_med)))

system.time({
  med_found <- 
    lookup_users(med_twi %>% pull(user_id) %>% unique(), 
                 parse = TRUE, 
                 token = ox_token)
})
#saveRDS(med_found,"data/processed/med_found.rds")

med_tot <- 
  med_twi %>% 
  group_by(user_id,user_handle) %>% 
  summarise(n_tweets = n())


d3 <- 
  readRDS("data/nogit/granular_statuses.rds")#%>% 
#filter(final_status == "suspended") %>% ungroup

rts_susp_med <- 
  df %>% 
  left_join(d3 %>% filter(status == 63),
            by = "user_id") %>%
  filter(status == 63) %>%
  filter(where == "twitter" & (retweet_med %in% c(en_med,oth_med))) %>% 
  group_by(user_handle = retweet_med) %>% 
  summarise(n_rts_susp = n())

twi_labs <- 
  readRDS("data/results/twitter_labels_final.rds")%>%
  select(handle,gov_label,verified)

med_table <- 
  left_join(med_found %>% select(user_id,api_screen_name = screen_name,name,account_created_at,followers_count),
            med_tot,
            by = c("user_id")) %>%
  left_join(rts_susp_med,
            by = c("user_handle")) %>% 
  left_join(twi_labs,
            by = c("user_handle" = "handle")) %>% 
  left_join(merged_twitter %>% filter(platform == "twitter",type == "statemedia") %>% select(lang,handle,dipgroup),
            by = c("user_handle" = "handle")) %>%
  left_join(rts_med,
            by = "user_handle") %>%
  arrange(desc(n_tweets))

med_table$user_handle <- ifelse(med_table$user_handle %in% added_later,
                                paste0(med_table$user_handle,"*"),
                                med_table$user_handle)


med_table$susp_perc  <- paste0(round(100*med_table$n_rts_susp/med_table$n_rts ,1),"%")
med_table$susp_perc[med_table$susp_perc  == "NA%"] <- "0%"

med_table <- med_table %>%
 # mutate(med_tweets = paste0(scales::comma(med_tweets,accuracy = 1),
  #                           " (",round(100*as.numeric(med_tweets)/as.numeric(n_tweets),1),"%)")) %>%
  mutate_at(vars(followers_count,n_tweets,n_rts),
            funs(scales::comma(.,accuracy = 1))) 

#med_table$verified  <- ifelse(med_table$verified == FALSE,"","\\ding{52}")
med_table$verified  <- ifelse(med_table$verified == FALSE,"","veri")
med_table$gov_label  <- ifelse(is.na(med_table$gov_label),"","GOV")

med_table$account_created_at <- format(med_table$account_created_at,"%b %Y")
#dip_table$med_tweets[dip_table$med_tweets  == "NA (NA%)"] <- "0 (0%)"
med_table$n_rts[med_table$n_rts  == "NA" | is.na(med_table$n_rts)] <- "0"
med_table$susp_perc[med_table$n_rts  == "0"] <- "-"


med_sorted <- 
  med_table %>% 
  mutate(user_handle = paste0("@",user_handle)) %>%
  select(user_handle,#name,
         dipgroup,lang,account_created_at,followers_count,n_tweets,#med_tweets,
         n_rts,susp_perc,gov_label,verified)


meds_word <- 
  med_table %>% 
  mutate(user_handle = paste0("@",user_handle)) %>%
  select(#country,#diptype,
         user_handle,#name,
         lang,
         #dipgroup,#ISO2,
         account_created_at,followers_count,n_tweets,#med_tweets,
         n_rts,susp_perc,gov_label,verified)

colnames(meds_word) <- 
  c("Handle","Language","Created","Followers","Total Tweets",
    #"Amplify State Media",
    "Total Retweets",
    "Suspended RTs (in%)","GOV Label","Verified")

writexl::write_xlsx(meds_word,"manuscript/word/appendixtable2_sbm_twitter.xlsx")




colnames(med_sorted) <- 
  paste0("\\textbf{",c("Handle",#"Name",
                       "Type","Lang","Created","Followers",
                       "N Tweets",
                       "N RTs", "RTs by Suspended","GOV Lab","Verified"),"}")

tab2 <-
  med_sorted %>%
  mutate_all(funs(gsub("%","\\%",.,fixed = T))) %>% 
  mutate_all(funs(gsub("_","\\_",.,fixed = T))) %>%
  mutate_all(funs(gsub("#","\\#",.,fixed = T)))

colnames(tab2) <-gsub("_","\\_",colnames(tab2),fixed = T)

hlines <- seq(-1,nrow(tab2))

tt2 <- xtable(tab2,
             label = "appendixtab2",
             caption = "List of All Twitter State Media Accounts with Activity and Engagement Statistics",
             align = "l|p{2.5cm}|p{0.6cm}|p{1cm}|p{1.3cm}|p{1.2cm}|p{1.2cm}|p{1.1cm}|p{1.2cm}|p{1.1cm}|p{1.1cm}|"
)

addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste(
  "\\hline \n",
  "\\endhead \n",
  "\\hline \n",
  #"{\\footnotesize Continued on next page} \n",
  "\\endfoot \n",
  "\\endlastfoot \n",
  sep=""))


print(tt2, 
      caption.placement = "top",
      tabular.environment="longtable",
      include.rownames = F, 
      floating = F,
      booktabs = T,
      sanitize.text.function = identity,
      add.to.row = addtorow,     # this is where you actually make the substitution
      hline.after = hlines,
      file="manuscript/results/statemediaappendixtwitter.tex")



#########################

##Facebook

med_fb <- 
  df %>% 
  filter(where == "facebook" & (post_med %in% c(en_med,oth_med)))

ct_pgdata <- 
  read_csv(file = "data/ct_lists/returned_ct_page_ids_final2.csv",col_types = cols(.default = col_guess(),
                                                                                   id = col_character(),
                                                                                   platformId = col_character()))

med_tot <- 
  med_fb %>% 
  group_by(user_id,user_handle) %>% 
  summarise(n_tweets = n(),
            sum_rea = sum(likes,na.rm = T),
            sum_rep = sum(replies,na.rm = T),
            sum_shr = sum(shares,na.rm = T))

fb_labs <- 
  readRDS("data/results/facebook_labels_final.rds")%>%
  select(handle = url_handle,gov_label,verified)

med_table <- 
  left_join(med_tot,
            fb_labs %>% 
              mutate(handle = handle %>%
                       paste0(.," ") %>%
                       str_extract(".*?(?=/| |-[0-9])")),
            by = c("user_handle" = "handle")) %>% 
  left_join(fb_seedlist %>% 
              filter(platform == "facebook",type == "statemedia") %>% 
              select(handle,ISO2,lang),
            by = c("user_handle" = "handle")) %>%
  left_join(ct_pgdata %>% select(subscriberCount,handle,ct_verified = verified),
            by = c("user_handle" = "handle")) %>%
  arrange(desc(n_tweets))


med_table <- med_table %>%
  # mutate(med_tweets = paste0(scales::comma(med_tweets,accuracy = 1),
  #                           " (",round(100*as.numeric(med_tweets)/as.numeric(n_tweets),1),"%)")) %>%
  mutate_at(vars(subscriberCount,n_tweets,sum_rea,sum_rep,sum_shr),
            funs(scales::comma(.,accuracy = 1))) 

#med_table$verified  <- ifelse(med_table$verified == FALSE,"","\\ding{52}")
med_table$verified  <- ifelse(med_table$verified == FALSE,"","veri")
med_table$gov_label  <- ifelse(med_table$gov_label == "" |is.na(med_table$gov_label),"","GOV")


meds_word <- 
  med_table %>% 
  ungroup() %>%
  select(#country,#diptype,
    user_handle,#name,
    lang,subscriberCount,n_tweets,#med_tweets,
    sum_rea,sum_rep,sum_shr,gov_label,verified)

colnames(meds_word) <- 
  c("Page",#"Name",
    "Lang","Followers",
    "N Posts",
    "Reactions", "Comments","Shares","GOV Lab","Verified")

writexl::write_xlsx(meds_word,"manuscript/word/appendixtable3_sbm_fb.xlsx")



med_sorted <- 
  med_table%>% ungroup() %>% 
  #mutate(user_handle = paste0("@",user_handle)) %>%
  select(user_handle,#name,
         lang,subscriberCount,n_tweets,#med_tweets,
         sum_rea,sum_rep,sum_shr,gov_label,verified) 

colnames(med_sorted) <- 
  paste0("\\textbf{",c("Page",#"Name",
                       "Lang","Followers",
                       "N Posts",
                       "Reactions", "Comments","Shares","GOV Lab","Verified"),"}")

tab2 <-
  med_sorted %>%
  mutate_all(funs(gsub("%","\\%",.,fixed = T))) %>% 
  mutate_all(funs(gsub("_","\\_",.,fixed = T))) %>%
  mutate_all(funs(gsub("#","\\#",.,fixed = T)))

colnames(tab2) <-gsub("_","\\_",colnames(tab2),fixed = T)

hlines <- seq(-1,nrow(tab2))

tt2 <- xtable(tab2,
              label = "appendixtab3",
              caption = "List of All Facebook State Media Accounts with Activity and Engagement Statistics",
              align = "l|p{4cm}|p{0.6cm}|p{1.5cm}|p{1.1cm}|p{1.2cm}|p{1.1cm}|p{1.2cm}|p{1.1cm}|p{1.1cm}|"
)

addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste(
  "\\hline \n",
  "\\endhead \n",
  "\\hline \n",
  #"{\\footnotesize Continued on next page} \n",
  "\\endfoot \n",
  "\\endlastfoot \n",
  sep=""))


print(tt2, 
      caption.placement = "top",
      tabular.environment="longtable",
      include.rownames = F, 
      floating = F,
      booktabs = T,
      sanitize.text.function = identity,
      add.to.row = addtorow,     # this is where you actually make the substitution
      hline.after = hlines,
      file="manuscript/results/statemediaappendixfacebook.tex")


####################################################################

# DIPS Facebook

dip_fb <- 
  df %>% 
  filter(where == "facebook" & (post_dip %in% c(dips)))

ct_pgdata <- 
  read_csv(file = "data/ct_lists/returned_ct_page_ids_final2.csv",col_types = cols(.default = col_guess(),
                                                                                   id = col_character(),
                                                                                   platformId = col_character()))

dip_tot <- 
  dip_fb %>% 
  group_by(user_id,user_handle) %>% 
  summarise(n_tweets = n(),
            sum_rea = sum(likes,na.rm = T),
            sum_rep = sum(replies,na.rm = T),
            sum_shr = sum(shares,na.rm = T)) 

fb_labs <- 
  readRDS("data/results/facebook_labels_final.rds")%>%
  select(handle = url_handle,gov_label,verified)

fb_labs$gov_label[str_detect(fb_labs$gov_label,"Digital Media")] <- ""


dip_table <- 
  left_join(dip_tot,
            fb_labs %>% 
              mutate(handle = handle %>%
                       paste0(.," ") %>%
                       str_extract(".*?(?=/| |-[0-9])")),
            by = c("user_handle" = "handle")) %>% 
  left_join(fb_seedlist %>% 
              filter(platform == "facebook",type == "diplomat") %>% 
              select(handle,ISO2,lang),
            by = c("user_handle" = "handle")) %>%
  left_join(ct_pgdata %>% select(subscriberCount,handle,ct_verified = verified),
            by = c("user_handle" = "handle")) %>%
  arrange(desc(n_tweets)) %>%
  left_join(fb_seedlist %>% 
              select(ISO2,country) %>% 
              mutate(country = ifelse(ISO2 == "OTH","Beijing/Int. Organiz.",country)) %>%
              distinct(),
            by  = "ISO2")


dip_table <- dip_table %>%
  # mutate(med_tweets = paste0(scales::comma(med_tweets,accuracy = 1),
  #                           " (",round(100*as.numeric(med_tweets)/as.numeric(n_tweets),1),"%)")) %>%
  mutate_at(vars(subscriberCount,n_tweets,sum_rea,sum_rep,sum_shr),
            funs(scales::comma(.,accuracy = 1))) 

#dip_table$verified  <- ifelse(dip_table$verified == FALSE,"","\\ding{52}")
dip_table$verified  <- ifelse(dip_table$verified == FALSE,"","veri")
dip_table$gov_label  <- ifelse(dip_table$gov_label == "" |is.na(dip_table$gov_label),"","GOV")

dip_sorted <- 
  dip_table%>% ungroup() %>% 
 # mutate(user_handle = paste0("@",user_handle)) %>%
  select(country,user_handle,#name,
        # dipgroup = ISO2
        subscriberCount,n_tweets,#med_tweets,
         sum_rea,sum_rep,sum_shr,gov_label,verified) %>%
  arrange(country)

colnames(dip_sorted) <- 
 c("Country","Page","Followers",
                       "N Posts",
                       "Reactions", "Comments","Shares","GOV Lab","Verified")


writexl::write_xlsx(dip_sorted,"manuscript/word/appendixtable4_dips_fb.xlsx")




tab2 <-
  dip_sorted %>%
  mutate_all(funs(gsub("%","\\%",.,fixed = T))) %>% 
  mutate_all(funs(gsub("_","\\_",.,fixed = T))) %>%
  mutate_all(funs(gsub("#","\\#",.,fixed = T)))

colnames(tab2) <-gsub("_","\\_",colnames(tab2),fixed = T)

hlines <- seq(-1,nrow(tab2))

tt2 <- xtable(tab2,
              label = "appendixtab4",
              caption = "List of All Facebook Diplomat Accounts with Activity and Engagement Statistics",
              align = "l|p{4cm}|p{0.6cm}|p{1.5cm}|p{1.1cm}|p{1.2cm}|p{1.1cm}|p{1.2cm}|p{1.1cm}|p{1.1cm}|"
)

addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste(
  "\\hline \n",
  "\\endhead \n",
  "\\hline \n",
  #"{\\footnotesize Continued on next page} \n",
  "\\endfoot \n",
  "\\endlastfoot \n",
  sep=""))


print(tt2, 
      caption.placement = "top",
      tabular.environment="longtable",
      include.rownames = F, 
      floating = F,
      booktabs = T,
      sanitize.text.function = identity,
      add.to.row = addtorow,     # this is where you actually make the substitution
      hline.after = hlines,
      file="manuscript/results/diplomatappendixfacebook.tex")


###########################################################

#### what are diplomats tweeting with different symbols

# facebook post (sharing URL)
# Tweet (sharing URL)
# reply to others tweet
# RT/QT quote 3rd party
# RT/quote other diplomat
# RT/quote state media

df %>% head(100) %>% View

dip_url_shares <- 
  df %>%
  filter(!is.na(post_dip)) %>% 
  filter(!is.na(resolved_outlet)) %>%
  filter(where == "twitter" & (is_retweeting_tweet == F & is_quoting_tweet == F)) %>%
  select(post_id,
         platform = where,
         outlet = resolved_outlet) %>% 
  mutate(what = "dip_url_post")

dip_other_posts <- 
  df %>%
  filter(!is.na(post_dip)) %>% 
  filter(is.na(detected_domain)) %>%
  filter(where == "twitter" & (is_retweeting_tweet == F & is_quoting_tweet == F)) %>%
  select(post_id,
         platform = where) %>% 
  mutate(what = "dip_oth_post")

dip_rt_med <- 
  df %>%
  filter(!is.na(post_dip) & (!is.na(retweet_med))) %>% 
  select(post_id,
         platform = where) %>% 
  mutate(what = "dip_rqt_med")


dip_rt_med_foreign <- 
  df %>%
  filter(!is.na(post_dip) & (!is.na(retweet_med_foreign))) %>% 
  select(post_id,
         platform = where) %>% 
  mutate(what = "dip_rqt_med")

dip_rt_dip <- 
  df %>%
  filter(!is.na(post_dip) & (!is.na(retweet_dip))) %>% 
  select(post_id,
         platform = where)%>% 
  mutate(what = "dip_rqt_dip")

dip_rt_oth <- 
  df %>%
  filter(!is.na(post_dip) & 
           (is.na(retweet_dip)) & 
           (is.na(quoted_dip)) & 
           (is.na(retweet_med)) & 
           (is.na(quoted_med)) & 
           (is.na(retweet_med_foreign))&
           (is.na(quoted_med_foreign))) %>% 
  filter(is_retweeting_tweet == T) %>%
  select(post_id,
         platform = where)%>% 
  mutate(what = "dip_rqt_oth")

dip_qt_med <- 
  df %>%
  filter(!is.na(post_dip) & (!is.na(quoted_med))) %>% 
  select(post_id,
         platform = where)%>% 
  mutate(what = "dip_rqt_med")

dip_qt_med_foreign <- 
  df %>%
  filter(!is.na(post_dip) & (!is.na(quoted_med_foreign))) %>% 
  select(post_id,
         platform = where)%>% 
  mutate(what = "dip_rqt_med")

dip_qt_dip <- 
  df %>%
  filter(!is.na(post_dip) & (!is.na(quoted_dip))) %>% 
  select(post_id,
         platform = where)%>% 
  mutate(what = "dip_rqt_dip")

dip_qt_oth <- 
  df %>%
  filter(!is.na(post_dip) & 
           (is.na(retweet_dip)) & 
           (is.na(quoted_dip)) & 
           (is.na(retweet_med)) & 
           (is.na(quoted_med)) &
           (is.na(retweet_med_foreign))&
           (is.na(quoted_med_foreign))) %>%  
  filter(is_quoting_tweet == T) %>%
  select(post_id,
         platform = where) %>% 
  mutate(what = "dip_rqt_oth")


dip_rt_med2 <- 
  bind_rows(dip_rt_med,
            dip_rt_med_foreign)

dip_qt_med2 <- 
  bind_rows(dip_qt_med,
            dip_qt_med_foreign)

d5b <-
  bind_rows(
    dip_url_shares,
    dip_other_posts,
    dip_rt_med2,
    dip_rt_dip,
    dip_rt_oth,
    dip_qt_med2,
    dip_qt_dip,
    dip_qt_oth
  )

d5c <- 
  d5b %>% 
  group_by(platform,what) %>%
  summarise(n = n()) %>% 
  mutate(what2 = paste0(what,"-",platform))

d5c$what2 <- factor(d5c$what2, 
                    levels = d5c$what2[c(1,2,3,7,5,6,4)])

d5c$xx <- c(0,6,17,30,44,55,68)#[c(1,2,3,7,5,6,4)]
d5c$who <- c("FB Posts",
             "FB SBM URL",
             "Tweet with no URL",
             "Retweet 3rd Person",
             "Retweet Other Diplomat",
             "Retweet State Media",
             "Tweet State Media URL")



d5c$lab <- 
  paste0(d5c$who,"\n",scales::comma(d5c$n),"\n(",round(100*d5c$n/sum(d5c$n),1),"%)")


d5c$lab <- factor(d5c$lab, 
                  levels = d5c$lab[c(1,2,3,7,5,6,4)])


#install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)
library(ggoxford)

gg5 <- 
  ggplot(d5c,
         aes(values = n/100,fill = what2)
  ) +
  waffle::geom_waffle(
    color = "white",
    size = .25, n_rows = 20, flip = F
  ) +
geom_label(aes(label = lab,
               x = xx,
               y = -2,
               fill = what2),
           # y = -0.7,
           family = "Roboto",#fontface = "bold",
           size = 3.5,vjust = "center",nudge_x = 0.5)+
  # scale_colour_brewer(palette = "Set1")+
  scale_fill_brewer(palette = "Set1")+
coord_equal()  + 
  theme_minimal() + 
  theme(text = element_text(family="LM Roman 10",size = 14),
        legend.position = "top" ) + 
  labs(x = NULL,y = NULL,title = "Diplomat's Activity Log",
       subtitle = "139,674 Tweets and FB Posts split up by type of shared content") + 
  theme(legend.position = "none",
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
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 16,family = "Roboto")) +
  scale_y_continuous(limits = c(-3,21))


gg5


row1 <- "Source: Data collected via Twitter Streaming API and Facebook Crowdtangle API"
row2 <- "Observation Window: 9 June 2020 - 8 December 2020"
row3 <- "Target Accounts: xxx Twitter accounts and xxx Facebook Accounts (See Appendix for full list)"

oii_finalise_plot(gg5, 
                  source_name = paste0(row1,"
",row2,"
",row3,""), 
                  save_filepath = "manuscript/results/005_what_diplomats_doing.pdf",
                  width_pixels = 1200/1.4, 
                  height_pixels = 550/1.4,
                  height_footer = 0.081,
                  dpi = 1000,
                  scale = 1,footer_start = 0.9)



browseURL("manuscript/results/005_what_diplomats_doing.pdf")

