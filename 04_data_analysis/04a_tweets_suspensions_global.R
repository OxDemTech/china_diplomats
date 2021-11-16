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

system.time({
  df_sub <- readRDS("data/nogit/final_df_sub.rds")
})

all_lags <- readRDS("data/processed/all_rt_lags.rds")

accs <- 
  readRDS("data/input_selectors/accs.rds")

dips <- accs %>% filter(platform == "twitter" & type == "diplomat") %>% pull(handle)
meds <- accs %>% filter(platform == "twitter" & type == "statemedia") %>% pull(handle)


rts <- 
  df_sub %>%
  filter(retweet_dip %in% dips | retweet_med %in% meds ) %>%
  filter(!is.na(status))

rts$whom <- NA
rts$whom[!is.na(rts$retweet_dip)| !is.na(rts$quoted_dip)] <- "diplomat"
rts$whom[!is.na(rts$retweet_med)| !is.na(rts$quoted_med)] <- "statemedia"
table(rts$whom)

rts %>% 
  group_by(whom) %>% 
  mutate(n_group = n()) %>%
  group_by(whom,status,n_group) %>%
  summarise(n = n()) %>%
  mutate(perc = n/n_group)


########

firstact <-
  rts %>% 
  filter(!is.na(retweet_dip)) %>%
  filter(final_status == "active") %>% 
  group_by(user_id) %>%
  group_by(user_id,user_screen_name,user_name,user_description,
           user_created_at,final_status,status,whom) %>%
  summarise(n = n(),
            first_tweeted = min(tweet_created_at,na.rm = T)) %>%
  mutate(dif = first_tweeted-user_created_at)

firstact2 <- 
  firstact %>% 
  filter(user_created_at > as.Date("2020-06-09")) %>% 
  mutate(difnum = as.numeric(dif)) %>%
  mutate(created_hour = floor_date(user_created_at,"1 hour")) %>% 
  group_by(created_hour) %>%
  mutate(n_hour = n()) %>%
  arrange(desc(n_hour),created_hour,user_created_at) %>%
  mutate_at(vars(user_created_at,created_hour,first_tweeted),funs(as.character(.))) 


### how long till first

first <-
  rts %>% 
  filter(!is.na(retweet_dip)) %>%
  filter(final_status == "suspended") %>% 
  group_by(user_id) %>%
  group_by(user_id,user_screen_name,user_name,user_description,
           user_created_at,final_status,status,whom) %>%
  summarise(n = n(),
            first_tweeted = min(tweet_created_at,na.rm = T)) %>%
  mutate(dif = first_tweeted-user_created_at)

first2 <- 
  first %>% 
  filter(user_created_at > as.Date("2020-06-09")) %>% 
  mutate(difnum = as.numeric(dif)) %>%
  mutate(created_hour = floor_date(user_created_at,"hour")) %>% 
  group_by(created_hour) %>%
  mutate(n_hour = n()) %>%
  arrange(desc(n_hour),created_hour,user_created_at) %>%
  mutate_at(vars(user_created_at,created_hour,first_tweeted),funs(as.character(.))) 



###########

g0 <- 
  rts %>% 
  group_by(whom,final_status) %>%
  summarise(n_author = length(unique(user_id)),
            total_rts = n()) %>% 
  gather(key,value,n_author,total_rts) %>% 
  group_by(whom,key) %>%
  mutate(perc = value/sum(value))

g2 <- 
  g1 <- 
  rts %>% 
  group_by(final_status) %>%
  summarise(n_author = length(unique(user_id)),
            total_rts = n()) %>% 
  gather(key,value,n_author,total_rts) %>% 
  group_by(key) %>%
  mutate(perc = value/sum(value)) %>%
  mutate(whom = "combined")

g1 <- bind_rows(g0,g2)


tabdf <- 
  data.frame(v1 = c("\\parbox[t]{2mm}{\\multirow{4}{*}{\\rotatebox[origin=c]{90}{Diplomats}}}",
                    paste0("\\cline{2-4}"),
                    "",
                    "",
                    "\\parbox[t]{2mm}{\\multirow{4}{*}{\\rotatebox[origin=c]{90}{SM}}}",
                    paste0("\\cline{2-4}"),
                    "",
                    "",
                    "\\parbox[t]{2mm}{\\multirow{4}{*}{\\rotatebox[origin=c]{90}{Combined}}}",
                    paste0("\\cline{2-4}"),
                    "",
                    ""),
             v2 = c(paste0("\\textbf{Retweeting Diplomats:}"),
                    "Suspended Accounts",
                    "No longer Existing",
                    "Suspicious* Accounts",
                    paste0("\\textbf{Retweeting State Media:}"),
                    "Suspended Accounts",
                    "No longer Existing",
                    "Suspicious* Accounts",
                    paste0("\\textbf{Retweeting State Media:}"),
                    "Suspended Accounts",
                    "No longer Existing",
                    "Suspicious* Accounts"),
             ###########################################################
             v3 = c( paste0("\\textbf{",comma(g1 %>% filter(key == "n_author",
                                                            whom == "diplomat") %>% 
                                                pull(value) %>% sum()),"}"),
                     paste0("",comma(g1 %>% filter(key == "n_author",
                                                   whom == "diplomat",
                                                   final_status == "suspended") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "n_author",
                                                                                 whom == "diplomat",
                                                                                 final_status == "suspended") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "n_author",
                                                   whom == "diplomat",
                                                   final_status == "deleted") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "n_author",
                                                                                 whom == "diplomat",
                                                                                 final_status == "deleted") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "n_author",
                                                   whom == "diplomat",
                                                   final_status == "active") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "n_author",
                                                                                 whom == "diplomat",
                                                                                 final_status == "active") %>% pull(perc),1),"\\%)"),
                     paste0("\\textbf{",comma(g1 %>% filter(key == "n_author",
                                                            whom == "statemedia") %>% 
                                                pull(value) %>% sum()),"}"),
                     paste0("",comma(g1 %>% filter(key == "n_author",
                                                   whom == "statemedia",
                                                   final_status == "suspended") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "n_author",
                                                                                 whom == "statemedia",
                                                                                 final_status == "suspended") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "n_author",
                                                   whom == "statemedia",
                                                   final_status == "deleted") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "n_author",
                                                                                 whom == "statemedia",
                                                                                 final_status == "deleted") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "n_author",
                                                   whom == "statemedia",
                                                   final_status == "active") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "n_author",
                                                                                 whom == "statemedia",
                                                                                 final_status == "active") %>% pull(perc),1),"\\%)"),
                     paste0("\\textbf{",comma(g1 %>% filter(key == "n_author",
                                                            whom == "combined") %>% 
                                                pull(value) %>% sum()),"}"),
                     paste0("",comma(g1 %>% filter(key == "n_author",
                                                   whom == "combined",
                                                   final_status == "suspended") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "n_author",
                                                                                 whom == "combined",
                                                                                 final_status == "suspended") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "n_author",
                                                   whom == "combined",
                                                   final_status == "deleted") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "n_author",
                                                                                 whom == "combined",
                                                                                 final_status == "deleted") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "n_author",
                                                   whom == "combined",
                                                   final_status == "active") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "n_author",
                                                                                 whom == "combined",
                                                                                 final_status == "active") %>% pull(perc),1),"\\%)")),
             ############################################
             v4 = c( paste0("\\textbf{",comma(g1 %>% filter(key == "total_rts",
                                                            whom == "diplomat") %>% 
                                                pull(value) %>% sum()),"}"),
                     paste0("",comma(g1 %>% filter(key == "total_rts",
                                                   whom == "diplomat",
                                                   final_status == "suspended") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "total_rts",
                                                                                 whom == "diplomat",
                                                                                 final_status == "suspended") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "total_rts",
                                                   whom == "diplomat",
                                                   final_status == "deleted") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "total_rts",
                                                                                 whom == "diplomat",
                                                                                 final_status == "deleted") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "total_rts",
                                                   whom == "diplomat",
                                                   final_status == "active") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "total_rts",
                                                                                 whom == "diplomat",
                                                                                 final_status == "active") %>% pull(perc),1),"\\%)"),
                     paste0("\\textbf{",comma(g1 %>% filter(key == "total_rts",
                                                            whom == "statemedia") %>% 
                                                pull(value) %>% sum()),"}"),
                     paste0("",comma(g1 %>% filter(key == "total_rts",
                                                   whom == "statemedia",
                                                   final_status == "suspended") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "total_rts",
                                                                                 whom == "statemedia",
                                                                                 final_status == "suspended") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "total_rts",
                                                   whom == "statemedia",
                                                   final_status == "deleted") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "total_rts",
                                                                                 whom == "statemedia",
                                                                                 final_status == "deleted") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "total_rts",
                                                   whom == "statemedia",
                                                   final_status == "active") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "total_rts",
                                                                                 whom == "statemedia",
                                                                                 final_status == "active") %>% pull(perc),1),"\\%)"),
                     paste0("\\textbf{",comma(g1 %>% filter(key == "total_rts",
                                                            whom == "combined") %>% 
                                                pull(value) %>% sum()),"}"),
                     paste0("",comma(g1 %>% filter(key == "total_rts",
                                                   whom == "combined",
                                                   final_status == "suspended") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "total_rts",
                                                                                 whom == "combined",
                                                                                 final_status == "suspended") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "total_rts",
                                                   whom == "combined",
                                                   final_status == "deleted") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "total_rts",
                                                                                 whom == "combined",
                                                                                 final_status == "deleted") %>% pull(perc),1),"\\%)"),
                     paste0("",comma(g1 %>% filter(key == "total_rts",
                                                   whom == "combined",
                                                   final_status == "active") %>% 
                                       pull(value))," (",round(100*g1 %>% filter(key == "total_rts",
                                                                                 whom == "combined",
                                                                                 final_status == "active") %>% pull(perc),1),"\\%)"))
  )

colnames(tabdf) <- c("","","\\textbf{Unique Users}","\\textbf{Retweets}")

tabdf[c(4,8,12),c(2:4)] <- ""


tabdf



tt <- xtable(tabdf,
             align = "l|p{0.7cm}|p{5cm}||p{4cm}|p{3cm}|"
)

comment          <- list()
comment$pos      <- list()
comment$pos[[1]] <- c(nrow(tabdf))
comment$command[1]  <- paste0("\\hline \n \\multicolumn{2}{l}",
                              "{\\makecell[l]{\\footnotesize{Note: xxx }}}")

hlines <- c(-1,0,4,8,8,nrow(tabdf))

print(tt, 
      caption.placement = "top",
      # tabular.environment="tabular",
      include.rownames = F, 
      floating = F,
      booktabs = T,
      sanitize.text.function = identity,
      # add.to.row = comment,     # this is where you actually make the substitution
      hline.after = hlines,
      file="manuscript/results/table_suspended.tex")


#############################################################

### suspended

forgephi2 <- 
  rts %>% 
  filter(!is.na(retweet_dip)) %>%
  mutate(target = ifelse(!is.na(retweet_dip),retweet_dip,retweet_med)) %>%
  # filter(final_status == "suspended" | !is.na(tweet_dip) | !is.na(tweet_med)) %>%
  filter(final_status == "suspended") %>% 
  left_join(accs %>% filter(platform == "twitter") %>% select(type_source = type,handle),
            by = c("user_screen_name" = "handle")) %>%
  left_join(accs %>% filter(platform == "twitter") %>% select(type_target = type,handle),
            by = c("target" = "handle")) %>%
  group_by(Source = user_screen_name, 
           user_id,
           Target = target,
           type_source,
           type_target,
           final_status) %>% 
  summarise(abs_weight = n()) %>% 
  ungroup() %>%
  mutate_at(vars(contains("type_")),funs(ifelse(is.na(.),"other",.)))

## aggregated

edges2 <- 
  forgephi2 %>% 
  ungroup() %>%
  na.omit() %>% 
  filter(type_source %in% c(#"diplomat","statemedia",
    "other")) %>% 
  ungroup() %>% 
  group_by(Source) %>%
  mutate(n_source = sum(abs_weight,na.rm = T)) %>%
  #filter(n_source > 5) %>% 
#  filter(abs_weight > 4) %>%
  ungroup()


edges2$Weight <- edges2$abs_weight

table(edges2$Weight)

write_csv(edges2,"data/gephi/gephi_edges_final.csv")

indeg <- 
  edges2$Target %>% table() %>% .[. > 0] %>% names()

# nodes
# Id column || 

which_nodes <- 
  c(edges2$Source,edges2$Target) %>% 
  unique() %>% data.frame(user_id = .)

latest_included <- c("SpokespersonCHN","zlj517","MFA_China","ChinaEUMission",
                     "ChinaEmbOttawa","ChineseEmbinUS","Chinamission2un",
                     "AmbCuiTiankai","AmbLiuXiaoming")

nodes2 <- 
  bind_rows(
    edges2 %>% select(Id = Source,label = Source,status = type_source,active = final_status),
    edges2 %>% select(Id = Target,label = Target,status = type_target,active = final_status)
  ) %>% 
  distinct(Id,.keep_all = T) %>% 
  filter(!is.na(Id)) %>%
  mutate(stat_comb = paste0(status,"--",active)) %>%
  mutate(label = ifelse( stat_comb != "other--active",label,"")) %>%
  mutate(label = ifelse( status != "other",label,"")) %>%  mutate(label = ifelse( Id %in% latest_included,label,""))

write_csv(nodes2,"data/gephi/gephi_nodes_final_selected_diplabels.csv")


rts %>% filter(user_screen_name %in% c("amos3875"))
####################################################

## who profits most from suspended

dips_susp1 <- 
  rts %>% 
  filter(!is.na(final_status))%>%
  ungroup() %>%
  filter(!is.na(retweet_dip)) %>%
  group_by(retweet_dip) %>%
  mutate(n_combined = n()) %>%
  group_by(retweet_dip,final_suspended = final_status == "suspended",n_combined) %>% 
  summarise(n = n()) %>%
  mutate(perc = n/n_combined) %>%
  group_by(final_suspended) 


triplecheck_susp <- 
  dips_susp1 %>%
  filter(final_suspended == T) %>%
  rename(suspended_march1 = final_suspended) %>%
  arrange(desc(perc))

writexl::write_xlsx(triplecheck_susp,
                    "data/nogit/triple_checks/per_diplomat_sharesuspended.xlsx")


all_susps <- 
  rts %>%
  filter(final_status == "suspended") %>% 
  filter(retweet_dip %in% dips) %>%
  group_split(retweet_dip) %>%
  set_names(map_chr(., ~ .x %>% pull(retweet_dip) %>% unique)) %>% 
  .[triplecheck_susp$retweet_dip]




dips_susp <- 
  left_join(
    dips_susp1,
    dips_susp1 %>% 
      ungroup() %>% 
      filter(final_suspended == T) %>% 
      select(retweet_dip,susp_perc = perc,n_susp = n),
    by = c("retweet_dip")
  ) %>% 
  arrange(desc(susp_perc)) %>% 
  filter(n_combined > 100) %>% 
  ungroup %>%
  mutate(r = dense_rank(desc(susp_perc)),
         r_n =  dense_rank(desc(n_susp)))



gg1 <- 
  dips_susp %>%
  filter(n_combined >= 100) %>% 
 # filter(r <= 20) %>%
  filter(retweet_dip %in% c("Chinaemb_Hellas","ChinaEmbAngola","ChineseLiberia","AmbChenBo","ChinaAmbBelgium",
                            "ChineseEmbinUK","ChineseEmbinUS","AmbLiuXiaoMing","AmbCuiTiankai",
                            "SpokespersonCHN","zlj517","MFA_China","ChinaEUMission")) %>%
  ungroup() %>%
  arrange(r) %>% 
  mutate(target = retweet_dip %>% paste0("@",.)%>% forcats::fct_inorder()) %>%
  ggplot(aes(x = target, 
             y = perc,
             fill = final_suspended)) + 
  geom_bar(stat = "identity",position = "stack") +
  scale_fill_manual(name = "Account\nStatus of\nRetweeter",
                    values = c("grey","red")) +
  geom_label(data = function(x) x %>% filter(final_suspended == T),
            aes(x = target,y = perc,label = paste0(round(100*perc,0),"")),
            col = "white",size = 4.5,nudge_y = 0.01) +
  

  scale_y_continuous(breaks = seq(0,1,0.1),labels = seq(0,1,0.1) %>% scales::percent() %>% str_remove(".0%")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1,face = "bold",size = 12),
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
        plot.caption = ggplot2::element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,1),"cm")) + 
  labs(x = NULL,
       y = NULL,
       title = "Share of Suspended Accounts Retweeting Diplomats (Jun 2020 - Jan 2021)",
       subtitle = "Top 25 Diplomats with highest share of suspended account engagement")

#############################################################################
gg1


row1 <- "Observation Window: June 9th 2020 - January 31st 2021"
row2 <- "Account Status of Retweeters Queried on March 1st, 2021"
row3 <- "Diplomats with less than 100 total retweets excluded"

oii_finalise_plot(gg1, 
                  source_name = paste0(row1,"
",row2,"
",row3,""), 
                  save_filepath = "manuscript/results/diplomat_inauth01b.pdf",
                  width_pixels = 1200/1.4, 
                  height_pixels = 700/1.4,  
                  height_footer = 0.081,
                  dpi = 1000,
                  scale = 1,
                  footer_start = 0.9)
browseURL("manuscript/results/diplomat_inauth01.pdf")

oii_finalise_plot(gg1, 
                  source_name = paste0(row1,"
",row2,"
",row3,""), 
                  save_filepath = "manuscript/word/diplomat_inauth01.pdf",
                  width_pixels = 1200/1.4, 
                  height_pixels = 700/1.4,  
                  height_footer = 0.081,
                  dpi = 1000,
                  scale = 1,
                  footer_start = 0.9)
browseURL("manuscript/word/diplomat_inauth01.pdf")

###################################

gg2 <- 
  dips_susp %>%
  filter(final_suspended == T) %>%
  filter(n_combined >= 100) %>% 
  filter(r_n <= 20) %>%
  ungroup() %>%
  arrange(r_n) %>% 
  mutate(target = retweet_dip%>% paste0("@",.) %>% forcats::fct_inorder()) %>%
  ggplot(aes(x = target, 
             y = n,
             fill = final_suspended)) + 
  geom_bar(stat = "identity",position = "stack") +
  scale_fill_manual(name = "Account\nStatus of\nRetweeter",
                    values = c("red")) +
  geom_label(data = function(x) x %>% filter(final_suspended == T),
             aes(x = target,y = n,label = scales::comma(n,accuracy = 1)),
             col = "white",size = 3.5,nudge_y = 0.01) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1,face = "bold",size = 12),
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
        plot.caption = ggplot2::element_blank(),
        plot.margin=unit(c(0.5,0.5,0.5,1.5),"cm")) + 
 
  labs(x = NULL,
       y = NULL,
       title = "Total Number of Diplomat Retweets by Suspended Accounts (Jun 2020 - Jan 2021)",
       subtitle = "Top 25 Diplomats with most suspended account engagement")

#############################################################################
gg2


row1 <- "Observation Window: June 9th 2020 - January 31st 2021"
row2 <- "Account Status of Retweeters Queried on March 1st, 2021"
row3 <- "Diplomats with less than 100 total retweets excluded"

oii_finalise_plot(gg2, 
                  source_name = paste0(row1,"
",row2,"
",row3,""), 
                  save_filepath = "manuscript/results/diplomat_inauth02b.pdf",
                  width_pixels = 1200/1.4, 
                  height_pixels = 700/1.4,  
                  height_footer = 0.081,
                  dpi = 1000,
                  scale = 1,
                  footer_start = 0.9)
browseURL("manuscript/results/diplomat_inauth02.pdf")

oii_finalise_plot(gg2, 
                  source_name = paste0(row1,"
",row2,"
",row3,""), 
                  save_filepath = "manuscript/word/diplomat_inauth02.pdf",
                  width_pixels = 1200/1.4, 
                  height_pixels = 700/1.4,  
                  height_footer = 0.081,
                  dpi = 1000,
                  scale = 1,
                  footer_start = 0.9)
browseURL("manuscript/word/diplomat_inauth02.pdf")

##############################################################
xxxxx <- 
  df_sub %>% 
  filter(what == "retweet") %>% 
  filter(!is.na(retweet_dip)) %>% 
filter(user_screen_name %in% top85 
       | (user_screen_name %in% celllist$user_screen_name))

ap_susp <- 
  df_sub %>% 
  filter(what == "retweet") %>% 
  filter(!is.na(retweet_dip)) %>% 
  filter(final_status == "suspended") %>% 
  select(tweet_created_at,user_screen_name,final_status,full_text,target,what) %>%
  group_by(user_screen_name) %>% 
  mutate(n_user = n()) %>%
  arrange(desc(n_user),user_screen_name,tweet_created_at) %>% 
  mutate(part_uk_operation = user_screen_name %in% celllist$user_screen_name)

ap_susp$new_status = NA
aP_susp$new_status[ap_susp$final_status == "suspeded" & ap_susp$user_screen_name %in% celllist$user_screen_name]

xxx
table(ap_susp$user_screen_name) %>% sort(decreasing = T) %>% .[1:85]
top85 <- table(ap_susp$user_screen_name) %>% sort(decreasing = T) %>% .[1:85] %>% names()

apsusp2 <- 
  ap_susp%>%
  mutate(tweet_created_at = tweet_created_at %>% as.character()) %>%
  filter(user_screen_name %in% top85 ) %>%
  mutate(user_screen_name = factor(user_screen_name,levels = top85)) %>% 
  group_by(user_screen_name) %>%
  group_split(user_screen_name) %>%
  set_names(top85)

writexl::write_xlsx(apsusp2,"data_exchange/AP04_raw_suspended_top1percent_85_mostactivesuspended.xlsx")

###########################

uk_new <- 
  df_sub %>% 
  mutate(part_uk_operation = user_screen_name %in% celllist$user_screen_name) %>%
  filter(part_uk_operation == T) %>% 
  filter(what == "retweet") %>% 
  filter(!is.na(retweet_dip)) %>% 
  filter(!final_status == "suspended") %>% 
  select(tweet_created_at,user_screen_name,final_status,full_text,target,what) %>%
  group_by(user_screen_name) %>% 
  mutate(n_user = n()) %>%
  arrange(desc(n_user),user_screen_name,tweet_created_at) 

new <- table(uk_new$user_screen_name) %>% sort(decreasing = T)  %>% names()

uk_new2 <- 
  uk_new %>%
  mutate(tweet_created_at = tweet_created_at %>% as.character()) %>%
  filter(user_screen_name %in% new ) %>%
  mutate(user_screen_name = factor(user_screen_name,levels = new)) %>% 
  group_by(user_screen_name) %>%
  group_split(user_screen_name) %>%
  set_names(new)

writexl::write_xlsx(apsusp2,"data_exchange/AP05_later_suspended_afterourtipoff_uk.xlsx")


########################################################

### LORENZ CURVE
#install.packages("gglorenz")
library(gglorenz)
library(ggforce)

system.time({
  cumulative <- 
    rts %>% 
    #filter(final_status == "suspended") %>%
    filter(whom == "diplomat") %>%
    group_by(whom,user_id,user_screen_name) %>% 
    summarise(n = n()) %>% 
    arrange(n) %>% 
    ungroup() %>%
    mutate(cumsum = cumsum(n)) %>%
    arrange(desc(n)) %>% 
    mutate(rev_cumsum = cumsum(n))  %>%
    arrange(n) 
})


dip_rts_cumulative_bydip <- 
  rts %>% 
  filter(whom == "diplomat") %>%
  group_by(whom,target,user_id,user_screen_name,final_status) %>% 
  summarise(n = n()) %>% 
  arrange(n) %>% 
  group_by(target) %>%
  mutate(cumsum = cumsum(n)) %>%
  arrange(desc(n)) %>% 
  mutate(rev_cumsum = cumsum(n))  %>%
  arrange(n)  %>%
  group_by(target) %>%
  mutate(perc = n/sum(n,na.rm = T)) #



###############



library(DescTools)

ginidf <-
  map(.x = dips,
    .f = function(dip){
      
      gi <-
        dip_rts_cumulative_bydip %>%
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
rownames(ginidf) <- NULL

saveRDS(ginidf,"data/processed/gini.rds")


##########################################################################





gglo <- 
  cumulative%>%
  ggplot(aes(x = n)) +
  stat_lorenz(desc = T,geom = "polygon", alpha = 0.65,fill = "salmon") + 
  scale_y_continuous(labels = scales::percent_format(suffix = ""))+
  scale_x_continuous(labels = scales::percent_format(suffix = ""))+
  geom_abline(linetype = "dashed") +
  coord_fixed() +
  facet_zoom(xlim = c(0,0.1))+
  annotate(geom = "point", color = "blue",size = 2,
           x = 0.01, y = 1-quantile(cumulative$cumsum,0.99)/sum(cumulative$n))+
  annotate(geom = "point", color = "blue",size = 2,
           x = 0.001, y = 1-quantile(cumulative$cumsum,0.999)/sum(cumulative$n))+
  annotate(geom = "point", color = "blue",size = 2,
           x = 0.05, y = 1-quantile(cumulative$cumsum,0.95)/sum(cumulative$n))+
  annotate(geom = "point", color = "blue",size = 2,
           x = 0.1, y = 1-quantile(cumulative$cumsum,0.90)/sum(cumulative$n))+
  annotate(
    geom = "curve", x = 0.03, xend = 0.0105, 
    y = 0.25, yend = 0.45, alpha = 1,
    color = "blue",
    curvature = -.3,
    size = 1.,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(
    geom = "curve", x = 0.015, xend = 0.048,
    y = 0.8, yend = 0.645, alpha = 1,
    color = "blue",
    curvature = .15,
    size = 1.,
    arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", color = "blue",
           x = 0.00, y = 0.1,
           label =paste0("0.1% most active users(n = ",scales::comma(round(0.001*nrow(cumulative))),"):\n",
                         round(100*(1-quantile(cumulative$cumsum,0.999)/sum(cumulative$n)),1),
                         "% (",scales::comma(round(quantile(cumulative$rev_cumsum,0.001))),") of DIP-RTs."), hjust = "left")+
  annotate(geom = "text", color = "blue",
           x = 0.03, y = 0.25, 
           label =paste0("1% of most active retweeters (",scales::comma(round(0.01*nrow(cumulative))),"):\n",
                         round(100*(1-quantile(cumulative$cumsum,0.99)/sum(cumulative$n)),0),
                         "% (",scales::comma(round(quantile(cumulative$rev_cumsum,0.01))),") of diplomat retweets."), hjust = "left")+
  annotate(geom = "text", color = "blue",
           x = 0.00, y = 0.9,
           label =  paste0("The 5% of most active retweeters (",scales::comma(round(0.05*nrow(cumulative))),")\naccounted for ",
                           round(100*(1-quantile(cumulative$cumsum,0.95)/sum(cumulative$n)),1),
                           "% (",scales::comma(round(quantile(cumulative$rev_cumsum,0.05))),")\nof diplomat retweets"), hjust = "left")+
annotate(geom = "text", color = "blue",
         x = 0.07, y = 0.5,
         label =paste0("10% most active:\n",
                       round(100*(1-quantile(cumulative$cumsum,0.90)/sum(cumulative$n)),1),
                       "% of DIP-RTs."), hjust = "left")+
annotate(
  geom = "curve", x = 0.085, xend = 0.098,
  y = 0.6, yend = 0.74, alpha = 1,
  color = "blue",
  curvature = -.15,
  size = 1.,
  arrow = arrow(length = unit(2, "mm"))
)  +
annotate(
  geom = "curve", x = 0.006, xend = 0.002,
  y = 0.2, yend = 0.25, alpha = 1,
  color = "blue",
  curvature = .3,
  size = 1.,
  arrow = arrow(length = unit(2, "mm"))
)  +
  labs(x = "Percent of Most Active Diplomat Retweeters",
       y = "Cumulative Percentage All Diplomat Retweets",
       title = "Few Superspreaders Account for Majority of Diplomat Amplification",
       subtitle = "Bottom panel zooms in to top 10% of most active retweeters")+
  theme_bw() + 
  theme(legend.position = "right",
           plot.title = ggplot2::element_text(family="Roboto",
                                              size=16,
                                              face="bold",
                                              color = oxblue),
           #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
           plot.subtitle = ggplot2::element_text(family="Roboto",
                                                 size=14,
                                                 face = "bold",
                                                 color= "#007fc8",#oxblue,
                                                 margin=ggplot2::margin(0,0,5,0)),
           plot.caption = ggplot2::element_blank()) + 
  labs(x = "Percent of Most Active Diplomat Retweeters",
       y = "Cumulative Percentage of All Diplomat Retweets",
       title = NULL,
       subtitle = NULL)+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

gglo


pb <- ggplot_build(gglo)
pb$data[[7]][1, 'alpha'] <- 0
pb$data[[8]][1, 'alpha'] <- 0
pb$data[[8]][2, 'alpha'] <- 0
pb$data[[9]][1, 'alpha'] <- 0
pb$data[[9]][2, 'alpha'] <- 0
pb$data[[10]][1, 'alpha'] <- 0
pb$data[[11]][1, 'alpha'] <- 0
pb$data[[11]][2, 'alpha'] <- 0
pb$data[[12]][1, 'alpha'] <- 0
pb$data[[12]][2, 'alpha'] <- 0
pb$data[[13]][1, 'alpha'] <- 0
pb$data[[13]][2, 'alpha'] <- 0
pb$data[[14]][1, 'alpha'] <- 0
pb$data[[14]][2, 'alpha'] <- 0

gglo2 <- ggplot_gtable(pb)

plot(gglo2)


ggsave(filename = "manuscript/results/lorenz.pdf",
       plot = plot(gglo2),
       height = 8,
       width = 8)
browseURL("manuscript/results/lorenz.pdf")


ggsave(filename = "manuscript/word/lorenz2.pdf",
       plot = plot(gglo2),
       height = 8,
       width = 8)
browseURL("manuscript/word/lorenz2.pdf")
##################################################

xiaojin <- 
  df_sub %>%
  filter(user_screen_name %in% c("Xiaojin05484077")) %>%
  filter(!is.na(status)) %>%
  arrange(tweet_created_at) %>% 
  select(user_screen_name,retweet_dip,tweet_created_at) %>%
  mutate(hour = format((tweet_created_at %>% floor_date("hour")),"%H") ,
         month = format((tweet_created_at %>% floor_date("day")),"%m"),
         day = format(tweet_created_at %>% floor_date("day"),"%Y-%m-%d") )%>% 
  mutate(day_hour = paste0(day," ",hour,"h")) %>% 
  group_by(day_hour) %>%
  arrange(tweet_created_at) %>%
  mutate(lag_x = lag(tweet_created_at,1)) %>%
  mutate(lagdif = as.numeric(tweet_created_at - lag_x)) %>%
  mutate(lagmid = tweet_created_at - seconds(0.5*lagdif)) %>% 
  group_by(day_hour) %>%
  mutate(label = paste0(day_hour," : ",n(), " Retweets of @AmbLiuXiaoming within ",
                        as.numeric(as.duration(max(tweet_created_at) - min(tweet_created_at)))," seconds"))

xiaojin%>%
  #filter(month == "06") %>%
  group_by(day,hour,day_hour) %>%
  filter(max(lagdif,na.rm = T)< 30) %>%
  summarise(n = n()) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>%
  slice(1:8)  -> these

library(ggrepel)

ggxi <- 
  ggplot(xiaojin %>% filter(day_hour %in% these$day_hour)
         ) + 
  geom_point(aes(x = tweet_created_at,
                 y = 1)) +
  facet_wrap(~label,scales = "free_x",ncol = 1) +
  geom_curve(data = function(df){df %>% filter(!is.na(lag_x)) %>% distinct(tweet_created_at,.keep_all = T)},
             aes(x = lag_x,xend = tweet_created_at,y = 1.0,yend = 1.0),
             curvature = -0.3,
             linetype = "dashed",
             arrow = arrow(length = unit(0.5, "mm")),show.legend = FALSE
  ) + 
  geom_label(aes(x = lagmid,
                y = 1.005,
                label = lagdif),size = 2.5,label.padding = unit(0.1, "lines"))+
  scale_y_continuous(limits = c(0.993,1.009))+
  scale_x_datetime(date_breaks = "30 sec",
                   date_labels = "%H:%M:%S") + 
  labs(x = "Time",
       y = NULL,
       title = "Selected Retweet Chains of @Xiaojin05484077 Retweeting @AmbLiuXiaoMing",
       subtitle = "Suspended @Xiaojin05484077 retweeted Chinese UK-Ambassador 1166 times between June and Sept '20") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank() ,
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
  labs(title = NULL,
       subtitle = NULL)


ggxi

row1 <- "Little Number labels signify seconds between two consecutive retweets"
row2 <- "The showcased behaviour occured on nearly every day between June 9th and September 10th 2020"
row3 <- "@Xiaojin05484077 was later found to be part of a coordinated amplification network (See Section 3.3)"

oii_finalise_plot(ggxi, 
                  source_name = paste0(row1,"
",row2,"
",row3,""), 
                  save_filepath = "manuscript/word/chain_retweeter2.pdf",
                  width_pixels = 1200/1.4, 
                  height_pixels = 1200/1.4,  
                  height_footer = 0.081,
                  dpi = 1000,
                  scale = 1,
                  footer_start = 0.9)
browseURL("manuscript/word/chain_retweeter2.pdf")

####

## Density lagdif

diprts <- 
  rts %>%
  filter(!is.na(retweet_dip)) %>%
  group_by(user_screen_name) %>%
  mutate(n_user = n()) %>%
  ungroup()


top01 <- 
  diprts %>% 
  filter(n_user >= quantile(diprts %>% distinct(user_id,.keep_all = T) %>% pull(n_user),0.99)) #%>% filter(n_user > 100)

top01b <- 
  top01 %>%
  group_by(user_screen_name,final_status,n_user) %>%
  mutate(lagtime = lag(tweet_created_at,1),
         lagdif = as.numeric(as.duration(tweet_created_at - lagtime))) %>%
  summarise(medlagdif = median(lagdif,na.rm = T)) %>%
  filter(final_status != "deleted")

top01b %>%
  group_by(final_status) %>% 
  summarise(p = sum(medlagdif <= 10)/n(),
            s = sum(medlagdif <= 10),
            n = n())

all_lags <- 
  diprts %>%
  group_by(user_screen_name,final_status,n_user) %>%
  mutate(lagtime = lag(tweet_created_at,1),
         lagdif = as.numeric(as.duration(tweet_created_at - lagtime))) %>%
  summarise(medlagdif = median(lagdif,na.rm = T),
            sd_lagdif = sd(lagdif,na.rm = T)) 
saveRDS(all_lags,"data/processed/all_rt_lags.rds")

all_lags <- readRDS("data/processed/all_rt_lags.rds")

all_lags %>%
  ungroup() %>%
  #group_by(final_status) %>% 
  filter(n_user > 100) %>%
 # filter(medlagdif <= 10) %>% View()
  summarise(p = sum(medlagdif <= 10,na.rm = T)/n(),
            s = sum(medlagdif <= 10,na.rm = T),
            p60 = sum(medlagdif <= 60,na.rm = T)/n(),
            s60 = sum(medlagdif <= 60,na.rm = T),
            n = n()) 

q_01 <- quantile(zlj_cumulative$n,0.99,type = 1)
q_001 <- quantile(zlj_cumulative$n,0.999,type = 1)



ggdens <- 
  ggplot(top01b %>% filter(medlagdif < 3600)) + 
  geom_point(aes(x = medlagdif,y = n_user,col = final_status,alpha =final_status)) + 
  #scale_x_continuous(limits = c(0,3600)) + 
  facet_zoom(xlim = c(0,60),zoom.size = 1)+
  scale_color_manual(name = "Account Status\non 1st of March",values = c("green","red")) + 
  scale_alpha_manual(name = "Account Status\non 1st of March",breaks  = c("active","suspended"),values = c(0.2,1)) +
  scale_y_log10(labels = scales::comma)+
  labs(x = "Median Seconds between two Retweets",
       y = "Total Retweets of Diplomats",
       title = "Many High-Frequency Super-Spreaders Remain Active",
       subtitle = "Total Number of Diplomat Retweets and Median Time between Retweets") + 
  theme(legend.position = "right",
        plot.title = ggplot2::element_text(family="Roboto",
                                           size=19,
                                           face="bold",
                                           color = oxblue),
        #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
        plot.subtitle = ggplot2::element_text(family="Roboto",
                                              size=14,
                                              face = "bold",
                                              color= "#007fc8",#oxblue,
                                              margin=ggplot2::margin(0,0,5,0)),
        plot.caption = ggplot2::element_blank()) +
  annotate(geom = "text", color = "red",
           x = 13, y = 1500, size = 4,
           label ="@Xiaojin05484077: Retweeting\ndiplomats 1060 times with\nmedian lag of 3 seconds", hjust = "left")+
  annotate(
    geom = "curve", x = 13, xend = 4, 
    y = 1500, yend = 1170, alpha = 1,
    color = "red",
    curvature = .3,
    size = 1.,
    arrow = arrow(length = unit(2, "mm"))
  )  +
  theme(legend.position = c(0.8,0.9))
ggdens

pb <- ggplot_build(ggdens)
pb$data[[2]][1, 'alpha'] <- 0
pb$data[[3]][1, 'alpha'] <- 0
gglo2 <- ggplot_gtable(pb)

ggsave(filename = "manuscript/results/chain_density2.pdf",
       plot = plot(gglo2),
       height = 8,
       width = 8)
browseURL("manuscript/results/chain_density2.pdf")



### Graphika Overlap

graphika <- 
  readxl::read_xlsx("data/input_selectors/graphika_assets.xlsx")

gr <- df_sub
gr$target <- NA
gr$target[!is.na(gr$retweet_dip)] <- gr$retweet_dip[!is.na(gr$retweet_dip)]
gr$target[!is.na(gr$quoted_dip)] <- gr$quoted_dip[!is.na(gr$quoted_dip)]
gr$target[!is.na(gr$reply_dip)] <- gr$reply_dip[!is.na(gr$reply_dip)]

gr2 <- 
  gr %>%
  filter(user_screen_name %in% graphika$username | 
         target %in% graphika$username) %>% 
  left_join(graphika %>% select(username,graphika_source = source,page),
            by = c("user_screen_name" = "username"))

gr %>% 
  select(user_name,user_description,user_created_at,final_status,
         user_screen_name, tweet_created_at,target,retweet_dip,
         full_text,retweet_id) %>%
  group_by(user_screen_name) %>%
  mutate(n = n()) %>%
  # filter(n > 100) %>%
  #filter(final_status == "suspended") %>%
  View()

gr2 %>%
  group_by(target) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>% filter(!is.na(target)) %>% View()


gr2 %>% 
  filter(!is.na(retweet_dip)|!is.na(quoted_dip)|!is.na(reply_dip)) %>%
  group_by(user_screen_name,graphika_source,page) %>% 
  summarise(n = n()) %>% View()

gr2 %>% 
  filter(!is.na(retweet_dip)|!is.na(quoted_dip)|!is.na(reply_dip)) %>%
  group_by(user_screen_name,graphika_source,page,target) %>% 
  summarise(n = n()) %>% View()

forgephi2 <- 
  gr2 %>% 
  ungroup() %>%
  filter(!is.na(retweet_dip)|!is.na(quoted_dip)|!is.na(reply_dip)) %>%
  group_by(user_screen_name,graphika_source,page,target) %>% 
  summarise(n = n()) %>%
  left_join(accs %>% filter(platform == "twitter") %>% select(type_source = type,handle),
            by = c("user_screen_name" = "handle")) %>%
  left_join(accs %>% filter(platform == "twitter") %>% select(type_target = type,handle),
            by = c("target" = "handle")) %>%
  ungroup() %>%
  mutate_at(vars(contains("type_")),funs(ifelse(is.na(.),"other",.))) %>% 
  rename(Source = user_screen_name ,
         Target = target)

## aggregated

edges2 <- 
  forgephi2 %>% 
  ungroup() %>%
  na.omit() %>% 
  ungroup() %>% 
  group_by(Source) %>%
  mutate(n_source = sum(n,na.rm = T)) %>%
  ungroup() #%>% filter(n >1)

edges2$Weight <- edges2$n
write_csv(edges2,"data/gephi/gephi_edges_graphika.csv")

# nodes
# Id column || 

indeg <- 
  gr2$target %>% table() %>% .[. >25] %>% names()

indeg2 <- 
  gr2$user_screen_name %>% table() %>% .[. >25] %>% names()



which_nodes <- 
  c(edges2$Source,edges2$Target) %>% 
  unique() %>% data.frame(user_id = .)

nodes2 <- 
  bind_rows(
    edges2 %>% select(Id = Source,label = Source,status = type_source,page),
    edges2 %>% select(Id = Target,label = Target,status = type_target,page)
  ) %>% 
  distinct(Id,.keep_all = T) %>% 
  filter(!is.na(Id)) %>%
  mutate(label = ifelse(Id %in% indeg,
                        Id,
                        ifelse(status == "other",
                               paste0(Id," (",page %>% str_replace_all("p","p."),")"),
                               "")))

write_csv(nodes2,"data/gephi/gephi_nodes_graphika.csv")

# 
tab <- 
  gr2 %>% 
  filter(!is.na(retweet_dip)|!is.na(quoted_dip)|!is.na(reply_dip)) %>%
  group_by(user_screen_name,page,target) %>% 
  summarise(n = n()) %>%
  group_by(user_screen_name) %>%
  mutate(n_total = sum(n)) %>% 
  arrange(desc(n)) %>% 
  slice(1:3) %>% 
  mutate(pos = 1:3) %>%
  mutate(nr = paste0(target," (",n,")")) %>%
  select(-target,-n) %>%
  pivot_wider(names_from = pos,
              values_from = nr) %>% 
  arrange(desc(n_total))  %>% 
  mutate(page = str_replace_all(page,"p","p.")) %>% 
  mutate_at(vars(user_screen_name,`1`,`2`,`3`),
            funs(str_replace_all(.,"_","\\\\_")))


tab %>% writexl::write_xlsx("manuscript/word/graphikatab1.xlsx")

colnames(tab) <- 
  c("User Handle","Source","Diplomat Amplifications","Most Amplified Dip.","2nd Most Amplified Dip.","3rd Most Amplified Dip.") %>%
  paste0("\\textbf{",.,"}")


tt <- xtable(tab,
             align = "l||p{3cm}|p{1.2cm}|p{2.5cm}|p{4cm}|p{4cm}|p{4cm}|"
)

comment          <- list()
comment$pos      <- list()

hlines <- c(-1:nrow(tab))


print(tt, 
      caption.placement = "top",
      include.rownames = F, 
      floating = F,
      booktabs = T,
      sanitize.text.function = identity,
      #add.to.row = comment,     # this is where you actually make the substitution
      hline.after = hlines,
      file="manuscript/results/graphikatable.tex")


#################

