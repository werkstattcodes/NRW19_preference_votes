library(tidyverse)
library(ggforce)
library(extrafont)
library(paletteer)
loadfonts(device = "win", quiet=T)
extrafont::fonts()
extrafont::font_import()
library(hrbrthemes)
library(ggtext)

wdr <- getwd()
my_caption <- "Roland Schmidt | @zoowalk | http://werk.statt.codes"


# get Landes- and Regionalwahlkreise --------------------------------------

df_laender_regio <- readxl::read_xlsx(path=paste0(wdr, "/data/NRW19_Vorzugsstimmen_Landes_Regionalparteiliste_16102019.xlsx")) 

names(df_laender_regio) <- names(df_laender_regio) %>% 
  stringr::str_remove(., "\r") %>% 
  stringr::str_remove(., "\n") %>%
  stringr::str_remove(., "-") 

df_laender_regio <- df_laender_regio %>% 
  janitor::clean_names()


df_laender_regio_long <- df_laender_regio %>% 
  mutate(party=map_chr(reihung_1, ~str_extract(., regex("^List.*")))) %>% 
  mutate(landesparteiliste=map_chr(reihung_1, ~str_extract(., regex("^Landespartei.*")))) %>% 
  mutate(regionalparteiliste=map_chr(familien_vorname_2, ~str_extract(., regex("^Regionalpartei.*")))) %>% #mess in bmi file; heading in different columns
  mutate(regionalparteiliste_2=map_chr(reihung_1, ~str_extract(., regex("^Regionalpartei.*"))))  %>% 
  mutate(gebiet=coalesce(landesparteiliste, regionalparteiliste, regionalparteiliste_2)) %>%   
  janitor::remove_empty("rows") %>% 
  fill(., gebiet, .direction=c("down")) %>% 
  fill(., party, .direction=c("down")) %>% 
  select(-c("x6", "landesparteiliste", "regionalparteiliste", "regionalparteiliste_2")) %>% 
  filter(str_detect(reihung_1, "^[0-9]")) %>% 
  select(gebiet, party, everything()) %>% 
  rename_at(vars(reihung_7:vorz_stimm_11), 
            function(x) str_replace_all(x, "[:digit:]+", "b")) %>% 
  rename_at(vars(reihung_1:vorz_stimm_5), 
            function(x) str_replace_all(x, "[:digit:]+", "a")) %>% 
  mutate_at(vars(reihung_a:vorz_stimm_b), as.character) %>% 
  pivot_longer(cols=reihung_a:vorz_stimm_b, names_to = c(".value", "result"),
               names_pattern = "(.*)_([a|b]$)") %>% 
  mutate_at(vars(reihung, geb_jahr, vorz_stimm), as.numeric) %>% 
  group_by(gebiet, party) %>% 
  arrange(reihung, .by_group=T) %>% 
  mutate(kreis=case_when(str_detect(gebiet, regex("Landes.*")) ~ "Landeswahlkreis",
                         str_detect(gebiet, regex("^Regional.*")) ~ "Regionalwahlkreis",
                         TRUE ~ NA_character_)) %>% 
  select(kreis, gebiet, party, everything(), -result) %>% 
  rename(name=familien_vorname,
         stimmen=vorz_stimm) %>% 
  ungroup()
         

# get Bundeswahlkreis --------------------------------------

df_bund <- readxl::read_xlsx(path=paste0(wdr, "/data/NRW19_Vorzugsstimmen_bundesweit_16102019.xlsx"),
                             col_name=F,
                             trim_ws=T) 

names(df_bund) <- c("Nr", "Familienname, Vorname", "Geb.-Jahr", "Beruf", 
                     "Gesamt", "B", "K", "NÖ", "OÖ", "S", "Stmk", "Tirol",
                     "Vlbg", "Wien")


df_bund_clean <- df_bund %>% 
  janitor::remove_empty(., which=c("rows")) %>% 
  filter(!Nr=="Nr.") %>% 
  mutate(party=str_extract(Nr, regex("[:alpha:].*"))) %>% 
  fill(., party, .direction=c("down")) %>% 
  filter(!str_detect(Nr, regex("[:alpha:]"))) %>% 
  mutate(kreis="Bundeswahlkreis") %>% 
  pivot_longer(cols=Gesamt:Wien, names_to="gebiet", values_to = "stimmen") 


#rename to be consistent with regional/state level
df_bund_clean <- df_bund_clean %>% 
  select(kreis, gebiet, party, reihung="Nr", name="Familienname, Vorname", geb_jahr="Geb.-Jahr",
         beruf="Beruf", everything()) %>% 
  mutate_at(vars("reihung", "geb_jahr", "stimmen"), ~stringr::str_trim(., side=c("both"))) %>% 
  mutate_at(vars("reihung", "geb_jahr", "stimmen"), as.numeric)


# bind both bund, laender and regio -------------------------------------------------------------------

df_merge <- bind_rows(df_laender_regio_long, 
                      df_bund_clean) %>% 
  ungroup()

fn_remove_titles <- function(x) {
  
  titles <- c("Mag", "MMag", "Prof", "Dr", "O\\.Univ\\.Prof", "Dipl-Ing", "Dipl.-Ing.", "Dipl.-Rev.", "Ing", 
              "Dipl", "jun","sen", "Ba", "\\(FH\\)", "MSc",
              "med", "univ", "Bsc", "phil", "iur", "DDr", "BEd", "DI", "Mba", "Mas", "MA", "MIM",
              "vet", "M.A.", "PMM", "MTD", "Päd", "Bakk") %>%
    map(., paste0, c("\\.", "\\s", "\\,")) %>% unlist()
    
  
  #   map(c("\\s","\\.", "\\,"), paste0, .) %>%
  #   unlist() %>%
  #   unlist() %>%
  #   paste(., collapse="|")
  # 
  # name_wo_titles <- stringr::str_remove_all(x, regex(titles, ignore_case=T)) %>%
  #   stringr::str_remove_all(., regex("\\(FH\\)", ignore_case=T)) %>%
  #   stringr::str_squish()
  
  
  name_wo_titles <- stringr::str_remove_all(x, regex("[[:alpha:]\\-]+\\.")) %>% 
    str_remove_all(., regex("\\([:alpha:]+\\)")) %>% 
    str_remove_all(., regex(paste0(titles, collapse="|"), ignore_case = T)) %>% 
    str_squish()
  

}


u <- df %>% 
  distinct(name) %>% 
  mutate(clean_names=map_chr(name, fn_remove_titles))


fn_short_name <- function(x) {
  
  family_name <- stringr::str_extract(x, "^[[:alpha:]\\-|\\s]+(?=,)") %>% 
    str_to_title() %>% 
    str_squish() %>% str_trim()
  
  first_name <- str_extract(x, regex("(?<=\\,)[[:alpha:]\\-\\s]*$")) %>% 
    str_trim() %>% 
    str_squish() %>% 
    str_to_title()
  
  initals <- str_split(first_name, "\\s") %>% 
    map(., str_sub, start=1L, end=1L) %>% 
    map_chr(., str_c, collapse=".") %>% 
    paste0(., ".")
  
  name_short <- paste(family_name, initals, sep=", ")
  
}


df <- df_merge %>% 
  mutate(gebiet=str_remove(gebiet, "Landesparteiliste") %>% 
           str_remove(., "Regionalparteiliste") %>% 
           str_trim(., side=c("both"))) %>% 
  mutate(gebiet=case_when(gebiet=="B" ~ "Burgenland",
                          gebiet=="S" ~ "Salzburg",
                          gebiet=="K" ~ "Kärnten",
                          gebiet=="NÖ" ~ "Niederösterreich",
                          gebiet=="OÖ" ~ "Oberösterreich",
                          gebiet=="Stmk" ~ "Steiermark",
                          gebiet=="Vlbg" ~ "Vorarlberg",
                          TRUE ~ gebiet)) %>% 
  mutate(gebiet=as_factor(gebiet)) %>% 
  mutate(party2=str_extract(party, regex("\\([:alpha:]*\\)")) %>%  #take parties' short abbrevs.
           str_remove_all(., "\\(|\\)")) %>% 
  # mutate(name=str_remove_all(name, regex("[:alpha:]*\\.")) %>% 
  #          str_squish() %>% 
  #          str_to_title()) %>%   #remove academ titles/consistency of names
  # mutate(name=str_remove_all(name, "\\s*(?=\\,)")) %>%  #remove space before commas; consistency of words
  # #mutate(family_name=stringr::word(name, 1)) %>% 
  # #mutate(first_name=str_extract(name, "(?<=\\,).*$")) %>% 
  # mutate(family_name=stringr::str_extract(name, "^.+(?=,)")) %>% 
  # mutate(first_name=str_extract(name, regex("(?<=\\,).*$")) %>% 
  #          str_trim()) %>% 
  # mutate(initals=str_split(first_name, "\\s") %>% 
  #          map(., str_sub, start=1L, end=1L) %>% 
  #          map_chr(., str_c, collapse=".") %>% 
  #          paste0(., ".")) %>% 
  # mutate(name_short=paste(family_name, initals, sep=", ")) %>% 
  mutate(name_org=name) %>% 
  mutate(name=map_chr(name_org, fn_remove_titles)) %>% 
  mutate(name_short=map_chr(name, fn_short_name)) %>% 
  mutate(bundesland=case_when(str_detect(gebiet, "^[0-9].*") ~ str_sub(gebiet, start=1L, end=1L),
                              TRUE ~ as.character(gebiet))) %>% 
  mutate(bundesland=case_when(bundesland==1 ~ "Burgenland",
                              bundesland==2 ~ "Kärnten",
                              bundesland==3 ~ "Niederösterreich",
                              bundesland==4 ~ "Oberösterreich",
                              bundesland==5 ~ "Salzburg",
                              bundesland==6 ~ "Steiermark",
                              bundesland==7 ~ "Tirol",
                              bundesland==8 ~ "Vorarlberg",
                              bundesland==9 ~ "Wien",
                              TRUE ~ NA_character_)) %>% 
  mutate(kreis=fct_relevel(kreis, "Bundeswahlkreis", "Landeswahlkreis", "Regionalwahlkreis"))

#name corrections
df <- df %>% 
  mutate(name=case_when(str_detect(name, "Pistracher") ~ "Pistracher, Gerald",
                        TRUE ~ as.character(name)))



main_parties <- c("SPÖ", "ÖVP", "NEOS", "GRÜNE", "FPÖ")

df <- df %>% 
  mutate(party3=fct_other(party2, keep=main_parties) %>% 
           fct_relevel(., "ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS","Other"))


# ANALYSIS ----------------------------------------------------------------


# where do parties get preferential votes ---------------------------------

df %>% 
  filter(party2 %in% main_parties) %>% 
  # group_by(party, kreis, gebiet, .drop=T) %>% 
  # summarise(sum_votes=sum(stimmen, na.rm = T)) %>% 
  ungroup() %>% 
  filter(!str_detect(gebiet, "Gesamt")) %>% 
  filter(str_detect(party, "Kurz")) %>% 
  ggplot()+
  geom_bar(aes(x=gebiet, y=stimmen),
           stat="identity")+
  facet_wrap(vars(party, kreis),
             scales = "free_y")+
  coord_flip()
  
df %>% 
  filter(party2 %in% main_parties) %>% 
  filter(!str_detect(gebiet, "Gesamt")) %>% 
  ggplot()+
  geom_bar(aes(x=party2, 
               y=stimmen,
               fill=kreis),
           stat="identity",
           position="fill")+
  coord_flip()
  

# top 5 candidates per party ----------------------------------------------

top_10 <- df %>% 
  #filter(party2 %in% main_parties) %>% 
  filter(!str_detect(gebiet, "Gesamt")) %>% 
  group_by(party3, name) %>% 
  summarise(preferences_candidate=sum(stimmen)) %>% 
  arrange(desc(preferences_candidate), .by_group=T) %>% 
  slice(1:10)

df %>% 
  filter(name %in% top_10$name) %>% 
  filter(!str_detect(gebiet, "Gesamt")) %>% 
  group_by(party3, name, kreis) %>% 
  summarise(sum_candiate=sum(stimmen, na.rm=F)) %>% 
  group_by(party3) %>% 
  arrange(desc(sum_candiate), .by_group=T) %>% 
  ungroup() %>% 
  group_by(name) %>% 
  mutate(total_candidate=sum(sum_candiate)) %>% 
  ungroup() %>% 
  mutate(name=forcats::fct_reorder(name, total_candidate, .desc=F)) %>%  
  ggplot()+
  geom_bar(aes(x=name, 
               y=sum_candiate,
               fill=kreis),
           group=name,
           width=0.6,
           stat="identity")+
  geom_text(data=top_10 %>% filter(party3=="FPÖ"),
            aes(x=name,
                y=+Inf,
                label=scales::comma(preferences_candidate)),
            stat="identity",
            hjust=1,
            group=name)+
  # stat_summary(fun.y = sum, 
  #              aes(x=name,
  #                  y=sum_candiate,
  #                  label = ..y.., 
  #                  group = name), 
  #              hjust=-5,
  #              geom = "text")+
  labs(caption = paste("Data: bmi.gv.at/412/Nationalratswahlen/Nationalratswahl_2019/start.aspx#vorzugsstimmen", "<br>","Roland Schmidt | @zoowalk | <span style='color:orange'>**werk.statt.codes**</span>"),
       title="NRW 2019:KandidatInne mit den meisten Vorzugsstimmen (Top 10)",
       y="Anzahl Vorzugsstimmen in 1000er")+
  scale_x_discrete(label=fn_short_name)+
  scale_y_continuous(label=scales::comma_format(scale=0.001),
                     expand=expansion(mult=c(0, 0.3)),
                     breaks=seq(0,90000,30000),
                     minor_breaks = NULL)+
  scale_fill_paletteer_d("ggsci::default_jama")+
  facet_wrap(vars(party3),
             ncol=2,
             scale="free_y")+
  coord_flip()+
  hrbrthemes::theme_ipsum_rc()+
  theme(panel.grid.major.y = element_blank(),
        axis.title.y = element_blank(),
        plot.caption = element_markdown(color="grey30"),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.4, units=c("cm")),
        legend.justification = "right",
       # axis.text.y = element_text(face=x),
        strip.text = element_text(face="bold"))+
  guides(fill=guide_legend(rev=T, ))


name <- "FPÖ_top_10_candidates"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 15
width  <- 20
ggsave(filename=paste0(folder, time, name, format),
        height=height,
        width=width,
       scale=1,
       unit=c("cm"))


Hofer <- df %>% 
  filter(str_detect(name_org, "^Hofer")) %>% 
  filter(party3=="FPÖ") %>% 
  distinct(name_org, kreis, gebiet)


# top per kreis -----------------------------------------------------------

fn_top_plot <- function(x) {  
  x %>% 
  ggplot() +
  geom_bar(aes(x=name,
               y=stimmen),
           stat="identity")+
  labs(title=paste(unique(x$kreis)))+  
  scale_x_discrete(labels=fn_short_name)+  
  hrbrthemes::theme_ipsum_ps()+  
  facet_wrap(vars(gebiet),
            scales="free_y")+
  coord_flip()
  }

list_plots <- df %>% 
  filter(!gebiet=="Gesamt") %>% 
  group_by(kreis, gebiet) %>% 
  arrange(desc(stimmen), .by_group=T) %>% 
  slice(1:3) %>% 
  ungroup() %>% 
  group_split(kreis) %>% 
  map(~fn_top_plot(.))

list_plots

library(patchwork)
patchwork::wrap_plots(list_plots[1:2])+
  plot_layout(ncol=1)

ggsave("test.png", height=20, width=10)



# difference btw ranking and number of preferential votes -----------------
names(df)

rank_diff <- df %>% 
  filter(!(kreis=="Bundeswahlkreis" & gebiet!="Gesamt")) %>% 
  group_by(party3, kreis, gebiet) %>% 
  arrange(desc(stimmen), .by_group=T) %>% 
  mutate(rank_votes=row_number()) %>% 
  select(kreis, gebiet, party3, reihung, rank_votes, name, stimmen) %>% 
  mutate(diff=reihung-rank_votes) %>% 
  arrange(desc(diff)) %>% 
  slice(1:3)

rank_diff <- df %>% 
  filter(!(kreis=="Bundeswahlkreis" & gebiet!="Gesamt")) %>% 
  group_by(party3, name) %>% 
  summarise(stimmen=sum(stimmen)) %>% 
  arrange(desc(stimmen), .by_group=T)




rank_diff

rank_diff %>% 
  filter(party3 %in% main_parties) %>% 
  #filter(str_detect(kreis, "Bundes")) %>% 
  pivot_longer(cols=c(reihung, rank_votes),
               values_to = "rank",
               names_to = "ranking") %>% 
  ggplot()+
  geom_point(aes(x=fct_rev(ranking), 
                 y=rank,
                 group=name,
                color=party3))+
  geom_line(aes(x=fct_rev(ranking), y=rank,
                group=name,
                color=party3))+
  ggrepel::geom_text_repel(aes(x=fct_rev(ranking), 
                               y=rank,
                group=name,
                label=name,
                color=party3),
            size=2)+
  facet_grid(rows=vars(party3), cols=vars(kreis))




# composition of preferential votes ---------------------------------------

df %>% 
  filter(!(kreis=="Bundeswahlkreis" & gebiet!="Gesamt")) %>% 
  group_by(kreis) %>% 
  summarise(votes_kreis=sum(stimmen, na.rm=T)) %>% 
  arrange(desc(votes_kreis)) %>% 
  mutate(cum_votes_kreis=cumsum(votes_kreis)) %>% 
  mutate(perc_votes_kreis=votes_kreis/sum(votes_kreis))%>% 
  mutate(kreis=fct_reorder(kreis, cum_votes_kreis) %>% fct_rev) %>% 
  {ggplot(data=.)+
  geom_bar(aes(x=1,
               y=votes_kreis,
               fill=kreis),
           stat="identity")+
  geom_text(aes(x=1,
                y=votes_kreis,
                label = paste(scales::comma(votes_kreis),
                              scales::percent(perc_votes_kreis),
                              sep="\n")),
            position=position_stack(vjust=.5, reverse=T))+
  scale_y_continuous(expand=expansion(mult=c(0,0)),
                     breaks=max(.$cum_votes_kreis),
                     label=scales::comma(max(.$cum_votes_kreis)))+
  #scale_fill_viridis_d()+
  scale_fill_paletteer_d("ggsci::default_aaas")+
  labs(title="Total number of preference votes per electoral district",
       y="number of preference votes")+
  hrbrthemes::theme_ipsum_tw()+
  theme(legend.position = "bottom",
        legend.justification = "right",
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank())+
  coord_flip()+
  guides(fill=guide_legend(reverse=T))}
  
name <- "Total_preferences_per_district"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 5
width <- 10
ggsave(filename=paste0(folder, time, filename, format),
     #  height=height,
    #   width=width,
       scale=1,
       unit=c("cm"))



# > per party -------------------------------------------------------------

x <- df %>% 
  filter(!(kreis=="Bundeswahlkreis" & gebiet!="Gesamt")) %>% 
  group_by(party3, kreis) %>% 
  summarise(votes_kreis=sum(stimmen, na.rm=T)) %>% 
  arrange(desc(votes_kreis)) %>% 
  mutate(cum_votes_kreis=cumsum(votes_kreis)) %>% 
  mutate(perc_votes_kreis=votes_kreis/sum(votes_kreis))%>% 
  ungroup() 


x %>% 
  ggplot()+
  geom_bar(aes(x=kreis,
               y=votes_kreis,
               fill=party3),
           stat="identity",
           position=position_stack())

x %>% 
  ggplot()+
  geom_bar(aes(x=party3,
               y=votes_kreis,
               group=kreis,
               fill=kreis),
           color=NA,
           stat="identity",
           position="fill")+
  geom_text(aes(x=party3,
                group=kreis,
                y=perc_votes_kreis,
                label=scales::percent(perc_votes_kreis, accuracy=0.1)),
            color="beige",
            position=position_stack(vjust=0.5, reverse = F))+
  labs(title = "Contribution of different electoral constituencies to total of preferential votes",
       caption=my_caption) +
  hrbrthemes::theme_ipsum_rc()+
  scale_fill_paletteer_d("ggsci::default_aaas")+
  #scale_fill_paletteer_d("awtools::a_palette")+
  scale_y_continuous(labels = scales::percent,
                     expand=expansion(mult=c(0, 0)))+
  scale_x_discrete(expand=expansion(mult=c(0, 0)))+  
  theme(panel.grid = element_blank(),
        legend.title= element_blank(),
        legend.justification = "right",
        axis.title = element_blank(),
        legend.position = "bottom")


name <- "Total_preferences_per_district_per_party"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 5
width <- 10
ggsave(filename=paste0(folder, time, name, format),
        # height=height,
        #  width=width,
       scale=1,
       unit=c("cm"))



# titanic plot on prefernetial votes --------------------------------------





# checks ------------------------------------------------------------------

x <- df %>% 
  filter(str_detect(name, "Lercher|Wagner")) %>% 
  distinct(name) %>% 
  # mutate(name2=str_extract(name, "(?<=Lercher).*")) %>% 
  # mutate(name3=str_extract(name, ".*(?=Maximilian)")) %>% 
  # mutate(name4=str_extract(name, ".*(?=,)")) %>% 
  # mutate(name5=str_remove_all(name, "\\s*(?=\\,)")) %>% 
  mutate(family_name=stringr::str_extract(name, "^.+(?=,)")) %>% 
  mutate(first_name=str_extract(name, regex("(?<=\\,).*$")) %>% 
         str_trim()) %>% 
  mutate(initals=str_split(first_name, "\\s") %>% 
           map(., str_sub, start=1L, end=1L) %>% 
           map_chr(., str_c, collapse=".") %>% 
           paste0(., ".")) %>% 
  mutate(name_short=paste(family_name, initals, sep=", "))
x

unique_names <- df %>% 
  mutate(first_name=stringr::word(name)) %>% 
  group_by(first_name) %>% 
  summarise(unique_names=length(unique(name))) %>% 
  filter(unique_names>1)
unique_names$first_name

x <- df %>% 
  mutate(first_name=stringr::word(name)) %>% 
  filter(first_name %in% unique_names$first_name)
  
  