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
           str_remove_all(., "\\(|\\)")) %>% mutate(name_org=name) %>% 
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

# top 5 candidates per party ----------------------------------------------

top_10 <- df %>% 
  #filter(party2 %in% main_parties) %>% 
  filter(!str_detect(gebiet, "Gesamt")) %>% 
  group_by(party3, name) %>% 
  summarise(preferences_candidate=sum(stimmen)) %>% 
  arrange(desc(preferences_candidate), .by_group=T) %>% 
  slice(1:10)

x <- c("plain", "bold", rep("plain", each=8))


df %>% 
  filter(name %in% top_10$name) %>% 
  filter(party3=="FPÖ") %>% 
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
       title="NRW 2019:\nFPÖ KandidatInnen mit meisten Vorzugsstimmen (Top 10)",
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
        plot.title.position = "plot",
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.key.size = unit(0.4, units=c("cm")),
        legend.justification = "right",
        axis.text.y = element_text(face=x),
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



