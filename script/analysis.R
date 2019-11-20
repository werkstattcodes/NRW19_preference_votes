library(tidyverse)
library(ggforce)
library(extrafont)
library(paletteer)
loadfonts(device = "win", quiet = T)
extrafont::fonts()
library(hrbrthemes)
library(ggtext)
library(patchwork)
library(ggiraph)
library(htmlwidgets)
library(lemon)

wdr <- getwd()
my_caption <- "Roland Schmidt | @zoowalk | http://werk.statt.codes"
my_caption_2 <- c("Data: bmi.gv.at/412/Nationalratswahlen/Nationalratswahl_2019/start.aspx#vorzugsstimmen", "Roland Schmidt | @zoowalk | <span style='color:orange'>**werk.statt.codes**</span>")


# preps -------------------------------------------------------------------

party_colors <- c(FPÖ = "#005DA8", NEOS = "#EA5290", ÖVP = "#5DC2CC", SPÖ = "#FC0204", GRÜNE = "#A3C630")


# get state and regional list--------------------------------------

df_state_region <- readxl::read_xlsx(path = paste0(wdr, "/data/NRW19_Vorzugsstimmen_Landes_Regionalparteiliste_16102019.xlsx"))

names(df_state_region) <- names(df_state_region) %>%
  stringr::str_remove(., "\r") %>%
  stringr::str_remove(., "\n") %>%
  stringr::str_remove(., "-")

df_state_region <- df_state_region %>%
  janitor::clean_names()


df_state_region_long <- df_state_region %>%
  mutate(party = map_chr(reihung_1, ~ str_extract(., regex("^List.*")))) %>%
  mutate(state_list = map_chr(reihung_1, ~ str_extract(., regex("^Landespartei.*")))) %>%
  mutate(regional_list = map_chr(familien_vorname_2, ~ str_extract(., regex("^Regionalpartei.*")))) %>% # mess in bmi file; heading in different columns
  mutate(regional_list_2 = map_chr(reihung_1, ~ str_extract(., regex("^Regionalpartei.*")))) %>%
  mutate(district = coalesce(state_list, regional_list, regional_list_2)) %>%
  janitor::remove_empty("rows") %>%
  fill(., district, .direction = c("down")) %>%
  fill(., party, .direction = c("down")) %>%
  select(-c("x6", "state_list", "regional_list", "regional_list_2")) %>%
  filter(str_detect(reihung_1, "^[0-9]")) %>%
  select(district, party, everything()) %>%
  rename_at(
    vars(reihung_7:vorz_stimm_11),
    function(x) str_replace_all(x, "[:digit:]+", "b")
  ) %>%
  rename_at(
    vars(reihung_1:vorz_stimm_5),
    function(x) str_replace_all(x, "[:digit:]+", "a")
  ) %>%
  mutate_at(vars(reihung_a:vorz_stimm_b), as.character) %>%
  pivot_longer(
    cols = reihung_a:vorz_stimm_b, names_to = c(".value", "result"),
    names_pattern = "(.*)_([a|b]$)"
  ) %>%
  mutate_at(vars(reihung, geb_jahr, vorz_stimm), as.numeric) %>%
  group_by(district, party) %>%
  arrange(reihung, .by_group = T) %>%
  mutate(district_type = case_when(
    str_detect(district, regex("Landes.*")) ~ "Landeswahlkreis",
    str_detect(district, regex("^Regional.*")) ~ "Regionalwahlkreis",
    TRUE ~ NA_character_
  )) %>%
  select(district_type, district, party, everything(), -result) %>%
  rename(
    name = familien_vorname,
    pref_votes_abs = vorz_stimm,
    district_type = district_type,
    district = district,
    rank_list = reihung,
    birth_year = geb_jahr,
  ) %>%
  ungroup()


# get federal list --------------------------------------

df_federal <- readxl::read_xlsx(
  path = paste0(wdr, "/data/NRW19_Vorzugsstimmen_bundesweit_16102019.xlsx"),
  col_name = F,
  trim_ws = T
)

names(df_federal) <- c(
  "Nr", "Familienname, Vorname", "Geb.-Jahr", "Beruf",
  "Gesamt", "B", "K", "NÖ", "OÖ", "S", "Stmk", "Tirol",
  "Vlbg", "Wien"
)


df_federal_clean <- df_federal %>%
  janitor::remove_empty(., which = c("rows")) %>%
  filter(!Nr == "Nr.") %>%
  mutate(party = str_extract(Nr, regex("[:alpha:].*"))) %>%
  fill(., party, .direction = c("down")) %>%
  filter(!str_detect(Nr, regex("[:alpha:]"))) %>%
  mutate(district_type = "Bundeswahlkreis") %>%
  pivot_longer(cols = Gesamt:Wien, names_to = "district", values_to = "pref_votes_abs")


# rename to be consistent with regional/state level
df_federal_clean <- df_federal_clean %>%
  select(district_type, district, party,
    rank_list = "Nr", name = "Familienname, Vorname", birth_year = "Geb.-Jahr",
    beruf = "Beruf", everything()
  ) %>%
  mutate_at(vars("rank_list", "birth_year", "pref_votes_abs"), ~ stringr::str_trim(., side = c("both"))) %>%
  mutate_at(vars("rank_list", "birth_year", "pref_votes_abs"), as.numeric)


# bind both bund, laender and regio -------------------------------------------------------------------

df_merge <- bind_rows(
  df_state_region_long,
  df_federal_clean
) %>%
  ungroup() %>% 
  filter(!is.na(name))

write.csv2(df_merge,
           file = paste0(wdr, "/data/AUT_NRW19_preference_votes.csv"),
           row.names = F,
           fileEncoding = "latin1"
)


fn_remove_titles <- function(x) {
  titles <- c(
    "Mag", "MMag", "Prof", "Dr", "O\\.Univ\\.Prof", "Dipl-Ing", "Dipl.-Ing.", "Dipl.-Rev.", "Ing",
    "Dipl", "jun", "sen", "Ba", "\\(FH\\)", "MSc",
    "med", "univ", "Bsc", "phil", "iur", "DDr", "BEd", "DI", "Mba", "Mas", "MA", "MIM",
    "vet", "M.A.", "PMM", "MTD", "Päd", "Bakk"
  ) %>%
    map(., paste0, c("\\.", "\\s", "\\,")) %>%
    unlist()


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
    str_remove_all(., regex(paste0(titles, collapse = "|"), ignore_case = T)) %>%
    str_squish()
}


# u <- df %>%
#   distinct(name) %>%
#   mutate(clean_names = map_chr(name, fn_remove_titles))


fn_short_name <- function(x) {
  family_name <- stringr::str_extract(x, "^[[:alpha:]\\-|\\s]+(?=,)") %>%
    str_to_title() %>%
    str_squish() %>%
    str_trim()

  first_name <- str_extract(x, regex("(?<=\\,)[[:alpha:]\\-\\s]*$")) %>%
    str_trim() %>%
    str_squish() %>%
    str_to_title()

  initals <- str_split(first_name, "\\s") %>%
    map(., str_sub, start = 1L, end = 1L) %>%
    map_chr(., str_c, collapse = ".") %>%
    paste0(., ".")

  name_short <- paste(family_name, initals, sep = ", ")
}


df <- df_merge %>%
  mutate(district = str_remove(district, "Landesparteiliste") %>%
    str_remove(., "Regionalparteiliste") %>%
    str_trim(., side = c("both"))) %>%
  mutate(district = case_when(
    district == "B" ~ "Burgenland",
    district == "S" ~ "Salzburg",
    district == "K" ~ "Kärnten",
    district == "NÖ" ~ "Niederösterreich",
    district == "OÖ" ~ "Oberösterreich",
    district == "Stmk" ~ "Steiermark",
    district == "Vlbg" ~ "Vorarlberg",
    TRUE ~ district
  )) %>%
  mutate(district = as_factor(district)) %>%
  mutate(party2 = str_extract(party, regex("\\([:alpha:]*\\)")) %>% # take parties' short abbrevs.
    str_remove_all(., "\\(|\\)")) %>%
  mutate(name_orig = name) %>%
  mutate(name = map_chr(name_orig, fn_remove_titles)) %>%
  mutate(name_short = map_chr(name, fn_short_name)) %>%
  mutate(state = case_when(
    str_detect(district, "^[0-9].*") ~ str_sub(district, start = 1L, end = 1L),
    TRUE ~ as.character(district)
  )) %>%
  mutate(state = case_when(
    state == 1 ~ "Burgenland",
    state == 2 ~ "Kärnten",
    state == 3 ~ "Niederösterreich",
    state == 4 ~ "Oberösterreich",
    state == 5 ~ "Salzburg",
    state == 6 ~ "Steiermark",
    state == 7 ~ "Tirol",
    state == 8 ~ "Vorarlberg",
    state == 9 ~ "Wien",
    TRUE ~ NA_character_
  )) %>%
  mutate(district_type = fct_relevel(district_type, "Bundeswahlkreis", "Landeswahlkreis", "Regionalwahlkreis"))

# name corrections
df <- df %>%
  mutate(name = case_when(
    str_detect(name, "Pistracher") ~ "Pistracher, Gerald",
    TRUE ~ as.character(name)
  ))



main_parties <- c("SPÖ", "ÖVP", "NEOS", "GRÜNE", "FPÖ")

df <- df %>%
  mutate(party3 = fct_other(party2, keep = main_parties) %>%
    fct_relevel(., "ÖVP", "SPÖ", "FPÖ", "GRÜNE", "NEOS", "Other"))


# ANALYSIS ----------------------------------------------------------------


# top 5 candidates per party ----------------------------------------------

top_candidates <- df %>%
  # filter(party2 %in% main_parties) %>%
  filter(!str_detect(district, "Gesamt")) %>%
  group_by(party3, name) %>%
  summarise(preferences_candidate = sum(pref_votes_abs, na.rm = T)) %>%
  arrange(desc(preferences_candidate), .by_group = T) %>%
  slice(1:5)

df %>%
  filter(name %in% top_candidates$name) %>%
  filter(!str_detect(district, "Gesamt")) %>%
  group_by(party3, name, district_type) %>%
  summarise(sum_candiate = sum(pref_votes_abs, na.rm = T)) %>% # rename sum_candidate
  group_by(party3) %>%
  arrange(desc(sum_candiate), .by_group = T) %>%
  ungroup() %>%
  group_by(name) %>%
  mutate(total_candidate = sum(sum_candiate)) %>%
  ungroup() %>%
  mutate(name = forcats::fct_reorder(name, total_candidate, .desc = F)) %>%
  ggplot() +
  geom_bar(aes(
    x = name,
    y = sum_candiate,
    fill = district_type
  ),
  group = name,
  width = 0.6,
  stat = "identity"
  ) +
  geom_text(
    data = top_candidates,
    aes(
      x = name,
      y = +Inf,
      label = scales::comma(preferences_candidate, accuracy = 1)
    ),
    stat = "identity",
    family = "Roboto Condensed",
    color = "grey30",
    hjust = 1,
    group = name
  ) +
  labs(
    caption = my_caption_2,
    title = "Candidates with most preference votes (Top 5)",
    y = "Number of preference votes"
  ) +
  scale_x_discrete(label = fn_short_name) +
  scale_y_continuous(
    label = scales::comma_format(scale = 0.001),
    expand = expansion(mult = c(0, 0.3)),
    breaks = seq(0, 90000, 30000),
    minor_breaks = NULL
  ) +
  scale_fill_paletteer_d("ggsci::default_jama",
    labels = c(
      "Regionalwahlkreis" = "Regional lists",
      "Landeswahlkreis" = "State lists",
      "Bundeswahlkreis" = "Federal list"
    )
  ) +
  facet_wrap(vars(party3),
    ncol = 2,
    scale = "free_y"
  ) +
  coord_flip() +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.spacing.x = unit(0, units = "cm"),
    axis.title.y = element_blank(),
    plot.caption = element_markdown(color = "grey30", hjust = c(0, 1)),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.key.size = unit(0.4, units = c("cm")),
    legend.justification = "right",
    plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
    plot.title.position = "plot",
    plot.margin = margin(0, unit = "cm"),
    # axis.text.y = element_text(face=x),
    strip.text = element_text(face = "bold")
  ) +
  guides(fill = guide_legend(rev = T, ))


name <- "Top_candidates"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
width <- 26.49
height <- 20
ggsave(
  filename = paste0(folder, time, name, format),
  device = "png",
  dpi = 96,
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)


Hofer <- df %>%
  filter(str_detect(name_orig, "^Hofer")) %>%
  filter(party3 == "FPÖ") %>%
  distinct(name_orig, district_type, district)


# top per district_type (not included) -----------------------------------------------------------

fn_top_plot <- function(x) {
  x %>%
    ggplot() +
    geom_bar(aes(
      x = name,
      y = pref_votes_abs
    ),
    stat = "identity"
    ) +
    labs(title = paste(unique(x$district_type))) +
    scale_x_discrete(labels = fn_short_name) +
    hrbrthemes::theme_ipsum_ps() +
    facet_wrap(vars(district),
      scales = "free_y"
    ) +
    coord_flip()
}

list_plots <- df %>%
  filter(!district == "Gesamt") %>%
  group_by(district_type, district) %>%
  arrange(desc(pref_votes_abs), .by_group = T) %>%
  slice(1:3) %>%
  ungroup() %>%
  group_split(district_type) %>%
  map(~ fn_top_plot(.))

patchwork::wrap_plots(list_plots[1:2]) +
  plot_layout(ncol = 1)


# difference btw ranking and number of preferential votes -----------------

df_rank_diff <- df %>%
  filter(!(district_type == "Bundeswahlkreis" & district != "Gesamt")) %>%
  group_by(party3, district_type, district) %>%
  arrange(desc(pref_votes_abs), .by_group = T) %>%
  mutate(rank_votes = row_number()) %>%
  mutate(rank_diff = rank_list - rank_votes) %>%
  select(district_type, district, party3, name, pref_votes_abs, rank_list, rank_votes, rank_diff) %>%
  arrange(desc(rank_diff))

df_rank_diff_long <- rank_diff %>%
  filter(party3 %in% main_parties) %>%
  pivot_longer(
    cols = c(rank_list, rank_votes),
    values_to = "rank_no",
    names_to = "ranking_type"
  )

# df_rank_diff_long %>%
#   ggplot() +
#   geom_point(aes(
#     x = fct_rev(ranking_type),
#     y = rank_no,
#     group = name,
#     color = party3
#   )) +
#   geom_line(aes(
#     x = fct_rev(ranking_type),
#     y = rank_no,
#     group = name,
#     color = party3
#   )) +
#   ggrepel::geom_text_repel(aes(
#     x = fct_rev(ranking_type),
#     y = rank_no,
#     group = name,
#     label = name,
#     color = party3
#   ),
#   size = 2
#   ) +
#   facet_grid(rows = vars(party3), cols = vars(district_type))


# INCLUDED Composition of preferential votes ---------------------------------------

df_list <- df %>%
  filter(!(district_type == "Bundeswahlkreis" & district != "Gesamt")) %>%
  group_by(district_type) %>%
  summarise(district_type_votes = sum(pref_votes_abs, na.rm = T)) %>%
  #  arrange(desc(district_type_votes)) %>%
  mutate(district_type = forcats::fct_relevel(district_type, "Regionalwahlkreis", "Landeswahlkreis", "Bundeswahlkreis")) %>%
  mutate(district_type_votes_cum = cumsum(district_type_votes)) %>%
  mutate(district_type_votes_perc = district_type_votes / sum(district_type_votes)) %>%
  mutate(district_type = forcats::fct_reorder(district_type, district_type_votes_cum, .desc = F))

df_list %>%
  ggplot() +
  geom_bar(aes(
    x = 1,
    y = district_type_votes,
    fill = district_type,
    group = district_type
  ),
  stat = "identity"
  ) +
  geom_text(aes(
    x = 1,
    y = district_type_votes,
    group = district_type,
    label = paste(scales::comma(district_type_votes),
      scales::percent(district_type_votes_perc),
      sep = "\n"
    )
  ),
  colour = "white",
  position = position_stack(vjust = .5, reverse = F)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    breaks = max(df_list$district_type_votes_cum),
    label = scales::comma(max(df_list$district_type_votes_cum))
  ) +
  scale_fill_paletteer_d("ggsci::default_jama",
    labels = c(
      "Regionalwahlkreis" = "Regional lists",
      "Landeswahlkreis" = "State lists",
      "Bundeswahlkreis" = "Federal list"
    )
  ) +
  labs(
    title = "Total number of preference votes per electoral district",
    caption = my_caption_2,
    y = "number of preference votes"
  ) +
  hrbrthemes::theme_ipsum_tw() +
  theme(
    legend.position = "bottom",
    legend.justification = "right",
    plot.title.position = "plot",
    legend.title = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
    panel.grid = element_blank(),
    plot.caption = element_markdown(hjust = c(0, 1), color = "grey30"),
    plot.margin = margin(t = 0, l = 0, b = 0, r = 1, unit = "cm")
  ) +
  coord_flip() +
  guides(fill = guide_legend(reverse = T))


name <- "Total_preferences_per_district"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
width <- 26.49
height <- 7
ggsave( # plot=plot_n_candidates_crossing_threshold,
  filename = paste0(folder, time, name, format),
  device = "png",
  dpi = 96,
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)


# INCLUDED Preference votes per party and district list -------------------------------------------------------------

x <- df %>%
  filter(!(district_type == "Bundeswahlkreis" & district != "Gesamt")) %>%
  group_by(party3, district_type) %>%
  summarise(district_type_votes = sum(pref_votes_abs, na.rm = T)) %>%
  arrange(desc(district_type_votes)) %>%
  mutate(district_type_votes_cum = cumsum(district_type_votes)) %>%
  mutate(district_type_votes_perc = district_type_votes / sum(district_type_votes)) %>%
  ungroup()


x %>%
  filter(party3 != "Other") %>%
  ggplot() +
  geom_bar(aes(
    x = party3,
    y = district_type_votes,
    group = district_type,
    fill = district_type
  ),
  color = NA,
  stat = "identity",
  position = "fill"
  ) +
  geom_text(aes(
    x = party3,
    group = district_type,
    y = district_type_votes_perc,
    label = scales::percent(district_type_votes_perc, accuracy = 0.1)
  ),
  color = "white",
  position = position_stack(vjust = 0.5, reverse = F)
  ) +
  labs(
    title = "Contribution of different electoral constituencies to total of preferential votes",
    caption = my_caption_2
  ) +
  hrbrthemes::theme_ipsum_rc() +
  scale_fill_paletteer_d("ggsci::default_jama",
    labels = c(
      "Regionalwahlkreis" = "Regional lists",
      "Landeswahlkreis" = "State lists",
      "Bundeswahlkreis" = "Federal list"
    )
  ) +
  scale_y_continuous(
    labels = scales::percent,
    expand = expansion(mult = c(0, 0))
  ) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  theme(
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.justification = "right",
    axis.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
    plot.title.position = "plot",
    plot.margin = margin(0, unit = "cm"),
    plot.caption = element_markdown(hjust = c(0, 1), color = "grey30")
  ) +
  guides(fill = guide_legend(reverse = T))


name <- "Total_preferences_per_district_per_party"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
width <- 26.49
height <- 11.17
ggsave(
  filename = paste0(folder, time, name, format),
  device = "png",
  dpi = 96,
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)



# intra-party movements due to preference votes --------------------------


# > import election results -----------------------------------------------
file <- paste0(wdr, "/data/wahl_20191003_214253.csv")

nrw19_imp <- readr::read_csv2(
  file = file,
  locale = locale(encoding = "latin1")
) %>%
  janitor::remove_empty(., which = c("cols")) %>%
  janitor::clean_names() %>%
  rename(gkz = x1)

nrw19 <- nrw19_imp %>%
  mutate(state = str_sub(gkz, 2, 2)) %>%
  mutate(district_pol = str_sub(gkz, 3, 4)) %>% # Bezirk
  mutate(municipality = str_sub(gkz, 5, 6)) %>%
  mutate(level = case_when(
    str_sub(gkz, 2, 2) == 0 ~ "gesamt",
    str_detect(gkz, regex("^[A-Z][1-9]00[0,9][0,9]")) ~ "Bundesland",
    str_detect(gkz, regex("^G.{1}[A-Z].+")) ~ "Regional-Wahlkreis",
    str_detect(gkz, regex("^G[1-9][0-9][1-9]00")) ~ "Bezirk",
    str_detect(gkz, regex("^G[1-9][0-9][1-9]99")) ~ "Bezirk",
    str_detect(gkz, regex("^G[1-9][0-9][1-9][0-8][0-9]")) ~ "Gemeinde",
    TRUE ~ as.character("u")
  )) %>%
  mutate(wahlkarten = case_when(
    str_detect(gkz, regex("99$")) ~ "ja",
    TRUE ~ as.character("nein")
  )) %>%
  select(level,
    district_name = gebietsname, gkz, electorate_size = wahlberechtigte,
    votes_mail = wahlkarten, state, votes_casted = abgegebene, district_pol,
    municipality, everything()
  )


nrw19_bezirk <- nrw19 %>%
  filter(votes_mail == "nein") %>%
  filter(level == "Bezirk")

nrw19_bezirk <- nrw19_bezirk %>%
  mutate(turnout = votes_casted / electorate_size * 100) %>%
  mutate_at(vars(spo, ovp, fpo, grune, neos), .funs = list(perc = ~ .x / gultige))

nrw19_long <- nrw19_bezirk %>%
  select(contains("perc"), district_name, gkz, turnout) %>%
  pivot_longer(., cols = contains("perc"), names_to = "party", values_to = "party_perc")



# > federal level ---------------------------------------------------------
results_federal_list <- nrw19 %>%
  filter(level == "gesamt")

results_federal_list_long <- results_federal_list %>%
  pivot_longer(cols = ovp:slp, names_to = "party", values_to = "votes_abs") %>%
  mutate(district_type = "Bundeswahlkreis") %>%
  mutate(district = "00") %>%
  select(district_type, district_name, district, party, votes_abs)



# > state level -----------------------------------------------------------

results_state_list <- nrw19 %>%
  filter(level == "Bundesland") %>%
  group_by(state) %>%
  summarise_if(is.numeric, sum, na.rm = T) %>%
  mutate(district_name = case_when(
    state == 1 ~ "Burgenland",
    state == 2 ~ "Kärnten",
    state == 3 ~ "Niederösterreich",
    state == 4 ~ "Oberösterreich",
    state == 5 ~ "Salzburg",
    state == 6 ~ "Steiermark",
    state == 7 ~ "Tirol",
    state == 8 ~ "Vorarlberg",
    state == 9 ~ "Wien",
    TRUE ~ NA_character_
  )) %>%
  mutate(district = state)

results_state_list_long <- results_state_list %>%
  pivot_longer(cols = ovp:slp, names_to = "party", values_to = "votes_abs") %>%
  mutate(district_type = "Landeswahlkreis") %>%
  select(district_type, district_name, district, party, votes_abs)



# >> details on electoral districts/constituencies -----------------------

library(rvest)

MoI_url <- "https://www.bmi.gv.at/412/Nationalratswahlen/Wahlkreiseinteilung.aspx"

dput(n_mandates_regional_districts)

n_mandates_regional_districts <- MoI_url %>%
  read_html() %>%
  html_nodes("table") %>%
  html_table() %>%
  map_df(., bind_rows)

# from utils package; allows Umlaute when exporting
write.csv2(n_mandates_regional_districts,
  file = paste0(wdr, "/data/n_mandates_regional_districts.csv"),
  row.names = F,
  fileEncoding = "latin1"
)

n_mandates_regional_districts <- read_csv2(
  file = paste0(wdr, "/data/n_mandates_regional_districts.csv"),
  locale = locale(encoding = "latin1")
)


n_mandates_landeswahlkreis <- n_mandates_regional_districts %>%
  mutate(state = stringr::str_sub(Regionalwahlkreis, 1, 1)) %>%
  group_by(state) %>%
  summarise(n_mandates = sum(Mandate))

info_constituencies <- dplyr::bind_rows(
  n_mandates_regional_districts %>%
    select(-Stimmbezirke) %>%
    rename(
      district = Regionalwahlkreis,
      district_name = Bezeichnung,
      n_mandates = Mandate
    ),
  n_mandates_landeswahlkreis %>%
    mutate(district = case_when(
      state == "1" ~ "Burgenland",
      state == "2" ~ "Kärnten",
      state == "3" ~ "Niederösterreich",
      state == "4" ~ "Oberösterreich",
      state == "5" ~ "Salzburg",
      state == "6" ~ "Steiermark",
      state == "7" ~ "Tirol",
      state == "8" ~ "Vorarlberg",
      state == "9" ~ "Wien",
      TRUE ~ NA_character_
    )) %>%
    mutate(district_name = district)
) %>%
  mutate(district_name_short = str_replace(district_name, "Burgenland", "Bgld") %>%
    str_replace(., "Kärnten", "Ktn") %>%
    str_replace(., "Niederösterreich", "NÖ") %>%
    str_replace(., "Salzburg", "Sbg") %>%
    str_replace(., "steiermark", "Stmk") %>%
    str_replace(., "Innsbruck", "Ibk") %>%
    str_replace(., "Vorarlberg", "Vbg") %>%
    str_replace(., "\\sund\\s", " u ")) %>%
  select(., -state) %>%
  bind_rows(., data.frame(district = "Gesamt", district_name = "Gesamt", n_mandates = 183, district_name_short = "Gesamt"))

# >> get 'electoral number' --------------------------------------------------

electoral_number <- left_join(results_state_list %>%
  select(state, district_name, district, gultige),
n_mandates_landeswahlkreis,
by = c("district" = "state")
) %>%
  mutate(electoral_number = ceiling(gultige / n_mandates)) # rounded to next higher integer


results_state_list_long <- left_join(results_state_list_long,
  electoral_number %>% select(district, electoral_number),
  by = c("district" = "district")
)


# > regional level -------------------------------------------------------

results_regional_list <- nrw19 %>%
  mutate(district_name = str_remove_all(district_name, "Wahlkarten") %>%
    str_remove_all(., "-") %>%
    str_trim()) %>%
  filter(level == "Regional-Wahlkreis" & str_detect(gkz, "00$")) %>%
  mutate(district_type = "Regionalwahlkreis") %>%
  mutate(district = paste0(state, district_pol) %>% stringr::str_sub(., 1, 2)) %>%
  mutate(state = case_when(
    state == 1 ~ "Burgenland",
    state == 2 ~ "Kärnten",
    state == 3 ~ "Niederösterreich",
    state == 4 ~ "Oberösterreich",
    state == 5 ~ "Salzburg",
    state == 6 ~ "Steiermark",
    state == 7 ~ "Tirol",
    state == 8 ~ "Vorarlberg",
    state == 9 ~ "Wien",
    TRUE ~ NA_character_
  ))

unique(results_regional_list$district_name)

results_regional_list_long <- results_regional_list %>%
  select(district_type, district_name, district, ovp:slp) %>%
  pivot_longer(cols = ovp:slp, names_to = "party", values_to = "votes_abs")

results_regional_list_long <- results_regional_list_long %>%
  left_join(., n_mandates_regional_districts %>%
    select(-Stimmbezirke, -Bezeichnung) %>%
    rename(
      district = Regionalwahlkreis,
      n_mandates = Mandate
    ),
  by = c("district")
  )


# bind results
df_results <- bind_rows(results_federal_list_long, results_state_list_long, results_regional_list_long) %>%
  filter(party %in% c("ovp", "spo", "fpo", "grune", "neos")) %>%
  mutate(party = case_when(
    party == "ovp" ~ "ÖVP",
    party == "spo" ~ "SPÖ",
    party == "grune" ~ "GRÜNE",
    party == "fpo" ~ "FPÖ",
    party == "neos" ~ "NEOS",
    TRUE ~ as.character(party)
  )) %>%
  mutate(district = case_when(
    district == 1 ~ "Burgenland",
    district == 2 ~ "Kärnten",
    district == 3 ~ "Niederösterreich",
    district == 4 ~ "Oberösterreich",
    district == 5 ~ "Salzburg",
    district == 6 ~ "Steiermark",
    district == 7 ~ "Tirol",
    district == 8 ~ "Vorarlberg",
    district == 9 ~ "Wien",
    district == "00" ~ "Gesamt",
    TRUE ~ as.character(district)
  ))

# merge results of preference votes with parties' overall results.
df_comb <- left_join(df %>% select(district_type, district, party3, name, name_short, name_orig,
  preference_votes = pref_votes_abs
),

df_results %>% rename(party_votes = votes_abs),
by = c("district_type" = "district_type", "district" = "district", "party3" = "party")
)



# preference votes vs votes for party -------------------------------------

preference_votes_constituency <- df %>%
  group_by(district_type, district, party3, state) %>%
  summarise(sum_preference_votes = sum(pref_votes_abs, na.rm = T)) %>%
  ungroup()

unique(preference_votes_constituency$district_type)
unique(df_results$district_type)
unique(preference_votes_constituency$district)
unique(df_results$district)

# federal level would need disaggregation
u <- left_join(preference_votes_constituency,
  df_results %>% select(district_type, district, district_name, party, votes_abs),
  by = c(
    "district_type" = "district_type",
    "district" = "district",
    "party3" = "party"
  )
) %>%
  mutate(p_pvpv_ratio = sum_preference_votes / votes_abs) %>%  #p_pvpv_ratio=party preference vote vote ratio
  # filter(!(kreis=="Bundeswahlkreis" & district!="Gesamt")) %>%
  filter(district_type == "Regionalwahlkreis") %>%
  filter(party3 != "Other")

labeller_bld <- c(
  "Burgenland" = "Bgld",
  "Kärnten" = "Ktn",
  "Niederösterreich" = "NÖ",
  "Oberösterreich" = "OÖ",
  "Salzburg" = "Sbg",
  "Steiermark" = "Stmk",
  "Tirol" = "Tirol",
  "Vorarlberg" = "Vbg",
  "Wien" = "Wien"
)

u <- dplyr::left_join(u,
  n_mandates_regional_districts %>%
    rename(
      district = Regionalwahlkreis,
      n_mandates = Mandate
    ) %>%
    select(-Bezeichnung, -Stimmbezirke),
  by = c("district")
)



# not included: Parties' preference vote share (facet grid)  -----------------------------------------------------

u %>%
  ggplot() +
  geom_bar(aes(
    x = paste0(district_name, "(", district, ")"),
    y = p_pvpv_ratio,
    fill = party3
  ),
  stat = "identity"
  ) +
  labs(
    y = "preference votes in relation to party votes",
    x = "regional constituency list/Regionalwahlkreis",
    title = ""
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.1)),
    position = "right"
  ) +
  scale_fill_manual(values = party_colors) +
  facet_grid(
    rows = vars(party3), cols = vars(state),
    scales = "free_x",
    space = "free",
    switch = "y"
  ) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0),
    legend.position = "none",
    strip.text = element_text(hjust = 1),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.spacing = unit(0.05, "cm")
  )

name <- "Parties_preference_vote_share_NOT_INCLUDED_IN_BLOG"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 30
width <- 40
ggsave(
  filename = paste0(folder, time, name, format),
  device = "png",
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)


# not incldued: Parties' preference vote share (boxplot) ---------------------------------------------------------------

pos <- position_jitter(width = 0.2, height = 0, seed = 1) # define seed for positioning

u %>%
  group_by(party3) %>%
  arrange(desc(p_pvpv_ratio)) %>%
  mutate(index = row_number()) %>%
  mutate(label = case_when(
    index < 4 ~ district_name,
    TRUE ~ NA_character_
  )) %>%
  ungroup() %>%
  ggplot() +
  labs(
    title = "Preference votes as share of total votes casted (Parties' preference votes share)",
    caption = my_caption_2,
    subtitle = "Only regional constitlevel",
    y = "Preference votes in relation to total votes"
  ) +
  geom_boxplot(aes(
    x = party3,
    y = p_pvpv_ratio
  ),
  outlier.shape = NA,
  fill = "transparent"
  ) +
  geom_point(aes(
    x = party3,
    y = p_pvpv_ratio
  ),
  position = pos
  ) +
  ggrepel::geom_text_repel(aes(
    x = party3,
    y = p_pvpv_ratio,
    label = label
  ),
  position = pos
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, .75),
    breaks = seq(0, .75, .25)
  ) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.caption = element_markdown(
      color = "grey30",
      hjust = c(0, 1)
    ),
    axis.title.x = element_blank()
  )


name <- "Parties_preference_vote_share_boxplot_NOT_INCLUDED_IN_BLOG"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 30
width <- 40
ggsave(
  filename = paste0(folder, time, name, format),
  device = "png",
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)

# INCLUDED  Parties' Preference Vote Votre (P-PVPV) ratio by state  ------------------------------------------------------

pos <- position_jitter(width = 0.2, height = 0, seed = 1) # define seed for positioning

plot_region <- u %>%
  group_by(state) %>%
  arrange(desc(p_pvpv_ratio), .by_group = T) %>%
  mutate(index = row_number()) %>%
  mutate(label = case_when(
    index < 4 ~ district_name,
    TRUE ~ NA_character_
  )) %>%
  ungroup() %>%
  group_by(state) %>%
  mutate(median_bundesland = median(p_pvpv_ratio)) %>%
  ungroup() %>%
  mutate(state = fct_reorder(state, median_bundesland, .desc = T)) %>%
  ggplot() +
  labs(
    title = "Parties' preference vote - party vote ratio (P-PVPV) by state",
    caption = my_caption_2,
    subtitle = "Only regional constituency level (Regionalwahlliste)",
    y = "# of party's preference votes as ratio of party's total votes in the constituency (P-PVPV)"
  ) +
  geom_boxplot(aes(
    x = state,
    y = p_pvpv_ratio
  ),
  outlier.shape = NA,
  fill = "transparent"
  ) +
  geom_point_interactive(aes(
    x = state,
    color = party3,
    y = p_pvpv_ratio,
    data_id = district_name,
    tooltip = paste(
      party3,
      district_name,
      round(p_pvpv_ratio * 100, 2),
      "%"
    )
  ),
  position = pos
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, .75),
    breaks = seq(0, .75, .25)
  ) +
  scale_color_manual(values = party_colors) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.justification = "right",
    plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
    plot.subtitle = element_text(size = 12, color = "grey30"),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      color = "grey30",
      hjust = c(0, 1)
    ),
    plot.margin = margin(0, unit = "cm"),
    axis.title.x = element_blank()
  )

plot_region

my_plot <- girafe(
  code = print(plot_region),
  width_svg = 10,
  height_svg = 6,
  options = list(
    opts_toolbar(saveaspng = FALSE),
    opts_sizing(rescale = T),
    opts_tooltip(css = "background-color:lightgray; font-family:Roboto Condensed;")
  ))


my_plot


# save --------------------------------------------------------------------

x <- girafe_options(
  x = my_plot,
  sizingPolicy(padding = "0px")
)

saveWidget(x,
  file = "Party_PVPv_state.html",
  selfcontained = T,
  background = "white"
)


name <- "Party_PVPv_state"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 20
width <- 30
ggsave(
  filename = paste0(folder, time, name, format),
  device = "png",
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)


# INCLUDED: Party's preference vote vote ratio (P-PVPV ratio) by district magnitude -----------------------------------------------------

pos <- position_jitter(width = 0.2, height = 0, seed = 1) # define seed for positioning

constituencies_annotation <- info_constituencies %>%
  select(district, n_mandates, district_name_short) %>%
  filter(str_detect(district, "^[0-9]")) %>%
  group_by(n_mandates) %>%
  summarise(gebiete = paste0(district_name_short, collapse = ", ")) %>%
  mutate(n_mandates = as_factor(n_mandates))

plot_n_mandates <- u %>%
  mutate(n_mandates = as_factor(n_mandates) %>% fct_rev()) %>%
  group_by(n_mandates) %>%
  arrange(desc(p_pvpv_ratio), .by_group = T) %>%
  mutate(index = row_number()) %>%
  mutate(label = case_when(
    index < 4 ~ district_name,
    TRUE ~ NA_character_
  )) %>%
  ungroup() %>%
  ggplot() +
  labs(
    title = "Parties' preference Votes - party votes ratio (P-PVPV) and district magnitude",
    caption = my_caption_2,
    subtitle = "Only regional constituency level (Regionalwahlliste).",
    y = "# of party's preference votes as ratio of party's total votes in the constituency (P-PVPV)",
    x = "District magnitude (number of available mandates in constituency)"
  ) +
  geom_boxplot(aes(
    x = n_mandates,
    y = p_pvpv_ratio
  ),
  outlier.shape = NA,
  fill = "transparent"
  ) +
  geom_point_interactive(aes(
    x = n_mandates,
    color = party3,
    y = p_pvpv_ratio,
    data_id = party3,
    tooltip = paste0(district_name, "\n", party3, ": ", round(p_pvpv_ratio * 100, 2), " %")
  ),
  position = pos
  ) +
  geom_rect(
    ymin = .8, ymax = Inf,
    xmin = 0, xmax = Inf,
    fill = "white"
  ) +
  geom_text(
    data = constituencies_annotation,
    aes(
      y = .75,
      x = n_mandates,
      label = str_wrap(gebiete, width = 40)
    ),
    hjust = 0,
    family = "Roboto Condensed",
    nudge_x = 0.5,
    lineheight = 0.8,
    size = 3.5
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1),
    breaks = seq(0, .75, .25)
  ) +
  scale_color_manual(values = party_colors) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    axis.text.y = element_text(
      size = 12, face = "bold",
      vjust = 0,
      hjust = 1
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.justification = "right",
    plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
    plot.subtitle = element_text(size = 12, color = "grey30"),
    plot.caption = element_markdown(
      color = "grey30",
      hjust = c(0, 1)
    ),
    plot.margin = margin(0, unit = "cm"),
    plot.title.position = "plot",
  ) +
  coord_flip()

plot_n_mandates


# > save ------------------------------------------------------------------

my_plot <- girafe(
  code = print(plot_n_mandates),
  width_svg = 10,
  height_svg = 7,
  options = list(
    opts_toolbar(saveaspng = FALSE),
    opts_sizing(rescale = T),
    opts_tooltip(css = "background-color:lightgray;	
                                              font-family:Roboto Condensed;")
  ))


my_plot

x <- girafe_options(
  x = my_plot,
  sizingPolicy(
    padding = "0px",
    defaultWidth = "1000px",
    defaultHeight = "700px"
  )
)

saveWidget(x,
  file = "Party_PVPV_district_magnitude.html",
  background = "white"
)


# INCLUDED Candidate's preference vote vote ratio (C-PVPV) by list  ------------------------------------------

share <- df %>%
  filter(!party3 == "Other") %>%
  filter(!(district_type == "Bundeswahlkreis" & district != "Gesamt")) %>%
  filter(!is.na(pref_votes_abs)) %>%
  group_by(district_type, state, district, party3) %>%
  arrange(desc(pref_votes_abs), .by_group = T) %>%
  mutate(index = row_number()) %>%
  mutate(sum_preference = sum(pref_votes_abs, na.rm = T)) %>%
  ungroup() %>%
  mutate(share_pref = pref_votes_abs / sum_preference) %>%
  # filter(share_pref>0.05) %>%  #change later
  left_join(., info_constituencies,
    by = c("district")
  )

df_thresholds <- tibble::tribble(
  ~threshold, ~district_type,
  .07, "Bundeswahlkreis",
  .10, "Landeswahlkreis",
  .14, "Regionalwahlkreis"
)

df_thresholds_2 <- crossing(df_thresholds, unique(share$party3)) %>%
  rename(party3 = `unique(share$party3)`)


df_share <- share %>%
  left_join(., df_thresholds_2,
    by = c("party3", "district_type")
  ) %>%
  mutate(share_pref = share_pref * 100) %>%
  left_join(., electoral_number %>%
    select(district_name, electoral_number),
  by = c("district" = "district_name")
  ) %>%
  mutate(electoral_number_indicator = case_when(
    pref_votes_abs > electoral_number ~ "yes",
    TRUE ~ as.character("no")
  )) %>% 
  mutate(district_name=case_when(district_name=="Gesamt" ~ "Österreich",
                                 TRUE ~ as.character(district_name))) %>% 
  left_join(., df_results %>% select(district_type, -district_name, district, party, votes_abs) %>% 
              mutate(party=as_factor(party)),
              by=c("district_type"="district_type",
               #  "district_name"="district_name",
                 "district"="district",
                 "party3"="party")) %>% 
  mutate(c_pvpv_ratio=round(pref_votes_abs/votes_abs, 4)) %>%  #c_pvpv candidates' preference vote vote ratio
  mutate(party3=as_factor(party3)) %>% 
  ungroup() %>% 
  mutate(sorting_index=group_indices(., district))


x <- df_share %>% 
  select(district, district_name_short, sorting_index) %>% 
  distinct()


plot_c_pvpv_ratio <- df_share %>%
  filter(party3 != "Other") %>%
  group_split(district_type) %>%
  map(~ ggplot(.) +
    labs(
      title = paste(unique(.$district_type)),
      subtitle = paste("Candidates securing preference votes more than
                    <span style='color:orange'>", unique(.$threshold) * 100, "</span> % of their party's overall vote in their respective constituency are re-ranked."),
      x = "Candidate's preference votes in % of party votes (C-PVPV)"
    ) +
    geom_point_interactive(aes(
      y = reorder(district_name_short, sorting_index),
      x = c_pvpv_ratio*100,
      # shape = as_factor(electoral_number_indicator),
      color = party3,
      data_id = name_short,
      tooltip = paste(name_short, round(c_pvpv_ratio*100, 2), sep = "; ")
    ),
    position = position_jitter(width = 0, height = 0.2, seed = 2),
    stat = "identity"
    ) +
    geom_vline(aes(xintercept = threshold * 100),
      color = "orange"
    ) +
    scale_y_discrete(expand = expansion(add = 0.2)) +
    scale_x_continuous(
      trans = "log10",
      limits = c(NA, 40),
      labels = scales::label_percent(accuracy = 1, scale = 1),
      minor_breaks = NULL,
      breaks = c(NA, 0, 1, 10, 40)
    ) +
    scale_color_manual(values = party_colors) +
    ggforce::facet_row(vars(party3),
      space = "free",
      scale = "free_x",
      shrink = T
    ) +
    hrbrthemes::theme_ipsum_rc() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 9),
      axis.title.y = element_blank(),
      panel.spacing.x = unit(0.2, "cm"),
      panel.spacing.y = unit(0.2, "cm"),
      strip.text.x = element_text(
        angle = 0,
        vjust = 1,
        face = "bold"
      ),
      strip.text.y = element_text(
        angle = 180,
        vjust = 1,
        face = "bold"
      ),
      strip.placement = "outside",
      plot.title = element_text(size = 11, face = "bold.italic", 
                                margin=margin(b=0, unit="cm")),
      plot.subtitle = element_markdown(size = 11, color = "grey30",
                                       face="italic"),
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.justification = "right",
      panel.grid.major.y = element_blank(),
      plot.margin = margin(0, unit = "cm")
    ) +
    guides(color = "none"))

plot_c_pvpv_ratio <- (patchwork::wrap_plots(plot_c_pvpv_ratio, ncol = 1) &
  theme(plot.margin = margin(unit = "cm", 0))) +
  plot_annotation(
    title = "Candidates' preference votes v. party votes ratio (C-PVPV)",
    subtitle = "Candidates above vertical line move up the electoral list.",
    caption = my_caption,
    theme = theme(
      plot.title = element_text(family = "Roboto Condensed", size = 12,face = "bold"),
      plot.subtitle = element_text(family = "Roboto Condensed", size = 12,face = "plain")))+
      plot_layout(heights = c(1, 9, 39))


# > save ------------------------------------------------------------------

plot_c_pvpv_ratio

name <- "C_PVPV_by_list"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 30
width <- 30
ggsave(
  filename = paste0(folder, time, name, format),
  device = "png",
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)

my_plot <- girafe(
  code = print(plot_c_pvpv_ratio),
  width_svg = 10,
  height_svg = 15,
  options = list(
    opts_toolbar(saveaspng = FALSE),
    opts_sizing(rescale = T),
    opts_tooltip(css = "background-color:lightgray;	font-family:Roboto Condensed;")
  )
)
my_plot

x <- girafe_options(
  x = my_plot,
  sizingPolicy(
    padding = "0px"
  )
)

saveWidget(x,
  file = "C_PVPV_by_list.html",
  background = "white"
)



# INCLUDED. Candidates crossing preference vote threshold -------------------------------------------


df_n_candidates_crossing_threshold <- df_share %>%
  mutate(leader=case_when(rank_list==1 ~ "list leader",
                          TRUE ~ as.character("not list leader"))) %>% 
  mutate(threshold_share = case_when(
    c_pvpv_ratio * 100> threshold * 100 ~ "yes",
    TRUE ~ as.character("no")
  )) %>%
  filter(threshold_share == "yes")
 # select(district_type, district_name_short, party3, c_pvpv_ratio, threshold) %>%
  # group_by(district_type, party3, threshold_share) %>%
  # summarise(n = n()) %>%


plot_n_candidates_crossing_threshold <- df_n_candidates_crossing_threshold %>% 
  ggplot() +
  labs(
    title = "Number of candidates crossing preference vote threshold",
    caption = my_caption_2
  ) +
  geom_bar(aes(
    x = party3,
    fill = leader,
    group=leader,
    ),
  stat = "count",
  position=position_stack(vjust=0.5)
  ) +
  geom_text(aes(x=party3,
                group=leader,
                label=..count..),
            color="white",
            stat="count",
            position = position_stack(vjust=0.5))+
  geom_text(aes(x=party3,
                group=party3,
                label=..count..),
            color="black",
            stat="count",
            nudge_y = 0.5,
            #position = position_stack(vjust=1)
            )+
  scale_y_continuous(breaks=seq(0, 20, 2))+
  scale_fill_paletteer_d("ggsci::default_jama")+
  facet_wrap(vars(district_type)) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.justification = "right",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(0, unit = "cm"),
    plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = c(0, 1))
  )

plot_n_candidates_crossing_threshold

name <- "n_candidates_crossing_threshold"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
width <- 26.49
height <- 11.17
ggsave(
  plot = plot_n_candidates_crossing_threshold,
  filename = paste0(folder, time, name, format),
  device = "png",
  dpi = 96,
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)

# INCLUDED Candidates crossing electoral number with preference votes (geofacet) -----------------------------------
library(geofacet)

aut_grid <- data.frame(
  row = c(1, 1, 1, 2, 2, 2, 2, 2, 3),
  col = c(3, 4, 5, 1, 2, 3, 4, 5, 4),
  code = c("Oberösterreich", "Niederösterreich", "Wien", "Vorarlberg", "Tirol", "Salzburg", "Steiermark", "Burgenland", "Kärnten"),
  name = c("Oberösterreich", "Niederösterreich", "Wien", "Vorarlberg", "Tirol", "Salzburg", "Steiermark", "Burgenland", "Kärnten")
)

state_electoral_number <- df %>%
  filter(party3 != "Other") %>%
  filter(district_type == "Landeswahlkreis") %>%
  left_join(.,
    electoral_number %>% select(district_name, electoral_number),
    by = c("district" = "district_name")
  ) %>%
  select(district_type, district, name_short, party3, electoral_number, pref_votes_abs)

state_electoral_number_plot <- state_electoral_number %>%
  ggplot() +
  labs(
    title = "Candidates' preference votes and electoral number",
    caption = my_caption_2,
    y = "Preference votes"
  ) +
  geom_point_interactive(aes(
    x = party3,
    color = party3,
    y = pref_votes_abs,
    tooltip = paste(
      party3, "\n",
      name_short, pref_votes_abs
    )
  ),
  position = position_jitter(width = 0.2, height = NULL, seed = 2)
  ) +
  geom_hline(aes(yintercept = electoral_number),
    color = "orange"
  ) +
  geom_text(aes(
    x = Inf, y = electoral_number,
    label = scales::comma(electoral_number)
  ),
  color = "orange",
  nudge_y = -3000,
  family = "Roboto Condensed",
  hjust = 1,
  check_overlap = T
  ) +
  scale_color_manual(values = party_colors) +
  scale_x_discrete(expand = expansion(mult = 0)) +
  scale_y_continuous(
    labels = scales::label_comma(),
    expand = expansion(mult = 0),
    limits = c(0, 35000),
    minor_breaks = NULL
  ) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    strip.text.y = element_text(angle = 0, vjust = 1),
    legend.position = "none",
    plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
    plot.title.position = "plot",
    plot.caption = element_markdown(hjust = c(0, 1), color = "grey30"),
    plot.margin = margin(0, r = 0.2, unit = "cm"),
    strip.background.x = element_rect(fill = "lightgrey"),
    panel.border = element_rect(color = "lightgrey", fill = "transparent")
  ) +
  facet_geo(~district, grid = aut_grid) +
  guides(color = "none")


# > save ------------------------------------------------------------------

state_electoral_number_plot

my_plot <- girafe(
  code = print(state_electoral_number_plot),
  width_svg = 10,
  height_svg = 6,
  options = list(
    opts_toolbar(saveaspng = FALSE),
    opts_sizing(rescale = T),
    opts_tooltip(css = "background-color:lightgray;	font-family:Roboto Condensed;")
  )
)
my_plot

x <- girafe_options(
  x = my_plot,
  sizingPolicy(padding = "0px")
)

saveWidget(x,
  file = "state_electoral_number_plot.html",
  background = "white"
)



# INCLUDED Concentration of preference votes (gini) ------------------------------------------------------------------

library(reldist)


# > plot --------------------------------------------------------------------

plot_gini <- df %>%
  filter(party3 != "Other") %>%
  filter(!(district_type == "Bundeswahlkreis" & district != "Gesamt")) %>%
  left_join(., info_constituencies, by = c("district")) %>%
  filter(!is.na(pref_votes_abs)) %>%
  group_by(district_type, state, district_name_short, party3) %>%
  summarise(gini_pref = gini(pref_votes_abs)) %>%
  ggplot() +
  labs(
    title = "Concentration of preference votes",
    caption = my_caption_2,
    y = "Gini coefficient"
  ) +
  geom_point_interactive(aes(
    x = party3,
    y = gini_pref,
    color = party3,
    tooltip = paste(
      district_name_short, "\n",
      party3, round(gini_pref, 2)
    )
  ),
  pos = position_jitter(width = 0.2, height = NULL, seed = 2)
  ) +
  geom_boxplot(aes(
    x = party3,
    y = gini_pref
  ),
  fill = "transparent",
  outlier.shape = NA
  ) +
  scale_color_manual(values = party_colors) +
  scale_x_discrete(expand = expansion(mult = 0)) +
  scale_y_continuous(
    labels = scales::label_comma(),
    limits = c(0, 1),
    expand = expansion(mult = 0),
    minor_breaks = NULL
  ) +
  facet_wrap(vars(district_type)) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text.y = element_text(angle = 0, vjust = 1),
    legend.position = "none",
    plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
    plot.caption = element_markdown(hjust = c(0, 1), color = "grey30"),
    plot.margin = margin(0, unit = "cm")
  ) +
  guides(color = "none")


# > save --------------------------------------------------------------------

plot_gini

my_plot <- girafe(
  code = print(plot_gini),
  width_svg = 10,
  height_svg = 6,
  options = list(
    opts_toolbar(saveaspng = FALSE),
    opts_sizing(rescale = T),
    opts_tooltip(css = "background-color:lightgray;	font-family:Roboto Condensed;")
  )
)
my_plot

x <- girafe_options(
  x = my_plot,
  sizingPolicy(padding = "0px")
)

saveWidget(x,
  file = "concentration_preference_votes.html",
  background = "white"
)



name <- "Difference_leading_runnersup_prefernece_votes"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 30
width <- 30
ggsave(
  filename = paste0(folder, time, name, format),
  device = "png",
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)





# INCLUDED Competitiveness  ----------------------------------------------------------


# > NOT INCLUDED: Difference as share of total preference votes -------------------------

# df %>%
#   filter(!(district_type == "Bundeswahlkreis" & district != "Gesamt")) %>%
#   # filter(district=="Gesamt") %>%
#   filter(!is.na(pref_votes_abs)) %>%
#   group_by(district_type, state, district, party3) %>%
#   arrange(desc(pref_votes_abs), .by_group = T) %>%
#   mutate(index = row_number()) %>%
#   mutate(sum_preference = sum(pref_votes_abs, na.rm = T)) %>%
#   filter(index < 3) %>%
#   mutate(pref_diff = pref_votes_abs - lead(pref_votes_abs)) %>%
#   mutate(diff_perc = pref_diff / sum_preference) %>%
#   filter(!is.na(diff_perc)) %>%
#   mutate(indicator = paste0(district, "-", name_short)) %>%
#   mutate(indicator = district) %>%
#   ggplot() +
#   labs(
#     title = "Lead",
#     subtitle = "Lead in % of party's total number of preference votes"
#   ) +
#   geom_bar(aes(
#     y = indicator,
#     x = diff_perc
#   ),
#   stat = "identity"
#   ) +
#   lemon::facet_rep_grid(
#     cols = vars(party3),
#     rows = vars(district_type),
#     # repeat.tick.labels = T,
#     scales = "free",
#     drop = T,
#     space = "free"
#   ) +
#   hrbrthemes::theme_ipsum_rc() +
#   theme(axis.text.x = element_text(angle = 90))




# > INCLUDED: Difference % leader vs runers-up ---------------------------------------------------


d <- df %>%
  left_join(., info_constituencies, by = c("district")) %>%
  filter(!party3 == "Other") %>%
  filter(!(district_type == "Bundeswahlkreis" & district != "Gesamt")) %>%
  filter(!is.na(pref_votes_abs)) %>%
  group_by(district_type, state, district, party3) %>%
  arrange(desc(pref_votes_abs), .by_group = T) %>%
  mutate(index = row_number()) %>%
  mutate(sum_preference = sum(pref_votes_abs, na.rm = T)) %>%
  filter(index < 3) %>%
  mutate(share_pref = pref_votes_abs / sum_preference) %>%
  mutate(indicator = district) # %>%

d <- d %>%
  ggplot() +
  labs(
    title = "Lead",
    subtitle = "Lead in % of party's total number of preference votes",
    x = "Share of intra-party preference votes; leader and runners-up."
  ) +
  # geom_point(aes(
  #   y = district_name_short,
  #   x = share_pref,
  #   shape = as_factor(index),
  #   color=party3
  # ),
  # stat = "identity"
  # ) +
  geom_point_interactive(aes(
    y = district_name_short,
    x = share_pref,
    shape = as_factor(index),
    color = party3,
    data_id = name_short,
    tooltip = name_short,
  ),
  stat = "identity"
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    labels = scales::percent_format(accuracy = 1),
    minor_breaks = NULL,
    breaks = seq(0, 1, .5)
  ) +
  scale_shape_manual(
    labels = c("1" = "leader", "2" = "runners-up"),
    values = c(1, 2)
  ) +
  scale_color_manual(values = party_colors) +
  lemon::facet_rep_grid(
    cols = vars(party3),
    rows = vars(district_type),
    repeat.tick.labels = F,
    scales = "free",
    space = "free",
    drop = T
  ) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.y = element_blank(),
    panel.spacing.x = unit(0.1, "cm"),
    panel.spacing.y = unit(0.2, "cm"),
    strip.text.y = element_text(angle = 0, vjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.justification = "right"
  ) +
  guides(color = "none")

d


# >> save ------------------------------------------------------------------

my_plot <- girafe(
  code = print(d),
  width_svg = 8,
  height_svg = 12,
  options = list(
    opts_toolbar(saveaspng = FALSE),
    opts_sizing(rescale = T)
  )
)
my_plot

x <- girafe_options(
  x = my_plot,
  sizingPolicy(
    padding = "0px"
  )
)

saveWidget(x,
  file = "pi_Difference_leading_runnersup_prefernece_votes.html",
  background = "gray"
)


name <- "Difference_leading_runnersup_prefernece_votes"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 30
width <- 30
ggsave(
  filename = paste0(folder, time, name, format),
  device = "png",
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)



# INCLUDED difference leader - runnersup vs n mandates -----------------------------

d1 <- df %>%
  left_join(., info_constituencies, by = c("district")) %>%
  filter(!party3 == "Other") %>%
  filter(!(district_type == "Bundeswahlkreis" & district != "Gesamt")) %>%
  filter(!is.na(pref_votes_abs)) %>%
  group_by(district_type, state, district, party3) %>%
  arrange(desc(pref_votes_abs), .by_group = T) %>%
  mutate(index = row_number()) %>%
  mutate(sum_preference = sum(pref_votes_abs, na.rm = T)) %>%
  filter(index < 3) %>%
  mutate(share_pref = pref_votes_abs / sum_preference) %>%
  mutate(indicator = district) %>%
  select(
    district_type, state, district, party3, name_short, district_name, n_mandates, district_name_short,
    indicator, share_pref, index
  ) %>%
  group_by(district_type, state, district, party3, district_name, district_name_short, indicator) %>%
  mutate(
    runners_up = lead(name_short),
    runners_up_share = lead(share_pref)
  ) %>%
  rename(
    leader = name_short,
    leader_share = share_pref
  ) %>%
  filter(index == 1) %>%
  mutate(share_diff = leader_share - runners_up_share) %>%
  ungroup() %>%
  mutate(n_mandates = fct_reorder(as_factor(n_mandates), as.numeric(n_mandates)))

plot_difference <- d1 %>%
  ggplot() +
  labs(
    title = "Difference between top-2 candidates' preference vote share by constituency list",
    y = "Differnce between top-2 candidates' preference vote share",
    caption = my_caption_2
  ) +
  geom_point_interactive(aes(
    x = party3,
    y = share_diff,
    color = party3,
    tooltip = paste(
      district_name_short, "\n",
      leader, round(leader_share * 100, 2), "%", "\n",
      runners_up, round(runners_up_share * 100, 2), "%", "\n"
    )
  ),
  pos = position_jitter(width = 0.2, height = NULL, seed = 2)
  ) +
  geom_boxplot(aes(
    x = party3,
    y = share_diff
  ),
  fill = "transparent",
  outlier.shape = NA
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent()
  ) +
  scale_color_manual(values = party_colors) +
  facet_wrap(vars(district_type)) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    plot.title = element_text(size = 12, face="bold.italic", margin=margin(b=0, unit="cm")),
    plot.title.position = "plot",
    legend.position = "none",
    plot.caption = element_markdown(hjust = c(0, 1)),
    axis.title.x = element_blank(),
    plot.margin = margin(0, unit = "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )



# >> save ------------------------------------------------------------------

my_plot <- girafe(
  code = print(plot_difference),
  width_svg = 10,
  height_svg = 6,
  options = list(
    opts_toolbar(saveaspng = FALSE),
    opts_sizing(rescale = T),
    opts_tooltip(css = "background-color:lightgray;	font-family:Roboto Condensed;")
  )
)
my_plot

x <- girafe_options(
  x = my_plot,
  sizingPolicy(padding = "0px")
)

saveWidget(x,
  file = "candidate_vote_share_difference.html",
  background = "white"
)




# NOT INCLUDED. Identify candidates which were 'upgrades' -----------------------------------------------------

# calculate share of preference votes from votes per constituency level
df_comb <- df_comb %>%
  mutate(p_pvpv_ratio = preference_votes / party_votes * 100) %>%
  mutate(threshold_votes = case_when(
    district_type == "Regionalwahlkreis" ~ 14,
    district_type == "Bundeswahlkreis" ~ 7,
    district_type == "Landeswahlkreis" ~ 10
  )) %>%
  mutate(upgrade = case_when(
    p_pvpv_ratio > threshold_votes ~ "yes",
    TRUE ~ as.character("no")
  )) %>%
  mutate(upgrade = case_when(
    district_type == "Landeswahlkreis" &
      preference_votes > electoral_number ~ "yes",
    TRUE ~ as.character(upgrade)
  ))

upgrades <- df_comb %>%
  group_by(district_type, district, district_name, party3, upgrade) %>%
  summarise(n_upgrades = n()) %>%
  filter(upgrade == "yes")
