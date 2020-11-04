library(tidyverse)
library(httr)
library(jsonlite)

## Export class predictions from Govt 656 ======================================
class_656 <- read.csv("data/prediction-2020-no-pii.csv") %>%
  rename_with(
    ~ gsub(
      c(
        "Winners.in.winner.take.all.states",
        "key.battleground",
        "Trump.projected.to.win",
        "Biden.projected.to.win",
        "\\."
      ) %>%
        paste0(collapse = "|"),
      "", .x
    )
  ) %>%
  rename_with(~ tolower(gsub("", "", .x))) %>%
  rename(ne = nebraska) %>%
  mutate(
    ne = trimws(gsub("electoral votes", "", ne)),
    me = trimws(gsub("electoral votes", "", me))
  ) %>%
  mutate(
    across(
      c(setdiff(tolower(state.abb), c("ne", "me")), "dc"),
      ~ ifelse(.x == "Trump", 0, 1)
    )
  ) %>%
  mutate(
    ne = case_when(
      ne == "1/3" ~ 1 / 3,
      ne == "2/3" ~ 2 / 3,
      ne == "3/3" ~ 1
    ),
    me = ifelse(me == "1/2", 1 / 2, 1)
  )

## 16 responses, all different!
class_656 %>%
  .[duplicated(.), ]

## Compare to existing predictions and results =================================
load("data/pollster_forecasts_tidy.Rda")
load("data/nyt_result.Rda")

df <- nyt_result[[length(nyt_result)]] %>%
  .$data %>%
  .$races %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  select(
    state = state_id, result, margin = leader_margin_display ## ,
    ## label = update_sentences.top_level.generatedText
  ) %>%
  mutate(
    candidate = ifelse(grepl("R+", margin), 0, 1)
  ) %>%
  select(state, candidate) %>%
  t() %>%
  as_tibble() %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ] %>%
  rename_all(tolower) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(name = "NYT Election Results (Tentative)") %>%
  mutate(
    me = (1 + 0.5) / 2,  ## 2nd district undecided
    ne = (0 + 1 + 0) / 3 ## 2nd district to Biden, 1st/3rd to Trump
  ) %>%
  bind_rows(., forecast_final) %>%
  bind_rows(., class_656) %>%
  select(name, everything())

vec <- df %>%
  t() %>%
  as_tibble(rownames = "state") %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ] %>%
  janitor::clean_names() %>%
  select(-name) %>%
  mutate_all(
    ~ (as.numeric(.x) -
         (df[1, ] %>% unlist() %>% .[2:length(.)] %>% as.numeric()))^2
  ) %>%
  colSums() %>%
  .[2:length(.)] %>%
  sort(.)
vec

class_656 %>% 
  filter(
    az == 1 & fl == 0 & ga == 0 & mi == 1 & nc == 0 & 
      oh == 0 & pa == 0 & tx == 0 & wi == 1
  )
