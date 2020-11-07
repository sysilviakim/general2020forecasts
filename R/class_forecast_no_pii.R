library(tidyverse)
library(httr)
library(jsonlite)
library(janitor)

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
  rename_with(tolower) %>%
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
load("data/nyt_recent.Rda")
load("data/electoral_college.Rda")

df_compare <- nyt_recent %>%
  .$data %>%
  .$races %>%
  select_if(~ sum(!is.na(.)) > 0) %>%
  select(
    state = state_id, result, margin = leader_margin_display ## ,
    ## label = update_sentences.top_level.generatedText
  ) %>%
  mutate(candidate = ifelse(grepl("R+", margin), 0, 1)) %>%
  select(state, candidate) %>%
  t() %>%
  as_tibble(.name_repair = "unique") %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ] %>%
  rename_all(tolower) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(name = "NYT Election Results (Tentative)") %>%
  mutate(
    me = (1 + 0.5) / 2, ## 2nd district undecided
    ne = (0 + 1 + 0) / 3 ## 2nd district to Biden, 1st/3rd to Trump
  ) %>%
  bind_rows(
    ., 
    forecast_final,
    class_656,
    electors
  ) %>%
  select(name, sort(c(tolower(state.abb), "dc")), everything())

## Final result ================================================================
temp <- df_compare %>%
  t() %>%
  as_tibble(rownames = "state", .name_repair = "unique") %>%
  `colnames<-`(.[1, ]) %>%
  .[-1, ] %>%
  clean_names() %>%
  select(-name) %>%
  mutate_all(as.numeric)

## Unweighted with Electoral College Votes
temp %>%
  mutate_all(
    ~ (.x - temp$nyt_election_results_tentative)^2
  ) %>%
  colSums() %>%
  .[2:(length(.) - 1)] %>%
  sort(.)

## Weighted with Electoral College Votes
temp %>%
  mutate_all(
    ~ (.x - temp$nyt_election_results_tentative)^2 * temp$electoral_college
  ) %>%
  colSums() %>%
  .[2:(length(.) - 1)] %>%
  sort(.)