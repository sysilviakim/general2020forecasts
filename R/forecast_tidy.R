library(tidyverse)
load("data/pollster_forecasts.Rda")

forecast_final <- list(
  sabato %>%
    mutate(
      predict_category = case_when(
        grepl("-D", predict_category) ~ 1,
        grepl("-R", predict_category) ~ 0,
        grepl("Toss", predict_category) ~ 0.5
      )
    ) %>%
    t() %>% 
    as_tibble() %>%
    `colnames<-`(.[1, ]) %>%
    .[-1, ] %>%
    rename_all(tolower) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(
      me = (me01 + me02) / 2,
      ne = (ne01 + ne02 + ne03) / 3
    ) %>%
    select(-me01, -me02, -ne01, -ne02, -ne03) %>%
    mutate(name = "Sabato Crystal Ball"),
  fivethirtyeight %>%
    select(state, candidate) %>%
    t() %>% 
    as_tibble() %>%
    `colnames<-`(.[1, ]) %>%
    .[-1, ] %>%
    rename_all(tolower) %>%
    mutate_all(~ ifelse(.x == "Trump", 0, 1)) %>%
    select(-me, -ne) %>%
    mutate(
      me = (me1 + me2) / 2,
      ne = (ne1 + ne2 + ne3) / 3
    ) %>%
    select(-me1, -me2, -ne1, -ne2, -ne3) %>%
    mutate(name = "FiveThirtyEight"),
  economist %>%
    left_join(
      ., 
      tibble(
        abb = state.abb,
        state = gsub(" ", "-", tolower(state.name))
      ) %>%
        bind_rows(., tibble(abb = "DC", state = "washington-dc"))
    ) %>%
    select(state = abb, candidate) %>%
    t() %>% 
    as_tibble() %>%
    `colnames<-`(.[1, ]) %>%
    .[-1, ] %>%
    rename_all(tolower) %>%
    ## Nothing specific to splits in Maine or Nebraska
    mutate_all(~ ifelse(.x == "Trump", 0, 1)) %>%
    mutate(name = "The Economist"),
  cnn %>%
    select(state = stateCode, candidate = party) %>%
    mutate(
      candidate = case_when(
        grepl("Dem", candidate) ~ 1,
        grepl("Rep", candidate) ~ 0,
        grepl("Toss", candidate) ~ 0.5
      )
    ) %>%
    t() %>% 
    as_tibble() %>%
    `colnames<-`(.[1, ]) %>%
    .[-1, ] %>%
    rename_all(tolower) %>%
    select(-me, -ne) %>%
    mutate_if(is.character, as.numeric) %>%
    mutate(
      ## Hardcoded due to complexity
      me = (1 + 0.5) / 2, ## CD1 Solid-D, CD2 = Tossup
      ne = (0 + 1 + 0) / 3, ## CD1 Solid-R, CD-2 Lean-D, CD-3 Solid-R
      name = "CNN"
    ),
  politico %>%
    select(state, candidate) %>%
    t() %>% 
    as_tibble() %>%
    `colnames<-`(.[1, ]) %>%
    .[-1, ] %>%
    rename_all(tolower) %>%
    ## Nothing specific to splits in Maine or Nebraska
    mutate_all(~ ifelse(.x == "Trump", 0, 1)) %>%
    select(-me, -ne) %>%
    mutate(
      me = (me_01 + me_02) / 2,
      ne = (ne_01 + ne_02 + ne_03) / 3
    ) %>%
    select(-me_01, -me_02, -ne_01, -ne_02, -ne_03) %>%
    mutate(name = "Politico"),
  cookpolitical %>%
    mutate(state = gsub(" ", "-", tolower(state))) %>%
    left_join(
      ., 
      bind_rows(
        tibble(abb = "DC", state = "washington-dc"),
        tibble(
          abb = state.abb,
          state = gsub(" ", "-", tolower(state.name))
        )
      )
    ) %>%
    mutate(
      abb = case_when(
        state == "nebraska-1st-cd" ~ "NE01",
        state == "nebraska-2nd-cd" ~ "NE02",
        state == "nebraska-3rd-cd" ~ "NE03",
        state == "maine-1st-cd" ~ "ME01",
        state == "maine-2nd-cd" ~ "ME02",
        TRUE ~ abb
      )
    ) %>%
    select(state = abb, predict_category) %>%
    mutate(
      predict_category = case_when(
        grepl("-D", predict_category) ~ 1,
        grepl("-R", predict_category) ~ 0,
        grepl("Toss", predict_category) ~ 0.5
      )
    ) %>%
    t() %>% 
    as_tibble() %>%
    `colnames<-`(.[1, ]) %>%
    .[-1, ] %>%
    rename_all(tolower) %>%
    mutate_if(is.character, as.numeric) %>%
    select(-me, -ne) %>%
    mutate(
      me = (me01 + me02) / 2,
      ne = (ne01 + ne02 + ne03) / 3
    ) %>%
    select(-me01, -me02, -ne01, -ne02, -ne03) %>%
    mutate(name = "Cook Political Report")
) %>%
  bind_rows()

save(forecast_final, file = "data/pollster_forecasts_tidy.Rda")
