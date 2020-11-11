library(tidyverse)
library(rvest)
library(jsonlite)
source(
  'https://raw.githubusercontent.com/sysilviakim/Kmisc/master/R/transpose.R'
)
## How many Electoral College electors per state? ==============================
## Pre-written Gist
source(
  paste0(
    "https://gist.githubusercontent.com/sysilviakim/", 
    "846f70acc02ae8b80b00ac6ec4d27cc4/raw/db1c1da1da43558df27a97583f9cb8327", 
    "a433564/population-per-elector.R"
  )
)

electors <- electors %>%
  left_join(
    ., 
    bind_rows(
      tibble(abb = "DC", state = "District of Columbia"),
      tibble(abb = state.abb, state = state.name)
    )
  ) %>%
  select(state = abb, electors) %>%
  mutate(state = tolower(state)) %>%
  transpose() %>%
  mutate_all(as.numeric) %>%
  mutate(name = "Electoral College")
save(electors, file = "data/electoral_college.Rda")
