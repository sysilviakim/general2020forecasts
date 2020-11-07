## FiveThirtyEight GitHub does not contain state by state forecasts
## https://github.com/fivethirtyeight/data/tree/master/election-forecasts-2020

library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(textreadr)

## FiveThirtyEight =============================================================
url <- paste0(
  "https://projects.fivethirtyeight.com/2020-election-forecast/", 
  "state_recirc.json"
)

fivethirtyeight <- 
  fromJSON(content(GET(url), as = "text"), flatten = TRUE) %>% 
  .$president %>%
  .$states %>%
  .[[1]] %>%
  unnest(candidates) %>% 
  filter(candidate == "Trump") %>%
  mutate(candidate = ifelse(margin_mean > 0, "Trump", "Biden"))

## Economist ===================================================================
url <- "https://projects.economist.com/us-2020-forecast/president/"
economist <- c(state.name, "washington-dc") %>%
  map(~ tolower(gsub(" ", "-", .x))) %>%
  set_names(., .) %>%
  map(
    ~ {
      r <- paste0(url, .x) %>%
        xml2::read_html()
      
      r1 <- html_nodes(
        r, xpath = '//*[@class="table-highlight table-highlight-oneline"]'
      ) %>%
        ## Biden's predicted popular vote share
        .[[1]] %>% 
        html_text() %>%
        gsub("%", "", .) %>%
        str_split(., pattern = "-") %>%
        .[[1]] %>%
        as.numeric()
      
      r2 <- html_nodes(r, xpath = '//*[@class="table-pct"]') %>%
        ## Biden's predicted chance of winning the most votes
        .[[1]] %>% 
        html_text() %>%
        parse_number() %>%
        as.numeric()
      
      return(
        tibble(
          biden_min = r1[1],
          biden_max = r1[2],
          predict = r2
        ) %>%
          mutate(candidate = ifelse(predict > 50, "Biden", "Trump"))
      )
    }
  ) %>% 
  bind_rows(.id = "state")

## Politico ====================================================================
url <- paste0(
  "https://www.politico.com/2020-election/race-forecasts-and-predictions/", 
  "president/"
)
politico <- xml2::read_html(url) %>%
  html_nodes(xpath = '//*[@id="__NEXT_DATA__"]') %>%
  html_text() %>%
  fromJSON() 
politico <- tibble(
  state = gsub("-potus", "", politico$props$pageProps$rawRatings$ratings$id),
  predict_category = 
    politico$props$pageProps$rawRatings$ratings$rating_category
) %>%
  mutate(
    candidate = ifelse(grepl("-R", predict_category), "Trump", "Biden")
  )

## CNN =========================================================================
url <- paste0(
  "https://www.cnn.com/election/2020/", 
  "electoral-college-interactive-maps#2020-CNN-map"
)
cnn <- xml2::read_html(url) %>%
  html_nodes(xpath = '//*[@id="__NEXT_DATA__"]') %>%
  html_text() %>%
  fromJSON() %>%
  .$props %>%
  .$pageProps %>%
  .$geoStates %>%
  .$`2020 CNN map` %>%
  .$roadTo270 %>%
  .$data

## Cook Political Report =======================================================
url <- paste0(
  "https://cookpolitical.com/sites/default/files/2020-10/", 
  "EC%20Ratings.102820.pdf"
)

r <- read_pdf(url) %>%
  .$text %>% 
  paste0(collapse = "|") %>%
  gsub("\\s+", " ", .)

r1 <- r %>%
  str_match_all(., "SOLID REPUBLICAN\\s*(.*?)\\s*Note: ") %>%
  .[[1]] %>%
  .[1, 2] %>%
  gsub("\\|", " ", .) %>%
  trimws() %>%
  str_split(., pattern = " States") %>%
  .[[1]] %>%
  .[1:7] %>%
  map(
    ~ str_split(.x, " ") %>%
      .[[1]]
  ) %>%
  map(~ as.numeric(.x[length(.x)])) %>%
  unlist()

r1[1] <- r1[1] + 2 ## Solid Democrat:   ME-01 + DC
r1[3] <- r1[3] + 1 ## Lean Democrat:    NE-02
r1[4] <- r1[4] + 1 ## Toss Up:          ME-02
r1[7] <- r1[7] + 2 ## Solid Republican: NE-01 + NE-03

r2 <- r %>%
  str_match_all(., "Electoral Votes\\|\\s*(.*?)\\s*\\|Note: ") %>%
  .[[1]] %>%
  .[1, 2] %>%
  gsub("\\|", " ", .) %>%
  str_split(., " \\(") %>%
  .[[1]] %>%
  map(
    ~ trimws(gsub("\\)", "", str_replace(.x, "[:digit:]{1,2}", "")))
  ) %>%
  unlist() %>%
  .[. != ""]

cookpolitical <- tibble(
  state = r2,
  predict_category = c(
    rep("Solid-D", r1[1]),
    rep("Likely-D", r1[2]),
    rep("Lean-D", r1[3]),
    rep("Toss-Up", r1[4]),
    rep("Lean-R", r1[5]),
    rep("Likely-R", r1[6]),
    rep("Solid-R", r1[7])
  )
)

## Sabato Crystal Ball =========================================================
## Absolutely no idea how to automate this. Toss-ups called
url <- "https://centerforpolitics.org/crystalball/articles/21320/"
sabato <- c(
  WA = "Solid-D", OR = "Solid-D", CA = "Solid-D", IL = "Solid-D",
  NY = "Solid-D", VT = "Solid-D", MA = "Solid-D", RI = "Solid-D",
  CT = "Solid-D", NJ = "Solid-D", DE = "Solid-D", DC = "Solid-D",
  HI = "Solid-D", MD = "Solid-D",
  
  ME01 = "Solid-D", ME02 = "Lean-R",
  NE01 = "Solid-R", NE02 = "Lean-D", NE03 = "Solid-R",
  
  CO = "Likely-D", NM = "Likely-D", MN = "Likely-D", NH = "Likely-D",
  VA = "Likely-D",
  
  NV = "Lean-D", AZ = "Lean-D", WI = "Lean-D", MI = "Lean-D",
  PA = "Lean-D", NC = "Lean-D", GA = "Lean-D",
  
  TX = "Lean-R", IA = "Lean-R", OH = "Lean-R", FL = "Lean-R",
  
  AK = "Likely-R", MT = "Likely-R", UT = "Likely-R", KS = "Likely-R",
  MO = "Likely-R", IN = "Likely-R", SC = "Likely-R",
  
  ID = "Solid-R", WY = "Solid-R", ND = "Solid-R", SD = "Solid-R",
  OK = "Solid-R", AR = "Solid-R", LA = "Solid-R", MS = "Solid-R",
  AL = "Solid-R", TN = "Solid-R", KY = "Solid-R", WV = "Solid-R"
) %>% 
  as_tibble(rownames = "state") %>%
  rename(predict_category = value)

## Alan I. Abramowitz ==========================================================
## Unfortunately, another image
url <- paste0(
  "https://centerforpolitics.org/crystalball/articles/", 
  "final-forecast-results-from-two-methods-of-predicting-the-", 
  "2020-presidential-election/"
)
abramowitz <- c(
  WA = "Solid-D", OR = "Solid-D", CA = "Solid-D", IL = "Solid-D",
  NY = "Solid-D", VT = "Solid-D", MA = "Solid-D", RI = "Solid-D",
  CT = "Solid-D", NJ = "Solid-D", DE = "Solid-D", DC = "Solid-D",
  HI = "Solid-D", MD = "Solid-D", CO = "Solid-D", NM = "Solid-D", 
  VA = "Solid-D",
  
  ME01 = "Solid-D", ME02 = "Solid-D", ## No specific district-level classif.
  NE01 = "Solid-R", NE02 = "Solid-R", NE03 = "Solid-R",
  
  MN = "Likely-D", NH = "Likely-D", MI = "Likely-D", NV = "Likely-D", 
  PA = "Likely-D", WI = "Likely-D", 
  
  AZ = "Lean-D", FL = "Lean-D", NC = "Lean-D", GA = "Lean-D",
  
  TX = "Lean-R", IA = "Lean-R", OH = "Lean-R", 
  
  AK = "Likely-R", MT = "Likely-R", KS = "Likely-R",
  MO = "Likely-R", IN = "Likely-R", SC = "Likely-R",
  
  ID = "Solid-R", WY = "Solid-R", ND = "Solid-R", SD = "Solid-R",
  OK = "Solid-R", AR = "Solid-R", LA = "Solid-R", MS = "Solid-R",
  AL = "Solid-R", TN = "Solid-R", KY = "Solid-R", WV = "Solid-R",
  UT = "Solid-R"
) %>% 
  as_tibble(rownames = "state") %>%
  rename(predict_category = value)

## Skipping Inside Elections, NYT 3-state needle, U.S. News & World, ===========
## and PredictIt, despite 
## https://www.270towin.com/content/forecasts-in-the-2020-consensus-map
save(
  list = c(
    "fivethirtyeight", "economist", "politico", "cnn", 
    "cookpolitical", "sabato", "abramowitz"
  ),
  file = "data/pollster_forecasts.Rda"
)
