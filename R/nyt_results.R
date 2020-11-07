## NYT election results ========================================================
library(tidyverse)
library(lubridate)
library(jsonlite)
library(httr)

url <- paste0(
  "https://static01.nyt.com/elections-assets/2020/data/api/2020-11-03/",
  "national-map-page/national/president.json"
)
snap_time <- format(Sys.time(), "%Y%m%d%H%M%S")

nyt_result <- list()
while (substr(snap_time, 1, 8) != "20220101") {
  
  tryCatch({
    snap_time <- format(Sys.time(), "%Y%m%d%H%M%S")
    nyt_result[[snap_time]] <- 
      fromJSON(content(GET(url), as = "text"), flatten = TRUE)
    nyt_recent <- nyt_result[[length(nyt_result)]]
    
    save(nyt_result, file = "data/nyt_result.Rda")
    save(nyt_recent, file = "data/nyt_recent.Rda")
    print(snap_time)
    Sys.sleep(295)
    
  }, error = function(e) {
    print(e)
  })
}
