library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)
library(doParallel)
library(parallel)
library(rvest)
load('../RData/movies.all.RData')

## get the name ids
## the api just mysterious broke, needs a new function
get_nmid = function(name) {
  ## url_format = 'http://www.imdb.com/xml/find?json=1&nr=1&nm=on&q=%s'
  url_format = 'http://sg.media-imdb.com/suggests/%s/%s.json'
  search_str = str_split(name, ' ')[[1]] %>% str_trim() %>% str_to_lower() %>%
    paste0(collapse = '+')
  url = sprintf(url_format, str_sub(search_str, end = 1), search_str)
  response = GET(url) %>% content(as = 'text', encoding = 'utf-8')
  idx = str_locate(response, '\\(')[1, 1]
  response = fromJSON(str_sub(response, idx + 1, str_length(response) - 1))
  ## nmid = response$name_popular$id
  id = response$d$id[1]
  image = response$d$i[[1]][1]
  return(list(id = id, image = image))
} 

cores = 24 
registerDoParallel(cores = cores)
directorIDs = foreach(dir = unique(directors$Director)) %dopar%
  tryCatch({print(dir); get_nmid(dir)},
           error = function(e) {print(sprintf('Error: Director %s', dir)); return(NULL)}
           )
names(directorIDs) = unique(directors$Director)
save(directorIDs, file = '../RData/directorIDs.RData')
actorIDs = foreach(act = unique(actors$Actor)) %dopar%
  tryCatch(get_nmid(act),
           error = function(e) {print(sprintf('Error: Actor %s', act)); return(act)}
           )
names(actorIDs) = unique(actors$Actor)
save(actorIDs, file = '../RData/actorIDs.RData')
