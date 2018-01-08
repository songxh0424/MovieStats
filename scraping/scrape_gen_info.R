library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)
library(doParallel)
library(parallel)
library(rvest)
load('../RData/directorIDs.RData')
load('../RData/actorIDs.RData')

## get information on actors and directors
getInfo = function(id) {
  url = sprintf('http://www.imdb.com/name/%s/?ref_=nv_sr_1', id)
  webHTML = read_html(url)
  born = webHTML %>% html_node('#name-born-info')
  bdate = born %>% html_node('time') %>% html_text() %>% str_replace_all('\\s+', ' ') %>% str_trim()
  bplace = born %>% html_nodes('a') %>% last() %>% html_text() %>% str_replace_all('\\s+', ' ') %>% str_trim()
  ## aka = webHTML %>% html_nodes('#details-akas') %>% html_text()
  height = webHTML %>% html_nodes('#details-height') %>% html_text() %>%
    str_replace_all('\\s+', '') %>% str_replace('Height:', '')

  ## url_trademark = sprintf('http://www.imdb.com/name/%s/bio?ref_=nm_dyk_tm_sm#trademark', id)

  ## url_awards = sprintf('http://www.imdb.com/name/%s/awards?ref_=nm_ql_2', id)
  ## awardHTML = read_html(url_awards)
  ## tables = html_nodes(awardHTML, 'table')
  return(list(bdate = bdate, bplace = bplace, height = height))
}
cores = 24 
registerDoParallel(cores = cores)
dirInfos = foreach(dir = directorIDs) %dopar% 
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = dir$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
names(dirInfos) = names(directorIDs)
save(dirInfos, file = '../RData/dirInfos.RData')

cores = 24 
registerDoParallel(cores = cores)
actInfos = foreach(act = actorIDs) %dopar% 
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = act$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
names(actInfos) = names(actorIDs)
save(actInfos, file = '../RData/actInfos.RData')

  ## tryCatch({
  ## })
