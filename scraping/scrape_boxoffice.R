library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)
library(doParallel)
library(parallel)
library(rvest)
movies.all = readRDS('../RData/movies.all.rds')
movieIDs = unique(movies.all$imdbID)

## get information on actors and directors
getInfo = function(id) {
  url = sprintf('http://www.imdb.com/title/%s/?ref_=nv_sr_1', id)
  webHTML = read_html(url)
  details = webHTML %>% html_node('#titleDetails')
  h3s = details %>% html_nodes('h3') %>% html_text()
  idx = which(str_detect(h3s, 'Box Office'))
  boxoffice = data.frame(Name = character(), Value = character())
  if(length(idx) != 0) {
    xpath = sprintf("./div[count(preceding-sibling::h3)=%d]", idx)
    boxoffice = details %>% html_nodes(xpath = xpath)
    rowNames = boxoffice %>% html_nodes('h4') %>% html_text()
    boxoffice = boxoffice %>% html_text() %>% str_match_all('[\\$£][0-9,]+\\b') %>% unlist()
    boxoffice = data.frame(Name = rowNames, Value = boxoffice)
  }
  return(boxoffice)
}

cores = 24
registerDoParallel(cores = cores)

n = ceiling(length(movieIDs) / 2500)
lefts = 1 + 2500 * (1:n - 1)
rights = 2500 * (1:n)
rights[n] = length(movieIDs)

for(i in 1:n) {
  movieBO = foreach(id = movieIDs[lefts[i]:rights[i]]) %dopar%
    tryCatch(getInfo(id), error = function(e) {print(e); return(NULL)})
  saveRDS(movieBO, file = sprintf('../RData/movieBO%d.rds', i))
}

files = paste0('../RData/movieBO', 1:19, '.rds')
tmp = lapply(files, readRDS) %>% unlist(recursive = FALSE)
names(tmp) = movieIDs
saveRDS(tmp, file = '../RData/movieBO.rds')
