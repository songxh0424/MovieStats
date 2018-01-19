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
  release = webHTML %>% html_node('.subtext')
  tv = release %>% html_text() %>% str_to_lower() %>% str_detect('tv')
  return(tv)
}

cores = 24
registerDoParallel(cores = cores)

n = ceiling(length(movieIDs) / 2500)
lefts = 1 + 2500 * (1:n - 1)
rights = 2500 * (1:n)
rights[n] = length(movieIDs)

for(i in 1:n) {
  movieTV = foreach(id = movieIDs[lefts[i]:rights[i]]) %dopar%
    tryCatch(getInfo(id), error = function(e) {print(e); return(NULL)})
  saveRDS(movieTV, file = sprintf('../RData/movieTV%d.rds', i))
}

files = paste0('../RData/movieTV', 1:19, '.rds')
tmp = lapply(files, readRDS) %>% unlist(recursive = FALSE)
names(tmp) = movieIDs
saveRDS(tmp, file = '../RData/movieTV.rds')
