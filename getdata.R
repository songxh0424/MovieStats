library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)
library(doParallel)

cores = 24

movies = read_csv('./data/links.csv')
movieGenres = read_csv('./data/movies.csv')
## get ratings from OMDb API
api_key = '896050f3'
url_prefix = sprintf('http://www.omdbapi.com/?apikey=%s&i=tt', api_key)

requestData = function(id) {
  url = paste0(url_prefix, str_pad(id, width = 7, pad = '0'))
  response = GET(url) %>% content(as = 'text') %>% fromJSON()
  if('Error' %in% names(response)) return(NULL)
  return(response)
}

if(!file.exists('apiData.RData')) {
  registerDoParallel(cores = cores)
  apiData = foreach(id = movies$imdbId) %dopar% requestData(id)
  save(apiData, file = 'apiData.RData')
} else {
  load('apiData.RData')
}
