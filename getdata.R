library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)
library(doParallel)
library(parallel)
library(rvest)

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

## transform data
apiData = apiData[!sapply(apiData, is.null)]
## filter out tvs
apiData = apiData[!sapply(apiData, function(x) 'Season' %in% names(x))]
omdb = lapply(apiData, function(x) {
  x$Ratings = x$Ratings$Value[2] %>% str_replace('%', '')
  if(length(x$Ratings) == 0) x$Ratings = NA
  tmp = as.data.frame(x) %>% rename(rtRating = Ratings)
  return(tmp)
}) %>% bind_rows()
omdb = omdb %>% filter(Type == 'movie')
omdb[omdb == 'N/A'] = NA
num = . %>% str_replace_all(',', '') %>% as.numeric()
omdb = omdb %>%
  mutate(rtRating = num(rtRating), imdbRating = num(imdbRating) * 10,
         Metascore = num(Metascore), imdbVotes = num(imdbVotes), Year = num(Year)) %>%
  select(-c(Type, Response:totalSeasons)) %>%
  rename(Tomatometer = rtRating, `IMDb Rating` = imdbRating)

## genres using MovieLens data
genres = lapply(1:nrow(movieGenres), function(i) {
  gens = str_split(movieGenres$genres[i], '\\|')[[1]] %>% str_trim()
  data.frame(movieId = rep(movieGenres$movieId[i], length(gens)), Genre = gens)
}) %>% bind_rows()
movies.all = omdb %>% select(-Genre) %>%
  inner_join(movies %>% mutate(imdbId = paste0('tt', imdbId)), by = c('imdbID' = 'imdbId')) %>%
  inner_join(genres, by = 'movieId')

## actors table
actors = lapply(1:nrow(omdb), function(i) {
  acts = str_split(omdb$Actors[i], ',')[[1]] %>% str_trim()
  data.frame(imdbID = rep(omdb$imdbID[i], length(acts)), Actor = acts)
}) %>% bind_rows() %>% filter(!is.na(Actor) & Actor != '')
## directors table
directors = lapply(1:nrow(omdb), function(i) {
  dirs = str_split(omdb$Director[i], ',')[[1]] %>% str_trim()
  data.frame(imdbID = rep(omdb$imdbID[i], length(dirs)), Director = dirs)
}) %>% bind_rows() %>% filter(!is.na(Director) & Director != '')

save(movies.all, actors, directors, file = 'movies.all.RData')
load('movies.all.RData')

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
save(directorIDs, file = 'directorIDs.RData')
actorIDs = foreach(act = unique(actors$Actor)) %dopar%
  tryCatch(get_nmid(act),
           error = function(e) {print(sprintf('Error: Actor %s', act)); return(act)}
           )
names(actorIDs) = unique(actors$Actor)
save(actorIDs, file = 'actorIDs.RData')

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
save(dirInfos, file = 'dirInfos.RData')

  ## tryCatch({
  ## })
