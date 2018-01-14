library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)
library(doParallel)
library(parallel)
library(rvest)

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

if(!file.exists('./RData/apiData.RData')) {
  registerDoParallel(cores = cores)
  apiData = foreach(id = movies$imdbId) %dopar% requestData(id)
  save(apiData, file = './RData/apiData.RData')
} else {
  load('./RData/apiData.RData')
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
  inner_join(genres, by = 'movieId') %>%
  mutate(Title = str_trim(Title),
         BoxOffice = BoxOffice %>% str_sub(start = 2) %>% str_replace_all(',', '') %>% as.numeric())

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
## too many actors and directors, only keep the ones with a good amount of films
actors = actors %>% group_by(Actor) %>% mutate(movies = length(unique(imdbID))) %>% arrange(desc(movies))
directors = directors %>% group_by(Director) %>% mutate(movies = length(unique(imdbID))) %>% arrange(desc(movies))
acts = actors %>% filter(movies > 4)
dirs = directors %>% filter(movies > 2)
save(movies.all, actors, directors, file = './RData/movies.all.RData')
saveRDS(movies.all, file = './RData/movies.all.rds')
saveRDS(acts, file = './RData/acts.rds')
saveRDS(dirs, file = './RData/dirs.rds')
load('./RData/movies.all.RData')

## Oscar tables
dirOscar = readRDS('./RData/dirOscar.rds')
actOscar = readRDS('./RData/actOscar.rds')
dirOscar_tb = lapply(1:length(dirOscar), function(i) {
  if(is.null(dirOscar[[i]]$oscar)) return(NULL)
  dirOscar[[i]]$oscar %>% summarise(Won = sum(Result), Nominated = n(), `Nominated Years` = paste0(Year, collapse = ', '),
                                    `Won Years` = paste0(Year[which(Result)], collapse = ', ')) %>%
    mutate(Name = names(dirOscar)[i]) %>% select(Name, Won, Nominated, `Won Years`, `Nominated Years`)
}) %>% bind_rows() %>% arrange(desc(Nominated), Won)
actOscar_tb = lapply(1:length(actOscar), function(i) {
  if(is.null(actOscar[[i]]$oscar)) return(NULL)
  actOscar[[i]]$oscar %>% summarise(Won = sum(Result), Nominated = n(), `Nominated Years` = paste0(Year, collapse = ', '),
                                    `Won Years` = paste0(Year[which(Result)], collapse = ', ')) %>%
    mutate(Name = names(actOscar)[i]) %>% select(Name, Won, Nominated, `Won Years`, `Nominated Years`)
}) %>% bind_rows() %>% arrange(desc(Nominated), Won)
saveRDS(dirOscar_tb, file = './RData/dirOscar_tb.rds')
saveRDS(actOscar_tb, file = './RData/actOscar_tb.rds')

## tables for polarizing movies
polar_tb = movies.all %>% select_('Title', 'Genre', 'Year', choice_1, choice_2) %>%
  mutate_(Difference = abs(choice_1 - choice_2)) %>% arrange(desc(Deference)) %>% filter(Difference >= 30)
top_polar = polar_tb %>% group_by(imdbID) %>% filter(row_number() == 1) %>% head(50)
saveRDS(polar_tb, file = './RData/polar_tb.rds')
saveRDS(top_polar, file = './RData/top_polar.rds')
