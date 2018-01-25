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

cores = 24
if(!file.exists('./RData/apiData.rds')) {
  registerDoParallel(cores = cores)
  apiData = foreach(id = movies$imdbId) %dopar% tryCatch(requestData(id), error = function(e) NULL)
  saveRDS(apiData, file = './RData/apiData.rds')
} else {
  load('./RData/apiData.RData')
  apiData = readRDS('./RData/apiData.rds')
}

## transform data
apiData = apiData[!sapply(apiData, is.null)]
## filter out tvs
apiData = apiData[!sapply(apiData, function(x) 'Season' %in% names(x))]
omdb = mclapply(apiData, function(x) {
  x$Ratings = x$Ratings$Value[2] %>% str_replace('%', '')
  if(length(x$Ratings) == 0) x$Ratings = NA
  tmp = as.data.frame(x) %>% rename(rtRating = Ratings)
  return(tmp)
}, mc.cores = cores) %>% bind_rows()
omdb = omdb %>% filter(Type == 'movie')
omdb[omdb == 'N/A'] = NA
num = . %>% str_replace_all(',', '') %>% as.numeric()
omdb = omdb %>%
  mutate(rtRating = num(rtRating), imdbRating = num(imdbRating) * 10,
         Metascore = num(Metascore), imdbVotes = num(imdbVotes), Year = num(Year)) %>%
  select(-c(Type, Response:totalSeasons)) %>%
  rename(Tomatometer = rtRating, `IMDb Rating` = imdbRating)
saveRDS(omdb, file = './RData/omdb.rds')

## genres using MovieLens data
genres = lapply(1:nrow(movieGenres), function(i) {
  gens = str_split(movieGenres$genres[i], '\\|')[[1]] %>% str_trim()
  data.frame(movieId = rep(movieGenres$movieId[i], length(gens)), Genre = gens)
}) %>% bind_rows()
movies.all = omdb %>% select(-Genre) %>%
  inner_join(movies %>% mutate(imdbId = paste0('tt', imdbId)), by = c('imdbID' = 'imdbId')) %>%
  inner_join(genres, by = 'movieId')
## Box office table
movieBO = readRDS('./RData/movieBO.rds')
boxoffice = lapply(1:length(movieBO), function(i) {
  if(is.null(movieBO[[i]])) {
    data.frame(imdbID = names(movieBO)[i])
  } else if(nrow(movieBO[[i]]) == 0) {
    data.frame(imdbID = names(movieBO)[i])
  } else {
    movieBO[[i]] %>% mutate(Name = str_replace(Name, ':', ''), imdbID = names(movieBO)[i]) %>%
      spread(key = Name, value = Value)
  }
}) %>% bind_rows()
boxoffice[is.na(boxoffice)] = '$0'
saveRDS(boxoffice, file = './RData/boxoffice.rds')
movies.all = movies.all %>% inner_join(boxoffice, by = 'imdbID') %>%
  mutate(Title = str_trim(Title),
         `Estimated Budget` = Budget %>% str_sub(start = 2) %>% str_replace_all(',', '') %>% as.numeric(),
         BoxOffice = ifelse(is.na(`Gross USA`), BoxOffice, `Gross USA`)) %>%
  mutate(BoxOffice = BoxOffice %>% str_sub(start = 2) %>% str_replace_all(',', '') %>% as.numeric())
saveRDS(movies.all, file = './RData/movies.all.rds')

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
acts = actors %>% filter(movies >= 7)
dirs = directors %>% filter(movies >= 5)
saveRDS(acts, file = './RData/acts.rds')
saveRDS(dirs, file = './RData/dirs.rds')
## clear out all TV movies
movieTV = readRDS('./RData/movieTV.rds')
tvIDs = names(which(unlist(movieTV)))
movies.all = filter(movies.all, !(imdbID %in% tvIDs))
dirs = filter(dirs, !(imdbID %in% tvIDs))
acts = filter(acts, !(imdbID %in% tvIDs))
saveRDS(movies.all, file = './RData/movies.all.rds')
saveRDS(acts, file = './RData/acts.rds')
saveRDS(dirs, file = './RData/dirs.rds')

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

## tables for genres trend
genre_tb1 = movies.all %>% filter(Year >= 1930, Year <= 2017) %>% group_by(imdbID) %>%
  summarise(Year = Year[1], Count = n()) %>%
  group_by(Year) %>% summarise(Count = mean(Count))
genre_tb2 = movies.all %>% filter(!(Genre %in% c('(no genres listed)', 'IMAX', 'Film-Noir')), Year >= 1930, Year <= 2017) %>%
  group_by(Year) %>% mutate(Total = length(unique(imdbID))) %>% ungroup() %>% group_by(Year, Genre) %>%
  summarise(Count = n(), Total = first(Total)) %>% mutate(Percentage = round(Count / Total * 100, 2))
p = ggplot(genre_tb2, aes(Year, Percentage, color = Genre, name = Count)) + geom_line(size = 0.3)
saveRDS(plot_custom(p), './RData/genre_trend_plot.rds')

## Word frequency
words.ls = str_extract_all((movies.all %>% group_by(imdbID) %>% filter(row_number() == 1))$Plot, '\\b\\w+\\b')
words = lapply(words.ls, function(wds) data.frame(word = wds, count = rep(1, length(wds)))) %>%
  bind_rows() %>% mutate(word = str_to_lower(word)) %>% group_by(word) %>%
  summarise(count = sum(count)) %>% arrange(desc(count))
words.top = words %>% filter(count >= 200)
## word freq vs. rating
movies.quant = movies.all %>% group_by(imdbID) %>% filter(row_number() == 1, !is.na(`IMDb Rating`)) %>%
  ungroup() %>% select(Plot, `IMDb Rating`) %>% rename(Rating = `IMDb Rating`) 
tmp = movies.quant %>% summarise(q1 = quantile(Rating, 0.25), q2 = quantile(Rating, 0.5), q3 = quantile(Rating, 0.75))
movies.quant = movies.quant %>% mutate(q1 = tmp$q1, q2 = tmp$q2, q3 = tmp$q3)

words.quant.ls = lapply(words.top$word, function(wd) {
  movies.tmp = filter(movies.quant, str_detect(Plot, sprintf('\\b%s\\b', wd)))
  out = movies.tmp %>% summarise(Top25 = sum(Rating >= q3), Bottom25 = sum(Rating <= q1), Count = n()) %>%
    mutate(Word = wd, log_ratio = log2((Top25 + 0.01) / (Bottom25 + 0.01)) %>% round(2))
  return(out)
})
words.quant = words.quant.ls %>% bind_rows() %>% arrange(desc(log_ratio)) %>% filter(Top25 + Bottom25 > 100)
p = words.quant %>% filter(row_number() <= 10 | row_number() > n() - 10) %>% arrange(log_ratio) %>%
  mutate(Word = factor(Word, levels = Word), color = factor(Top25 > Bottom25, levels = c(TRUE, FALSE))) %>%
  ggplot(aes(Word, log_ratio, fill = color, Top25 = Top25, Bottom25 = Bottom25, Count = Count)) + geom_col() + coord_flip()
saveRDS(plot_custom(p, legend.pos = 'none', fill = TRUE, palette = 'tableau10'), file = './RData/bar_word.rds')
