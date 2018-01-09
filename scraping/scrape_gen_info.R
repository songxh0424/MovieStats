library(stringr)
library(dplyr)
library(tidyr)
library(readr)
library(httr)
library(jsonlite)
library(doParallel)
library(parallel)
library(rvest)
dirIDs = readRDS('../RData/dirIDs.rds')
actIDs = readRDS('../RData/actIDs.rds')

## get information on actors and directors
getInfo = function(id) {
  url = sprintf('http://www.imdb.com/name/%s/?ref_=nv_sr_1', id)
  webHTML = read_html(url)
  born = webHTML %>% html_node('#name-born-info')
  bdate = born %>% html_node('time') %>% html_text() %>% str_replace_all('\\s+', ' ') %>% str_trim()
  bplace = born %>% html_nodes('a') %>% last() %>% html_text() %>% str_replace_all('\\s+', ' ') %>% str_trim()
  height = webHTML %>% html_nodes('#details-height') %>% html_text() %>%
    str_replace_all('\\s+', '') %>% str_replace('Height:', '')

  url_trademark = sprintf('http://www.imdb.com/name/%s/bio?ref_=nm_dyk_tm_sm#trademark', id)
  content = read_html(url_trademark) %>% html_node('#bio_content')
  ## trade mark is listed under an h4 headline
  h4s = content %>% html_nodes('h4') %>% html_text()
  idx = which(str_detect(h4s, 'Trade Mark'))
  trademark = ''
  if(length(idx) != 0) {
    xpath = sprintf("./div[count(preceding-sibling::h4)=%d]", idx)
    trademark = content %>% html_nodes(xpath = xpath) %>% html_text(trim = TRUE) 
  }

  url_awards = sprintf('http://www.imdb.com/name/%s/awards?ref_=nm_ql_2', id)
  content = read_html(url_awards) %>% html_node('.article.listo')
  ## the oscar table is under an h3 headline
  h3s = content %>% html_nodes('h3') %>% html_text()
  idx = which(str_detect(h3s, '^Academy Awards, USA'))
  oscar_tb = NULL
  if(length(idx) != 0) {
    xpath = sprintf("./table[count(preceding-sibling::h3)=%d]", idx - 1)
    oscar_tb = (content %>% html_nodes(xpath = xpath) %>% html_table())[[1]]
    names(oscar_tb) = c('Year', 'Result', 'Description')
    oscar_tb = oscar_tb %>% mutate(Result = str_detect(Result, 'Won'))
  }
  return(list(bdate = bdate, bplace = bplace, height = height,
              trademark = trademark, oscar = oscar_tb))
}

cores = 24 
registerDoParallel(cores = cores)
dirInfos = foreach(i = 1:ceiling(length(dirIDs)/400)) %do% {
  range = (1 + 400 * (i - 1)):min((400 * i), length(dirIDs))
  foreach(dir = dirIDs[range]) %dopar%
    tryCatch({
      if(is.null(dir)) return(NULL)
      id = dir$id
      getInfo(id)
    }, error = function(e) {print(e); return(NULL)})
} %>% unlist(recursive = FALSE)
names(dirInfos) = names(dirIDs)
saveRDS(dirInfos, file = '../RData/dirInfos.rds')

cores = 24 
registerDoParallel(cores = cores)
actInfos = foreach(i = 1:ceiling(length(actIDs)/400)) %do% {
  range = (1 + 400 * (i - 1)):min((400 * i), length(actIDs))
  actInfos = foreach(act = actIDs[range]) %dopar% 
    tryCatch({
      if(is.null(dir)) return(NULL)
      id = act$id
      getInfo(id)
    }, error = function(e) {print(e); return(NULL)})
} %>% unlist(recursive = FALSE)
names(actInfos) = names(actIDs)
saveRDS(actInfos, file = '../RData/actInfos.rds')


## test
tmp = list()
for(i in sample(1:length(dirIDs), 10)) {
  tmp[[names(dirIDs)[i]]] = getInfo(dirIDs[[i]]$id)
}
