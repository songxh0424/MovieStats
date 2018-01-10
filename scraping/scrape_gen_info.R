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
  return(list(bdate = bdate, bplace = bplace, height = height))
}

cores = 24
registerDoParallel(cores = cores)

dirInfos1 = foreach(dir = dirIDs[1:2000]) %dopar%
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = dir$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
dirInfos2 = foreach(dir = dirIDs[2001:length(dirIDs)]) %dopar%
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = dir$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
dirInfos = c(dirInfos1, dirInfos2)
names(dirInfos) = names(dirIDs)
saveRDS(dirInfos, file = '../RData/dirInfos.rds')

actInfos1 = foreach(act = actIDs[1:2000]) %dopar% 
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = act$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
actInfos2 = foreach(act = actIDs[2001:4000]) %dopar% 
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = act$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
actInfos3 = foreach(act = actIDs[4001:length(actIDs)]) %dopar% 
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = act$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
actInfos = c(actInfos1, actInfos2, actInfos3)
names(actInfos) = names(actIDs)
saveRDS(actInfos, file = '../RData/actInfos.rds')


## test
## tmp = list()
## for(i in sample(1:length(dirIDs), 10)) {
##   tmp[[names(dirIDs)[i]]] = getInfo(dirIDs[[i]]$id)
## }
