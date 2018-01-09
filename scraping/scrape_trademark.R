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

getInfo = function(id) {
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
  return(list(trademark = trademark))
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
dirInfos = c(dirInfo1, dirInfos2)
names(dirInfos) = names(dirIDs)
saveRDS(dirInfos, file = '../RData/dirTM.rds')

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
saveRDS(actInfos, file = '../RData/actTM.rds')
