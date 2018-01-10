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
  return(list(oscar = oscar_tb))
}

cores = 24
registerDoParallel(cores = cores)

dirInfos1 = foreach(dir = dirIDs[1:1500]) %dopar%
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = dir$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
dirInfos2 = foreach(dir = dirIDs[1501:length(dirIDs)]) %dopar%
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = dir$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
dirInfos = c(dirInfos1, dirInfos2)
names(dirInfos) = names(dirIDs)
saveRDS(dirInfos, file = '../RData/dirOscar.rds')

actInfos1 = foreach(act = actIDs[1:1500]) %dopar% 
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = act$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
save(actInfos1, file = '../RData/actOscar1.rds')
actInfos2 = foreach(act = actIDs[1501:3000]) %dopar% 
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = act$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
save(actInfos2, file = '../RData/actOscar2.rds')
actInfos3 = foreach(act = actIDs[3001:length(actIDs)]) %dopar% 
  tryCatch({
    if(is.null(dir)) return(NULL)
    id = act$id
    getInfo(id)
  }, error = function(e) {print(e); return(NULL)})
actInfos = c(readRDS('../RData/actOscar1.rds'), readRDS('../RData/actOscar2.rds'), actInfos3)
names(actInfos) = names(actIDs)
saveRDS(actInfos, file = '../RData/actOscar.rds')
