################################################################################
## Functions
################################################################################
## plotting and theming functions
theme_Publication <- function(base_size=10, legend.pos = 'bottom') {
  t = (theme_foundation(base_size=base_size)
       + theme(plot.title = element_text(face = "bold",
                                         size = rel(1.2), hjust = 0.5),
               text = element_text(),
               panel.background = element_rect(colour = NA),
               plot.background = element_rect(colour = NA),
               panel.border = element_rect(colour = NA),
               axis.title = element_text(face = "bold",size = rel(1)),
               axis.title.y = element_text(angle=90,vjust =2),
               axis.title.x = element_text(vjust = -0.2),
               axis.text = element_text(), 
               axis.line = element_line(colour="black"),
               axis.ticks = element_line(),
               panel.grid.major = element_line(colour="#f0f0f0"),
               panel.grid.minor = element_blank(),
               legend.key = element_rect(colour = NA),
               legend.position = legend.pos,
               ## legend.direction = "horizontal",
               ## legend.key.size= unit(0.2, "cm"),
               ## legend.margin = unit(0.1, "cm"),
               legend.title = element_text(face="italic"),
               plot.margin=unit(c(10,5,5,5),"mm"),
               strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
               strip.text = element_text(face="italic")
      ))
  return(t)
}

scale_fill_Publication <- function(...){
      library(scales)
      discrete_scale("fill","Publication",manual_pal(values = rep(c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF"), 2)), ...)
}

scale_colour_Publication <- function(...){
      library(scales)
      discrete_scale("colour","Publication",manual_pal(values = rep(c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33", "#FFA500", "#3cb371", "#1E90FF"), 2)), ...)
}

plot_custom <- function(p, saveTo = NULL, base_size=10, legend.pos = "right") {
  ## out = p + theme_Publication(base_size, legend.pos) + scale_fill_Publication() + scale_colour_Publication()
  out = p + theme_Publication(base_size, legend.pos) + scale_fill_tableau(palette = 'tableau20') +
    scale_colour_tableau(palette = 'tableau20')
  if(is.null(saveTo)) return(out)
  ggsave(saveTo, out)
  return(out)
}

################################################################################
## First Page
################################################################################
## yearly trend
year_trend = function(dat) {
  p = ggplot(dat, aes(Year, Ratings)) + geom_line(color = '#386cb0') +
      geom_point(size = 0.5, color = '#386cb0') 
      ## ggtitle('Yearly Average Rating')
  plot_custom(p) %>% ggplotly()
}
## boxplot of genres
box_genre = function(dat) {
  p = ggplot(dat, aes(Genre, Ratings, color = Genre)) + stat_boxplot(geom = "errorbar", width = 0.8) +
    geom_boxplot() + ylim(c(0, 100))
    ## ggtitle('Ratings by Genre')  
  (plot_custom(p, legend.pos = 'none') + theme(axis.text.x = element_text(angle = 90, hjust = 1))) %>% ggplotly()
}
## scatter of box office
bo_scat_yr = function(dat) {
  p = ggplot(dat, aes(Year, BoxOffice, text = Title)) + geom_jitter(alpha = 0.5, color = 'slategrey') +
    ylab('Box Office')
  plot_custom(p, legend.pos = 'none') %>% ggplotly()
}
bo_scat_rating = function(dat, Source = c('imdb', 'meta', 'rt')) {
  sour = match.arg(Source, c('imdb', 'meta', 'rt'))
  tmp = switch(sour,
               'imdb' = dat %>% rename(Ratings = `IMDb Rating`),
               'meta' = dat %>% rename(Ratings = Metascore),
               'rt' = dat %>% rename(Ratings = Tomatometer)
               )
  p = ggplot(tmp, aes(Ratings, BoxOffice, text = Title)) + geom_jitter(color = 'slategrey', alpha = 0.5) +
    ylab('Box Office')
  plot_custom(p, legend.pos = 'none') %>% ggplotly()
}

## data tables for rankings
ranking = function(dat, type = c('dir', 'act', 'duo'), thres, Source = c('IMDb Rating', 'Metascore', 'Tomatometer')) {
  sour = match.arg(Source, c('IMDb Rating', 'Metascore', 'Tomatometer'))
  tmp = switch(sour,
               'IMDb Rating' = dat %>% rename(Ratings = `IMDb Rating`),
               'Metascore' = dat %>% select(-Movies) %>% rename(Ratings = Metascore, Movies = n_meta),
               'Tomatometer' = dat %>% select(-Movies) %>% rename(Ratings = Tomatometer, Movies = n_tomato)
               )
  tp = match.arg(type, c('dir', 'act', 'duo'))
  tmp = tmp %>% filter(Movies >= thres) %>% arrange(desc(Ratings))
  tmp = switch(tp,
               'dir' = tmp %>% select(Director, Movies, Ratings),
               'act' = tmp %>% select(Actor, Movies, Ratings),
               'duo' = tmp %>% select(Director, Actor, Movies, Ratings) %>% filter(Director != Actor)
               )
  datatable(tmp[1:100, ], class = 'cell-border stripe', options = list(pageLength = 10), filter = "top")
}

################################################################################
## Director/Actor pages
################################################################################
## data tables for actor/director summary statistics
dt_sumry = function(dat) {
  tmp = dat %>% gather(key = Source, value = Ratings, `IMDb Rating`:Tomatometer) %>%
    group_by(Source) %>%
    summarise(Mean = mean(Ratings, na.rm = T), Lowest = min(Ratings, na.rm = T), `Q1` = quantile(Ratings, 0.25, na.rm = T),
              Median = median(Ratings, na.rm = T), `Q3` = quantile(Ratings, 0.75, na.rm = T), Highest = max(Ratings, na.rm = T)) %>%
    mutate_if(is.numeric, funs(format(round(., digits = 2), nsmall = 2)))
  tmp %>% datatable(rownames = FALSE, class = 'cell-border stripe', options = list(dom = 't', ordering = F))
}
## data tables for actor/director movies
dt_movies = function(dat) {
  tmp = dat %>% mutate(`Box Office` = BoxOffice) %>%
    select(Title, Year, `IMDb Rating`:Tomatometer, `Box Office`) %>% 
    rename(IMDb = `IMDb Rating`, Tomato = Tomatometer) %>%
    replace_na(list(`Box Office` = 0)) %>% 
    mutate(`Box Office` = paste0('$', prettyNum(`Box Office`, big.mark = ',', trim = T))) %>%
    arrange(desc(Year)) %>% mutate(Title = sprintf('%s (%s)', Title, Year))
  datatable(tmp[, -c(1, 3)], rownames = FALSE, class = 'cell-border stripe', filter = "top", 
            options = list(pageLength = 10))
}
## formattable for most frequent collaborators
ft_most_collab = function(dat, person, collab_type = c('dir', 'act')) {
  type = match.arg(collab_type, c('dir', 'act'))
  tmp = switch(
    type,
    'act' = dat %>% filter(Actor != person) %>% group_by(Actor) %>% mutate(Movies = n()) %>%
      ungroup() %>% arrange(desc(Movies)) %>% filter(Actor == first(Actor)) %>% rename(Collab = Actor),
    'dir' = dat %>% filter(Director != person) %>% group_by(Director) %>% mutate(Movies = n()) %>%
      ungroup() %>% arrange(desc(Movies)) %>% filter(Director == first(Director)) %>% rename(Collab = Director),
  ) %>% mutate(`Box Office` = BoxOffice) %>% replace_na(list(`Box Office` = 0)) %>%
    mutate(`Box Office` = paste0('$', prettyNum(`Box Office`, big.mark = ',', trim = T))) %>%
    mutate(Title = str_sub(Title, end = 25) %>% paste0(ifelse(str_length(Title) > 25, '...', ''))) %>%
    mutate(Title = sprintf('%s(%s)', Title, Year)) %>% arrange(desc(Year)) %>%
    select(Collab, Title, `IMDb Rating`:Tomatometer, `Box Office`)
  collab = tmp$Collab[1]
  if(all(is.na(tmp$Metascore))) {
    ft = formattable(tmp[, -1], list(area(col = c(`IMDb Rating`, Tomatometer)) ~ normalize_bar('pink', 0.2)))
  } else {
    ft = formattable(tmp[, -1], list(
      Metascore = formatter('span', style = x ~ style(color = ifelse(is.na(x), 'grey', ifelse(x > 60, 'green', ifelse(x > 39, '#fdb462', 'red'))))),
      area(col = c(`IMDb Rating`, Tomatometer)) ~ normalize_bar('pink', 0.2)
    ))
  }
  return(list(collab = collab, ft = ft))
}

## plots for actor/director insights
## bar chart of ratings
bar_ratings = function(dat, Source = c('IMDb Rating', 'Metascore', 'Tomatometer')) {
  sour = match.arg(Source, c('IMDb Rating', 'Metascore', 'Tomatometer'))
  tmp = switch(sour,
               'IMDb Rating' = dat %>% rename(Ratings = `IMDb Rating`),
               'Metascore' = dat %>% rename(Ratings = Metascore),
               'Tomatometer' = dat %>% rename(Ratings = Tomatometer)
               )
  tmp = tmp %>% arrange(Ratings) %>% filter(!is.na(Ratings)) %>%
    mutate(Title = str_sub(Title, end = 25) %>% paste0(ifelse(str_length(Title) > 25, '...', '')))
  tmp$Title = factor(tmp$Title %>% as.character(), levels = tmp$Title)
  p = tmp %>% ggplot(aes(Title, Ratings)) + geom_col(fill = "#fdb462", width = 0.7, alpha = 0.8) +
    coord_flip() + ylab(sour)
  plot_custom(p) %>% ggplotly(height = 400) %>% layout(margin = list(t = 20))
}
## time line of films
timeline = function(dat, Source = c('IMDb Rating', 'Metascore', 'Tomatometer')) {
  sour = match.arg(Source, c('IMDb Rating', 'Metascore', 'Tomatometer'))
  tmp = switch(sour,
               'IMDb Rating' = dat %>% rename(Ratings = `IMDb Rating`),
               'Metascore' = dat %>% rename(Ratings = Metascore),
               'Tomatometer' = dat %>% rename(Ratings = Tomatometer)
               )
  tmp = tmp %>% arrange(Ratings) %>% filter(!is.na(Ratings))
  p = tmp %>% ggplot(aes(Year, Ratings, text = Title)) + geom_point(col = 'slategrey', alpha = 0.8) + ylab(sour)
  plot_custom(p) %>% ggplotly(height = 400) %>% layout(margin = list(t = 20))
}
## lolipop chart of movie genres
loli_genres = function(dat) {
  tmp = dat %>% group_by(Genre) %>% summarise(Movies = n(), `IMDb Rating` = mean(`IMDb Rating`, na.rm = T)) %>%
    arrange(Movies) %>% mutate(Genre = factor(Genre, levels = Genre)) %>% filter(Genre != 'Drama')
  p = tmp %>% ggplot(aes(Genre, Movies, color = Genre)) +
    geom_segment(aes(y = 0, x = Genre, yend = Movies, xend = Genre), color = 'grey78') +
    geom_point(aes(size = `IMDb Rating`)) + coord_flip() + scale_size_continuous(limits = c(50, 92))
  plot_custom(p, legend.pos = 'none') %>% ggplotly(height = 400) %>% layout(margin = list(t = 20))
}

################################################################################
## Fun Facts page
################################################################################
## oscar lucky/unlucky tables
oscar_unlucky = function(dat, n = 20) {
  tb = dat %>% filter(Won == 0) %>% head(n) %>% select(-`Won Years`, -Won)
  datatable(tb, class = 'cell-border stripe', options = list(dom = 'tp', ordering = F, pageLength = 5))
}
oscar_lucky = function(dat, n = 20) {
  tb = dat %>% arrange(desc(Won), desc(Nominated)) %>% mutate(`Win Rate` = round(Won / Nominated * 100, 2)) %>%
    select(-`Nominated Years`)
  datatable(tb[1:n, ], class = 'cell-border stripe', options = list(dom = 'tp', pageLength = 5))
}
## polarizing movies
ft_polar = function(dat, n = 50) {
  top_polar = dat %>% group_by(imdbID) %>% filter(row_number() == 1) %>% head(n) %>% select(-c(imdbID, Genre))
  formatter = list()
  if('Metascore' %in% names(top_polar))
    formatter$Metascore = formatter('span', style = x ~ style(color = ifelse(is.na(x), 'grey', ifelse(x > 60, 'green', ifelse(x > 39, '#fdb462', 'red')))))
  if(('IMDb Rating' %in% names(top_polar)) & ('Tomatometer' %in% names(top_polar)))
    formatter = c(formatter, area(col = c(`IMDb Rating`, Tomatometer)) ~ normalize_bar('pink', 0.2))
  if(('IMDb Rating' %in% names(top_polar)) & !('Tomatometer' %in% names(top_polar)))
    formatter = c(formatter, area(col = c(`IMDb Rating`)) ~ normalize_bar('pink', 0.2))
  if(!('IMDb Rating' %in% names(top_polar)) & ('Tomatometer' %in% names(top_polar)))
    formatter = c(formatter, area(col = c(Tomatometer)) ~ normalize_bar('pink', 0.2))
  formattable(top_polar[, -1], formatter)
}
dt_polar = function(dat, n = 50) {
  top_polar = dat %>% group_by(imdbID) %>% filter(row_number() == 1) %>% head(n) %>% select(-c(imdbID, Genre))
  datatable(top_polar[, -1], class = 'cell-border stripe', options = list(pageLength = 10, dom = 'tp'))
}
bar_polar = function(dat, movies_all) {
  totalCount = movies_all %>% group_by(Genre) %>% summarise(Total = n())
  tmp = dat %>% group_by(Genre) %>% summarise(Count = n()) %>% inner_join(totalCount, by = 'Genre') %>%
    arrange(desc(Count)) 
  tmp = bind_rows(tmp[1:10, ], tmp[-(1:10), ] %>% summarise(Count = sum(Count), Total = sum(Total)) %>% mutate(Genre = 'Others')) %>%
    mutate(Genre = factor(Genre, levels = rev(Genre)))
  p = ggplot(tmp, aes(Genre, Count, fill = Genre)) + geom_col(width = 0.8) + coord_flip() +
    geom_label(aes(label = Count, fill = Genre), color = 'white', fontface = 'bold', vjust = 0.3, angle = 270, size = 3) + 
    geom_label(aes(Genre, rep(0, nrow(tmp)), label = paste0(round(Count / Total * 100, digits = 1), '%')),
               color = 'white', fontface = 'bold', vjust = 0.3, size = 3) +
    ylab('# of movies with rating differences over 30')
  plot_custom(p, legend.pos = 'none')
}

################################################################################
## Global variables
################################################################################
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(plotly)
library(stringr)
library(DT)
library(markdown)
library(lubridate)
library(formattable)

movies.all = readRDS('./RData/movies.all.rds')
dirs = readRDS('./RData/dirs.rds')
acts = readRDS('./RData/acts.rds')
dirIDs = readRDS('./RData/dirIDs.rds')
actIDs = readRDS('./RData/actIDs.rds')
dirInfos = readRDS('./RData/dirInfos.rds')
actInfos = readRDS('./RData/actInfos.rds')
dirTM = readRDS('./RData/dirTM.rds')
actTM = readRDS('./RData/actTM.rds')
dirOscar = readRDS('./RData/dirOscar.rds')
actOscar = readRDS('./RData/actOscar.rds')
dirOscar_tb = readRDS('./RData/dirOscar_tb.rds')
actOscar_tb = readRDS('./RData/actOscar_tb.rds')

genres = c('Action', 'Adventure', 'Animation', 'Children', 'Comedy', 'Crime', 'Documentary',
           'Drama', 'Fantasy', 'Film-Noir', 'Horror', 'Musical', 'Mystery', 'Romance', 'Sci-Fi',
           'Thriller', 'War', 'Western', 'IMAX', '(no genres listed)')
b = tags$b
br = tags$br
bq = tags$blockquote

source('header.R')
source('sidebar.R')
source('body.R')

