################################################################################
## Functions
################################################################################
## plotting and theming functions
theme_Publication <- function(base_size=10, legend.pos = "bottom") {
      (theme_foundation(base_size=base_size)
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
  out = p + theme_Publication(base_size, legend.pos) + scale_fill_Publication() + scale_colour_Publication()
  if(is.null(saveTo)) return(out)
  ggsave(saveTo, out)
  return(out)
}

## yearly trend
year_trend = function(dat) {
  p = ggplot(dat, aes(Year, Ratings)) + geom_line(color = '#386cb0') +
      geom_point(size = 0.5, color = '#386cb0') 
      ## ggtitle('Yearly Average Rating')
  plot_custom(p) %>% ggplotly()
}

box_genre = function(dat) {
  p = ggplot(dat, aes(Genre, Ratings, color = Genre)) + stat_boxplot(geom = "errorbar", width = 0.8) +
    geom_boxplot() + ylim(c(0, 100))
    ## ggtitle('Ratings by Genre')  
  (plot_custom(p, legend.pos = 'none') + theme(axis.text.x = element_text(angle = 90, hjust = 1))) %>% ggplotly()
}

## data tables for rankings
ranking = function(dat, type = c('dir', 'act', 'duo'), thres) {
  tmp = match.arg(type, c('dir', 'act', 'duo'))
  dat = dat %>% filter(Movies >= thres) %>% arrange(desc(Ratings))
  dat = switch(tmp,
               'dir' = dat %>% select(Director, Movies, Ratings),
               'act' = dat %>% select(Actor, Movies, Ratings),
               'duo' = dat %>% select(Director, Actor, Movies, Ratings)
               )
  datatable(dat %>% filter(row_number() <= 100),
            class = 'cell-border stripe', options = list(pageLength = 10), filter = "top")
}

## plots for actor/director insights
## bar chart of ratings
bar_ratings = function(dat, Source = c('imdb', 'meta', 'rt')) {
  sour = match.arg(Source, c('imdb', 'meta', 'rt'))
  tmp = switch(sour,
               'imdb' = dat %>% rename(Ratings = `IMDb Rating`),
               'meta' = dat %>% rename(Ratings = Metascore),
               'rt' = dat %>% rename(Ratings = Tomatometer)
               )
  tmp = tmp %>% arrange(Ratings) %>% filter(!is.na(Ratings))
  tmp$Title = factor(tmp$Title %>% as.character(), levels = tmp$Title)
  p = tmp %>%
    ggplot(aes(Title, Ratings)) + geom_col(fill = "#fdb462", width = 0.7) +
    coord_flip() + ggtitle('All Movie Ratings')
  plot_custom(p) %>% ggplotly(height = 550)
}
## time line of films
timeline = function(dat, Source = c('imdb', 'meta', 'rt')) {
  sour = match.arg(Source, c('imdb', 'meta', 'rt'))
  tmp = switch(sour,
               'imdb' = dat %>% rename(Ratings = `IMDb Rating`),
               'meta' = dat %>% rename(Ratings = Metascore),
               'rt' = dat %>% rename(Ratings = Tomatometer)
               )
  tmp = tmp %>% arrange(Ratings) %>% filter(!is.na(Ratings))
  p = tmp %>% ggplot(aes(Year, Ratings, text = Title)) + geom_point(col = '#386cb0', alpha = 0.7) +
    ggtitle('Timeline of Movies')
  plot_custom(p) %>% ggplotly(width = 650, height = 450)
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
load('./movies.all.RData')
load('./actorIDs.RData')
load('./directorIDs.RData')
load('./dirInfos.RData')
dirs = directors$Director %>% unique()
acts = actors$Actor %>% unique()
acts = acts[1:10000]
genres = c('Action', 'Adventure', 'Animation', 'Children', 'Comedy', 'Crime', 'Documentary',
           'Drama', 'Fantasy', 'Film-Noir', 'Horror', 'Musical', 'Mystery', 'Romance', 'Sci-Fi',
           'Thriller', 'War', 'Western', 'IMAX', '(no genres listed)')
source('header.R')
source('sidebar.R')
source('body.R')

