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
      geom_point(size = 0.5, color = '#386cb0') +
      ggtitle('Yearly Average Rating')
  plot_custom(p) %>% ggplotly()
}

box_genre = function(dat) {
  p = ggplot(dat, aes(Genre, Ratings, color = Genre)) + stat_boxplot(geom = "errorbar", width = 0.8) +
    geom_boxplot() + ggtitle('Ratings by Genre') + ylim(c(0, 100))
  (plot_custom(p, legend.pos = 'none') + theme(axis.text.x = element_text(angle = 90, hjust = 1))) %>% ggplotly()
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
load('./movies.all.RData')
directors = movies.all$Director %>% unique()
actors = movies.all$Actors %>% str_split(',') %>% unlist() %>% str_trim() %>% unique()
actors = actors[1:10000]
genres = c('Action', 'Adventure', 'Animation', 'Children', 'Comedy', 'Crime', 'Documentary',
           'Drama', 'Fantasy', 'Film-Noir', 'Horror', 'Musical', 'Mystery', 'Romance', 'Sci-Fi',
           'Thriller', 'War', 'Western', 'IMAX', '(no genres listed)')
source('header.R')
source('sidebar.R')
source('body.R')

