library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(plotly)
source('header.R')
source('sidebar.R')
source('body.R')
source('functions.R')
load('./movies.all.RData')

fluidPage(
  fluidRow(
    dashboardPage(
      skin = 'purple',
      header = dbHeader,
      sidebar = dbSidebar,
      body = dbBody
    )
  )
)
