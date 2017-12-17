library(shiny)
library(shinydashboard)
source('sidebar.R')
source('body.R')

fluidPage(
  fluidRow(
    dashboardPage(
      skin = 'purple',
      header = dashboardHeader(title = 'MovieStats', titleWidth = 300),
      sidebar = dbSidebar,
      body = dbBody
    )
  )
)
