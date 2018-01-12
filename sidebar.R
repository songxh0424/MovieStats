dbSidebar = dashboardSidebar(
  width = 220,
  sidebarMenu(
    menuItem('Statistics and Rankings', tabName = 'stats', icon = icon('bar-chart')),
    menuItem('Directors', tabName = 'director', icon = icon('video-camera')),
    selectizeInput(inputId = 'search_director', label = NULL, choices = unique(dirs$Director), selected = 'Martin Scorsese',
                   options = list(maxOptions = 5, placeholder = 'Input a director\'s name')),
    menuItem('Actors', tabName = 'actor', icon = icon('user-secret')),
    ## too many actors seem to slow down the page significantly, maybe include only most popular ones
    selectizeInput(inputId = 'search_actor', label = NULL, choices = unique(acts$Actor), selected = 'Leonardo DiCaprio',
                   options = list(maxOptions = 5, placeholder = 'Input an actor\'s name')),
    menuItem('Fun Facts', tabName = 'fun', icon = icon('commenting')),
    hr(), menuItem('About', tabName = 'about', icon = icon('info-circle')),
    tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})')))
  )
)
