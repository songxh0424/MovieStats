dbSidebar = dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem('Main Page', tabName = 'main', icon = icon('home')),
    menuItem('Statistics and Rankings', tabName = 'stats', icon = icon('bar-chart')),
    menuItem('Directors', tabName = 'director', icon = icon('video-camera')),
    selectizeInput(inputId = 'search_director', label = NULL, choices = directors, selected = 'Quentin Tarantino',
                   options = list(maxOptions = 5, placeholder = 'Input a director\' name')),
    menuItem('Actors', tabName = 'actor', icon = icon('user-secret')),
    ## too many actors seem to slow down the page significantly, maybe include only most popular ones
    selectizeInput(inputId = 'search_actor', label = NULL, choices = actors, selected = 'Uma Thurman',
                   options = list(maxOptions = 5, placeholder = 'Input an actor\' name')),
    tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})')))
  )
)
