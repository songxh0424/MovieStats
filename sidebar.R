dbSidebar = dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem('Main Page', tabName = 'main', icon = icon('home')),
    menuItem('Statistics and Rankings', tabName = 'stats', icon = icon('bar-chart')),
    menuItem('Directors', tabName = 'director', icon = icon('video-camera')),
    menuItem('Actors', tabName = 'actor', icon = icon('user-secret')),
    tags$head(tags$script(HTML('$(document).ready(function() {$(".treeview-menu").css("display", "block");})')))
  )
)
