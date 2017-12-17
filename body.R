genres = c('Action', 'Adventure', 'Animation', "Children's", 'Comedy', 'Crime', 'Documentary',
           'Drama', 'Fantasy', 'Film-Noir', 'Horror', 'Musical', 'Mystery', 'Romance', 'Sci-Fi',
           'Thriller', 'War', 'Western')

dbBody = dashboardBody(
  fluidRow(
    column(
      width = 12, offset = 0,
      tabItems(
        ## Main page body 
        tabItem(
          tabName = 'main',
          fluidRow(
            column(width = 10, offset = 1, align = 'center', h2('Overview')),
            column(width = 10, offset = 1, p('Write something. '))
          )
        ),
        ## Stats page body
        tabItem(
          tabName = 'stats',
          fluidRow(
            column(width = 10, offset = 1, align = 'center', h2('Statistics and Rankings')),
            column(
              width = 3,
              box(title = 'Year released', status = 'primary', solidHeader = T, width = NULL,
                  sliderInput(inputId = 'year', label = NULL, min = 1900, max = 2017,
                              value = c(1970, 2017))
                  ),
              box(title = 'Minimum reviews', status = 'primary', solidHeader = T, width = NULL,
                  sliderInput(inputId = 'minReview', label = NULL, min = 0, max = 300,
                              value = c(0, 300))
                  ),
              box(title = 'First n actors in credits', status = 'primary', solidHeader = T, width = NULL,
                  sliderInput(inputId = 'first_n_actors', label = NULL, min = 1, max = 20,
                              value = 10)
                  ),
              box(title = 'Genres', status = 'primary', solidHeader = T, width = NULL,
                  checkboxGroupInput(inputId = 'genre', label = NULL,
                                     choices = genres, selected = genres)
                  )
            )
          )
        )
      )
    )
  )
)
