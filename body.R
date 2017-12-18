genres = c('Action', 'Adventure', 'Animation', 'Children', 'Comedy', 'Crime', 'Documentary',
           'Drama', 'Fantasy', 'Film-Noir', 'Horror', 'Musical', 'Mystery', 'Romance', 'Sci-Fi',
           'Thriller', 'War', 'Western', 'IMAX', '(no genres listed)')

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
            ## title
            column(width = 10, offset = 1, align = 'center', h2('Statistics and Rankings'), tags$br()),
            ## filtering criteria
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
                                     choices = genres, selected = genres),
                  actionButton(inputId = 'checkAll', label = 'Check all'),
                  actionButton(inputId = 'uncheckAll', label = 'Uncheck all')
                  )
            ),
            ## results area
            column(
              width = 9, align = 'center',
              tabBox(
                title = 'Visualizations and Rankings', width = NULL,
                ## plotting area
                tabPanel(
                  title = 'Visualizations',
                  fluidRow(
                    column(
                      width = 12, offset = 0, align = 'center',
                      ## yearly trend
                      box(title = 'Yearly Average Rating', status = 'success', width = NULL, solidHeader = T,
                        tabBox(
                          title = NULL, width = NULL,
                          tabPanel(
                            title = 'IMDb Rating',
                            plotlyOutput(outputId = 'trend_imdb')
                          ),
                          tabPanel(
                            title = 'Metascore',
                            plotlyOutput(outputId = 'trend_meta')
                          ),
                          tabPanel(
                            title = 'Tomatometer',
                            plotlyOutput(outputId = 'trend_rt')
                          )
                        )
                      ),
                      ## boxplot by genre
                      box(title = 'Distribution of Ratings by Genre', status = 'success', width = NULL, solidHeader = T,
                        tabBox(
                          title = NULL, width = NULL,
                          tabPanel(
                            title = 'IMDb Rating',
                            plotlyOutput(outputId = 'box_imdb', height = 500)
                          ),
                          tabPanel(
                            title = 'Metascore',
                            plotlyOutput(outputId = 'box_meta', height = 500)
                          ),
                          tabPanel(
                            title = 'Tomatometer',
                            plotlyOutput(outputId = 'box_rt', height = 500)
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)
