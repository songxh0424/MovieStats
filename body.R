dbBody = dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  ## tags$head(tags$style(HTML(".small-box {height: 95px}"))),
  fluidRow(
    column(
      width = 12, offset = 0,
      tabItems(
        ## Main page body 
        tabItem(
          tabName = 'main',
          fluidRow(
            ## column(width = 10, offset = 1, align = 'center', h1('Overview')),
            column(width = 10, offset = 1, includeMarkdown('./README.md'))
          )
        ),
        ## Stats page body
        tabItem(
          tabName = 'stats',
          fluidRow(
            ## title
            column(width = 10, offset = 1, align = 'center', h1('Statistics and Rankings'), tags$br()),
            ## filtering criteria
            column(
              width = 3,
              box(title = 'Year released', status = 'primary', solidHeader = T, width = NULL,
                  sliderInput(inputId = 'year', label = NULL, min = 1900, max = 2017,
                              value = c(1970, 2017))
                  ),
              ## need review count data for this filer
              ## box(title = 'Minimum reviews', status = 'primary', solidHeader = T, width = NULL,
              ##     sliderInput(inputId = 'minReview', label = NULL, min = 0, max = 300,
              ##                 value = c(0, 300))
              ##     ),
              box(title = 'Minimum number of movies - director', status = 'primary', solidHeader = T, width = NULL,
                  sliderInput(inputId = 'min_movies_dir', label = NULL, min = 1, max = 15, value = 8)
                  ),
              box(title = 'Minimum number of movies - actor', status = 'primary', solidHeader = T, width = NULL,
                  sliderInput(inputId = 'min_movies_act', label = NULL, min = 1, max = 20, value = 10)
                  ),
              ## need to scrape more actors from IMDb for this filter, the OMDb API only have the top 4 actors
              ## box(title = 'First n actors in credits', status = 'primary', solidHeader = T, width = NULL,
              ##     sliderInput(inputId = 'first_n_actors', label = NULL, min = 1, max = 20, value = 10)
              ##     ),
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
                      box(
                        title = 'Yearly Average Rating', status = 'success', width = NULL,
                        solidHeader = TRUE, collapsible = TRUE,
                        tabBox(
                          title = NULL, width = NULL,
                          tabPanel(title = 'IMDb Rating', plotlyOutput(outputId = 'trend_imdb')),
                          tabPanel(title = 'Metascore', plotlyOutput(outputId = 'trend_meta')),
                          tabPanel(title = 'Tomatometer', plotlyOutput(outputId = 'trend_rt'))
                        )
                      ),
                      ## boxplot by genre
                      box(
                        title = 'Distribution of Ratings by Genre', status = 'success', width = NULL,
                        solidHeader = TRUE, collapsible = TRUE,
                        tabBox(
                          title = NULL, width = NULL,
                          tabPanel(title = 'IMDb Rating', plotlyOutput(outputId = 'box_imdb', height = 500)),
                          tabPanel(title = 'Metascore', plotlyOutput(outputId = 'box_meta', height = 500)),
                          tabPanel(title = 'Tomatometer', plotlyOutput(outputId = 'box_rt', height = 500))
                        )
                      ),
                      ## scatter plot of box office over time/rating
                      box(
                        title = 'Box Office', status = 'success', width = NULL,
                        solidHeader = TRUE, collapsible = TRUE,
                        tabBox(
                          title = NULL, width = NULL,
                          tabPanel(title = 'By Year', plotlyOutput(outputId = 'bo_year', height = 600)),
                          tabPanel(title = 'IMDb Rating', plotlyOutput(outputId = 'bo_imdb', height = 600)),
                          tabPanel(title = 'Metascore', plotlyOutput(outputId = 'bo_meta', height = 600)),
                          tabPanel(title = 'Tomatometer', plotlyOutput(outputId = 'bo_rt', height = 600))
                        )
                      )
                    )
                  )
                ),
                ## rankings
                tabPanel(
                  title = 'Director/Actor Rankings',
                  fluidRow(
                    column(
                      width = 12, offset = 0, align = 'center',
                      box(
                        title = 'Top Directors by Average Rating', status = 'success', width = NULL,
                        solidHeader = TRUE, collapsible = TRUE,
                        tabBox(
                          title = NULL, width = NULL,
                          tabPanel(title = 'IMDb Rating', dataTableOutput(outputId = 'top_dir_imdb')),
                          tabPanel(title = 'Metascore', dataTableOutput(outputId = 'top_dir_meta')),
                          tabPanel(title = 'Tomatometer', dataTableOutput(outputId = 'top_dir_rt'))
                        )
                      ),
                      box(
                        title = 'Top Actors by Average Rating', status = 'success', width = NULL,
                        solidHeader = TRUE, collapsible = TRUE,
                        tabBox(
                          title = NULL, width = NULL,
                          tabPanel(title = 'IMDb Rating', dataTableOutput(outputId = 'top_act_imdb')),
                          tabPanel(title = 'Metascore', dataTableOutput(outputId = 'top_act_meta')),
                          tabPanel(title = 'Tomatometer', dataTableOutput(outputId = 'top_act_rt'))
                        )
                      ),
                      box(
                        title = 'Top Director-Actor Duos by Average Rating', status = 'success', width = NULL,
                        solidHeader = TRUE, collapsible = TRUE,
                        tabBox(
                          title = NULL, width = NULL,
                          tabPanel(title = 'IMDb Rating', dataTableOutput(outputId = 'top_duo_imdb')),
                          tabPanel(title = 'Metascore', dataTableOutput(outputId = 'top_duo_meta')),
                          tabPanel(title = 'Tomatometer', dataTableOutput(outputId = 'top_duo_rt'))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        ## Director insights page
        tabItem(
          tabName = 'director',
          fluidRow(
            column(width = 10, offset = 1, align = 'center', h1('Director Insights'), tags$br()),
            box(
              title = 'General Information', status = 'success', width = 6, height = 390, 
              collapsible = FALSE, solidHeader = TRUE,
              uiOutput(outputId = 'gen_info')
            ),
            box(
              title = 'Career Highlights', status = 'success', width = 6, height = 390,
              collapsible = FALSE, solidHeader = TRUE,
              uiOutput(outputId = 'carir_hlt')
            )
          ),
          fluidRow(
            box(
              title = 'Statistics', status = 'success', width = 12,
              collapsible = TRUE, solidHeader = TRUE,
              tabBox(
                title = NULL, width = NULL,
                tabPanel(
                  title = 'IMDb Rating', plotlyOutput('top_bottom_dir_imdb', height = 'auto'), plotlyOutput('timeline_dir_imdb', height = 'auto'),
                  plotlyOutput('genre_dir_imdb')
                ),
                tabPanel(
                  title = 'Metascore', plotlyOutput('top_bottom_dir_meta', height = 'auto'), plotlyOutput('timeline_dir_meta', height = 'auto'),
                  plotlyOutput('genre_dir_meta')
                ),
                tabPanel(
                  title = 'Tomatometer', plotlyOutput('top_bottom_dir_rt', height = 'auto'), plotlyOutput('timeline_dir_rt', height = 'auto'),
                  plotlyOutput('genre_dir_rt')
                )
              )
            )
          )
        ),
        ## Actor insights page
        tabItem(
          tabName = 'actor',
          fluidRow(
            column(width = 10, offset = 1, align = 'center', h1('Actor Insights'), tags$br()),
            column(
              width = 10, offset = 1, align = 'center',
              box(title = 'General Information', status = 'success', width = NULL,
                  collapsible = TRUE, solidHeader = TRUE,
                  column(width = 3, uiOutput(outputId = 'img_act')),
                  column(width = 5, uiOutput(outputId = 'act_info')),
                  column(width = 4, valueBoxOutput(outputId = 'oscar_a', width = NULL),
                         valueBoxOutput(outputId = 'golden_globe_a', width = NULL))
                  ),
              box(title = 'Statistics', status = 'success', width = NULL,
                  collapsible = TRUE, solidHeader = TRUE,
                  tabBox(
                    title = NULL, width = NULL,
                    tabPanel(
                      title = 'IMDb Rating', plotlyOutput('top_bottom_act_imdb', height = 'auto'), plotlyOutput('timeline_act_imdb', height = 'auto'),
                      plotlyOutput('genre_act_imdb')
                    ),
                    tabPanel(
                      title = 'Metascore', plotlyOutput('top_bottom_act_meta', height = 'auto'), plotlyOutput('timeline_act_meta', height = 'auto'),
                      plotlyOutput('genre_act_meta')
                    ),
                    tabPanel(
                      title = 'Tomatometer', plotlyOutput('top_bottom_act_rt', height = 'auto'), plotlyOutput('timeline_act_rt', height = 'auto'),
                      plotlyOutput('genre_act_rt')
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
