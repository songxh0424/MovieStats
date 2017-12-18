load('./movies.all.RData')
ggdat.ratings = movies.all %>%
  gather(key = Source, value = Ratings, Metascore, `IMDb Rating`, Tomatometer)

function(input, output, session) {
  ## update the genre selection on stats page
  observeEvent(input$checkAll, {
    updateCheckboxGroupInput(session, label = NULL, inputId = 'genre',
                             choices = genres, selected = genres)
    }
  )
  observeEvent(input$uncheckAll, {
    updateCheckboxGroupInput(session, label = NULL, inputId = 'genre',
                             choices = genres, selected = NULL)
    }
  )

  ## plots on stats page
  ggdat.ratings.react = reactive({
    ggdat.ratings %>% filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Genre %in% input$genre)
  })
  ## yearly trends plots
  ## need to figure out how to put ggdat.trend outside the render functions
  output$trend_imdb = renderPlotly({
    ggdat.trend = ggdat.ratings.react() %>% group_by(Source, Year) %>%
      summarise(Ratings = mean(Ratings, na.rm = TRUE))
    year_trend(ggdat.trend %>% filter(Source == 'IMDb Rating'))
  })
  output$trend_meta = renderPlotly({
    ggdat.trend = ggdat.ratings.react() %>% group_by(Source, Year) %>%
      summarise(Ratings = mean(Ratings, na.rm = TRUE))
    year_trend(ggdat.trend %>% filter(Source == 'Metascore'))
  })
  output$trend_rt = renderPlotly({
    ggdat.trend = ggdat.ratings.react() %>% group_by(Source, Year) %>%
      summarise(Ratings = mean(Ratings, na.rm = TRUE))
    year_trend(ggdat.trend %>% filter(Source == 'Tomatometer'))
  })
  ## boxplot of ratings by genre
  output$box_imdb = renderPlotly({
    box_genre(ggdat.ratings.react() %>% filter(Source == 'IMDb Rating'))
  })
  output$box_meta = renderPlotly({
    box_genre(ggdat.ratings.react() %>% filter(Source == 'Metascore'))
  })
  output$box_rt = renderPlotly({
    box_genre(ggdat.ratings.react() %>% filter(Source == 'Tomatometer'))
  })
}
