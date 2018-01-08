## wide to long is not necessary given the current layout
## ggdat.ratings = movies.all %>%
##   gather(key = Source, value = Ratings, Metascore, `IMDb Rating`, Tomatometer)

function(input, output, session) {
################################################################################
  ## stats page
################################################################################
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
  ggdat.ratings = reactive({
    movies.all %>% filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Genre %in% input$genre)
  })
  ggdat.trend = reactive({
    movies.all %>% filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Genre %in% input$genre) %>% group_by(imdbID) %>%
      filter(row_number() == 1) %>% group_by(Year) %>%
      summarise(`IMDb Rating` = mean(`IMDb Rating`, na.rm = TRUE),
                Metascore = mean(Metascore, na.rm = T), Tomatometer = mean(Tomatometer, na.rm = T))
  })
  ## yearly trends plots
  ## need to figure out how to put ggdat.trend outside the render functions
  output$trend_imdb = renderPlotly({
    year_trend(ggdat.trend() %>% rename(Ratings = `IMDb Rating`))
  })
  output$trend_meta = renderPlotly({
    year_trend(ggdat.trend() %>% rename(Ratings = Metascore))
  })
  output$trend_rt = renderPlotly({
    year_trend(ggdat.trend() %>% rename(Ratings = Tomatometer))
  })
  ## boxplot of ratings by genre
  output$box_imdb = renderPlotly({
    box_genre(ggdat.ratings() %>% rename(Ratings = `IMDb Rating`))
  })
  output$box_meta = renderPlotly({
    box_genre(ggdat.ratings() %>% rename(Ratings = Metascore))
  })
  output$box_rt = renderPlotly({
    box_genre(ggdat.ratings() %>% rename(Ratings = Tomatometer))
  })

  ## ranking tables
  ## top directors
  dat.directors = reactive({
    movies.all %>% filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Genre %in% input$genre) %>% group_by(imdbID) %>%
      filter(row_number() == 1) %>% select(-Director) %>%
      inner_join(directors, by = 'imdbID') %>% group_by(Director) %>%
      summarise(Movies = n(), `IMDb Rating` = mean(`IMDb Rating`, na.rm = TRUE) %>% round(2),
                Metascore = mean(Metascore, na.rm = T) %>% round(2),
                Tomatometer = mean(Tomatometer, na.rm = T) %>% round(2))
  })
  output$top_dir_imdb = renderDataTable({
    dat = dat.directors() %>% rename(Ratings = `IMDb Rating`)
    ranking(dat, 'dir', input$min_movies_dir)
  })
  output$top_dir_meta = renderDataTable({
    dat = dat.directors() %>% rename(Ratings = Metascore)
    ranking(dat, 'dir', input$min_movies_dir)
  })
  output$top_dir_rt = renderDataTable({
    dat = dat.directors() %>% rename(Ratings = Tomatometer)
    ranking(dat, 'dir', input$min_movies_dir)
  })
  ## top actors 
  dat.actors = reactive({
    movies.all %>% filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Genre %in% input$genre) %>% group_by(imdbID) %>%
      filter(row_number() == 1) %>% select(-Actors) %>%
      inner_join(actors, by = 'imdbID') %>% group_by(Actor) %>%
      summarise(Movies = n(), `IMDb Rating` = mean(`IMDb Rating`, na.rm = TRUE) %>% round(2),
                Metascore = mean(Metascore, na.rm = T) %>% round(2),
                Tomatometer = mean(Tomatometer, na.rm = T) %>% round(2))
  })
  output$top_act_imdb = renderDataTable({
    dat = dat.actors() %>% rename(Ratings = `IMDb Rating`)
    ranking(dat, 'act', input$min_movies_act)
  })
  output$top_act_meta = renderDataTable({
    dat = dat.actors() %>% rename(Ratings = Metascore)
    ranking(dat, 'act', input$min_movies_act)
  })
  output$top_act_rt = renderDataTable({
    dat = dat.actors() %>% rename(Ratings = Tomatometer)
    ranking(dat, 'act', input$min_movies_act)
  })

################################################################################
  ## director insights
################################################################################
  stat.dir1 = reactive({
    directors %>% filter(Director == input$search_director) %>%
      inner_join(movies.all %>% select(-Director), by = 'imdbID')
  })
  stat.dir2 = reactive({
    directors %>% filter(Director == input$search_director) %>%
      inner_join(movies.all %>% select(-Director), by = 'imdbID') %>%
      group_by(imdbID) %>% filter(row_number() == 1) %>%
      mutate(Title = str_sub(Title, end = 30) %>% paste0(ifelse(str_length(Title) > 30, '...', '')))
  })
  ## general infos
  output$gen_info = renderUI({
    src = directorIDs[[input$search_director]]$image
    src = ifelse(is.null(src), 'emptyPortrait.png', src)
    infos = dirInfos[[input$search_director]]
    fluidRow(
      column(
        width = 5, align = 'left', 
        img(src = directorIDs[[input$search_director]]$image, height = 320, width = 213)
      ),
      column(
        width = 7, align = 'left', h2(input$search_director),
        p(b('Age: '), ifelse(is.null(infos$bdate), 'Not found', 2018 - str_replace(infos$bdate, '^.+,', '' %>% str_trim()) %>% as.numeric()), br(),
          b('Birth Date: '), ifelse(is.null(infos$bdate), 'Not found', infos$bdate), br(),
          b('Birth Place: '), ifelse(is.null(infos$bplace), 'Not found', infos$bplace), br(),
          b('Height:'), ifelse(is.null(infos$height), 'Not found', infos$height), br())
      )
    )
  })
  ## career highlight
  output$carir_hlt = renderUI({
    fluidRow(
      column(
        width = 7, align = 'left',
        box(
          width = NULL, height = 320,
          h4('Best Seller'),
          p(b('Box Office: '), stat.dir2()$Title[which.max(stat.dir2()$`BoxOffice`)], '- ',
            paste0('$', max(stat.dir2()$`BoxOffice`, na.rm = T) %>% prettyNum(big.mark = ',', trim = T)), br()),
          h4('Highest rated movies'),
          p(b('IMDb Rating: '), stat.dir2()$Title[which.max(stat.dir2()$`IMDb Rating`)], '- ', max(stat.dir2()$`IMDb Rating`, na.rm = T), br(),
            b('Metascore: '), stat.dir2()$Title[which.max(stat.dir2()$Metascore)], '- ', max(stat.dir2()$Metascore, na.rm = T) / 10, br(),
            b('Tomatometer: '), stat.dir2()$Title[which.max(stat.dir2()$Tomatometer)], '- ', max(stat.dir2()$Tomatometer, na.rm = T), br()),
          h4('Lowest rated movies'),
          p(b('IMDb Rating: '), stat.dir2()$Title[which.min(stat.dir2()$`IMDb Rating`)], '- ', min(stat.dir2()$`IMDb Rating`, na.rm = T), br(),
            b('Metascore: '), stat.dir2()$Title[which.min(stat.dir2()$Metascore)], '- ', min(stat.dir2()$Metascore, na.rm = T) / 10, br(),
            b('Tomatometer: '), stat.dir2()$Title[which.min(stat.dir2()$Tomatometer)], '- ', min(stat.dir2()$Tomatometer, na.rm = T), br())
        )
      ),
      column(
        width = 5, align = 'left',
        valueBox(stat.dir2()$Title %>% unique() %>% length(), 'Movies Made', icon = icon('video-camera'), color = 'aqua', width = 12),
        valueBox(0, 'Oscars Won', icon = icon('user-circle-o'), color = 'yellow', width = 12),
        valueBox(0, 'Oscars Nominated', icon = icon('user-o'), color = 'green', width = 12)
      )
    )
  })
  ## plots
  output$top_bottom_dir_imdb = renderPlotly(bar_ratings(stat.dir2(), 'imdb'))
  output$timeline_dir_imdb = renderPlotly(timeline(stat.dir2(), 'imdb'))

  output$top_bottom_dir_meta = renderPlotly(bar_ratings(stat.dir2(), 'meta'))
  output$timeline_dir_meta = renderPlotly(timeline(stat.dir2(), 'meta'))

  output$top_bottom_dir_rt = renderPlotly(bar_ratings(stat.dir2(), 'rt'))
  output$timeline_dir_rt = renderPlotly(timeline(stat.dir2(), 'rt'))

################################################################################
  ## actor insights
################################################################################
  output$img_act = renderUI({
    src = actorIDs[[input$search_actor]]$image
    src = ifelse(is.null(src), 'emptyPortrait.png', src)
    img(src = actorIDs[[input$search_actor]]$image, height = 300)
  })
  stat.act1 = reactive({
    actors %>% filter(Actor == input$search_actor) %>%
      inner_join(movies.all %>% select(-Actors), by = 'imdbID')
  })
  stat.act2 = reactive({
    actors %>% filter(Actor == input$search_actor) %>%
      inner_join(movies.all %>% select(-Actors), by = 'imdbID') %>%
      group_by(imdbID) %>% filter(row_number() == 1) %>%
      mutate(Title = str_sub(Title, end = 30) %>% paste0(ifelse(str_length(Title) > 30, '...', '')))
  })
  output$top_bottom_act_imdb = renderPlotly(bar_ratings(stat.act2(), 'imdb'))
  output$timeline_act_imdb = renderPlotly(timeline(stat.act2(), 'imdb'))

  output$top_bottom_act_meta = renderPlotly(bar_ratings(stat.act2(), 'meta'))
  output$timeline_act_meta = renderPlotly(timeline(stat.act2(), 'meta'))

  output$top_bottom_act_rt = renderPlotly(bar_ratings(stat.act2(), 'rt'))
  output$timeline_act_rt = renderPlotly(timeline(stat.act2(), 'rt'))
}
