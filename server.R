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
  ggdat.bo = reactive({
    movies.all %>% filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Genre %in% input$genre) %>% group_by(imdbID) %>% filter(row_number() == 1)
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
  ## scatter of box office
  output$bo_year = renderPlotly(bo_scat_yr(ggdat.bo()))
  output$bo_imdb = renderPlotly(bo_scat_rating(ggdat.bo(), 'imdb'))
  output$bo_meta = renderPlotly(bo_scat_rating(ggdat.bo(), 'meta'))
  output$bo_rt = renderPlotly(bo_scat_rating(ggdat.bo(), 'rt'))

  ## ranking tables
  ## top directors
  dat.directors = reactive({
    movies.all %>% filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Genre %in% input$genre) %>% group_by(imdbID) %>%
      filter(row_number() == 1) %>% select(-Director) %>%
      inner_join(dirs, by = 'imdbID') %>% group_by(Director) %>%
      summarise(Movies = n(), `IMDb Rating` = mean(`IMDb Rating`, na.rm = TRUE) %>% round(2),
                Metascore = mean(Metascore, na.rm = T) %>% round(2),
                Tomatometer = mean(Tomatometer, na.rm = T) %>% round(2))
  })
  output$top_dir_imdb = renderDataTable(ranking(dat.directors(), 'dir', input$min_movies_dir, 'IMDb Rating'))
  output$top_dir_meta = renderDataTable(ranking(dat.directors(), 'dir', input$min_movies_dir, 'Metascore'))
  output$top_dir_rt = renderDataTable(ranking(dat.directors(), 'dir', input$min_movies_dir, 'Tomatometer'))
  ## top actors 
  dat.actors = reactive({
    movies.all %>% filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Genre %in% input$genre) %>% group_by(imdbID) %>%
      filter(row_number() == 1) %>% select(-Actors) %>%
      inner_join(acts, by = 'imdbID') %>% group_by(Actor) %>%
      summarise(Movies = n(), `IMDb Rating` = mean(`IMDb Rating`, na.rm = TRUE) %>% round(2),
                Metascore = mean(Metascore, na.rm = T) %>% round(2),
                Tomatometer = mean(Tomatometer, na.rm = T) %>% round(2))
  })
  output$top_act_imdb = renderDataTable(ranking(dat.actors(), 'act', input$min_movies_act, 'IMDb Rating'))
  output$top_act_meta = renderDataTable(ranking(dat.actors(), 'act', input$min_movies_act, 'Metascore'))
  output$top_act_rt = renderDataTable(ranking(dat.actors(), 'act', input$min_movies_act, 'Tomatometer'))
  ## top duos
  dat.duos = reactive({
    movies.all %>% filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Genre %in% input$genre) %>% group_by(imdbID) %>%
      filter(row_number() == 1) %>% select(-c(Actors, Director)) %>%
      inner_join(acts[, -3], by = 'imdbID') %>% inner_join(dirs[, -3], by = 'imdbID') %>%
      group_by(Director, Actor) %>%
      summarise(Movies = n(), `IMDb Rating` = mean(`IMDb Rating`, na.rm = TRUE) %>% round(2),
                Metascore = mean(Metascore, na.rm = T) %>% round(2),
                Tomatometer = mean(Tomatometer, na.rm = T) %>% round(2))
  })
  output$top_duo_imdb = renderDataTable(ranking(dat.duos(), 'duo', input$min_movies_duo, 'IMDb Rating'))
  output$top_duo_meta = renderDataTable(ranking(dat.duos(), 'duo', input$min_movies_duo, 'Metascore'))
  output$top_duo_rt = renderDataTable(ranking(dat.duos(), 'duo', input$min_movies_duo, 'Tomatometer'))

################################################################################
  ## director insights
################################################################################
  stat.dir1 = reactive({
    dirs %>% filter(Director == input$search_director) %>%
      inner_join(movies.all %>% select(-Director), by = 'imdbID')
  })
  stat.dir2 = reactive({
    dirs %>% filter(Director == input$search_director) %>%
      inner_join(movies.all %>% select(-Director), by = 'imdbID') %>%
      group_by(imdbID) %>% filter(row_number() == 1)
  })
  ## general infos
  output$dir_gen_info = renderUI({
    src = dirIDs[[input$search_director]]$image
    src = ifelse(is.null(src), 'emptyPortrait.png', src)
    infos = dirInfos[[input$search_director]]
    trademark = dirTM[[input$search_director]]$trademark
    trademark = trademark[str_length(trademark) < 100]
    trademark = paste0('<ul style="list-style-type:circle; margin-bottom:0px; padding-left:20px">',
                       paste0('<li>', trademark[1:min(4, length(trademark))], '</li>'), '</ul>')
    link = sprintf('http://www.imdb.com/name/%s', dirIDs[[input$search_director]]$id)
    link2TM = sprintf('http://www.imdb.com/name/%s/bio?ref_=nm_dyk_tm_sm#trademark', dirIDs[[input$search_director]]$id)
    fluidRow(
      column(
        width = 5, align = 'left', 
        img(src = dirIDs[[input$search_director]]$image, height = 320, width = 213)
      ),
      column(
        width = 7, align = 'left', h2(a(href = link, title = 'IMDb profile page', input$search_director)),
        p(b('Age: '), ifelse(is.null(infos$bdate), 'Not found', 2018 - str_replace(infos$bdate, '^.+,', '' %>% str_trim()) %>% as.numeric()), br(),
          b('Birth Date: '), ifelse(is.null(infos$bdate), 'Not found', infos$bdate), br(),
          b('Birth Place: '), ifelse(is.null(infos$bplace), 'Not found', infos$bplace), br(),
          b('Height:'), ifelse(is.null(infos$height), 'Not found', infos$height), br(),
          h4(a(href = link2TM, title = 'Trade Mark', 'Trade Mark')), HTML(trademark))
      )
    )
  })
  ## career highlight
  output$dir_carir_hlt = renderUI({
    oscar_tb = dirOscar[[input$search_director]]$oscar
    oscar_won = ifelse(is.null(oscar_tb), 0, sum(oscar_tb$Result))
    oscar_nom = ifelse(is.null(oscar_tb), 0, nrow(oscar_tb))
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
            b('Metascore: '), stat.dir2()$Title[which.max(stat.dir2()$Metascore)], '- ', max(stat.dir2()$Metascore, na.rm = T), br(),
            b('Tomatometer: '), stat.dir2()$Title[which.max(stat.dir2()$Tomatometer)], '- ', max(stat.dir2()$Tomatometer, na.rm = T), br()),
          h4('Lowest rated movies'),
          p(b('IMDb Rating: '), stat.dir2()$Title[which.min(stat.dir2()$`IMDb Rating`)], '- ', min(stat.dir2()$`IMDb Rating`, na.rm = T), br(),
            b('Metascore: '), stat.dir2()$Title[which.min(stat.dir2()$Metascore)], '- ', min(stat.dir2()$Metascore, na.rm = T), br(),
            b('Tomatometer: '), stat.dir2()$Title[which.min(stat.dir2()$Tomatometer)], '- ', min(stat.dir2()$Tomatometer, na.rm = T), br())
        )
      ),
      column(
        width = 5, align = 'left',
        valueBox(stat.dir2()$Title %>% unique() %>% length(), 'Movies Made', icon = icon('video-camera'), color = 'aqua', width = 12),
        valueBox(oscar_won, 'Oscars Won', icon = icon('user-circle-o'), color = 'yellow', width = 12),
        valueBox(oscar_nom, 'Oscars Nominated', icon = icon('user-o'), color = 'green', width = 12)
      )
    )
  })
  ## statistics
  output$dir_sumry = renderDataTable(dt_sumry(stat.dir2()))
  output$dir_movies = renderDataTable(dt_movies(stat.dir2()))
  ## data for finding collaborators
  dat_dir_act = reactive({
    movies.all %>% group_by(imdbID) %>%
      filter(row_number() == 1) %>% select(-c(Actors, Director)) %>%
      inner_join(dirs[, -3] %>% filter(Director == input$search_director), by = 'imdbID') %>%
      inner_join(acts[, -3], by = 'imdbID') %>% ft_most_collab(input$search_director, 'act')
  })
  output$dir_collab = renderUI(h4(sprintf('Most Frequent Collaborator(Actor) - %s', dat_dir_act()$collab)))
  output$dir_ft_most_collab = renderFormattable(dat_dir_act()$ft)
  ## plots
  output$dir_movies_bar = renderPlotly(bar_ratings(stat.dir2(), input$dir_radio))
  output$dir_timeline = renderPlotly(timeline(stat.dir2(), input$dir_radio))
  output$dir_genres_loli = renderPlotly(loli_genres(stat.dir1()))
  
################################################################################
  ## actor insights
################################################################################
  stat.act1 = reactive({
    acts %>% filter(Actor == input$search_actor) %>%
      inner_join(movies.all %>% select(-Actors), by = 'imdbID')
  })
  stat.act2 = reactive({
    acts %>% filter(Actor == input$search_actor) %>%
      inner_join(movies.all %>% select(-Actors), by = 'imdbID') %>%
      group_by(imdbID) %>% filter(row_number() == 1)
  })
  ## general infos
  output$act_gen_info = renderUI({
    src = actIDs[[input$search_actor]]$image
    src = ifelse(is.null(src), 'emptyPortrait.png', src)
    infos = actInfos[[input$search_actor]]
    trademark = actTM[[input$search_actor]]$trademark
    trademark = trademark[str_length(trademark) < 100]
    trademark = paste0('<ul style="list-style-type:circle; margin-bottom:0px; padding-left:20px">',
                       paste0('<li>', trademark[1:min(4, length(trademark))], '</li>'), '</ul>')
    link = sprintf('http://www.imdb.com/name/%s', actIDs[[input$search_actor]]$id)
    link2TM = sprintf('http://www.imdb.com/name/%s/bio?ref_=nm_dyk_tm_sm#trademark', actIDs[[input$search_actor]]$id)
    fluidRow(
      column(
        width = 5, align = 'left', 
        img(src = actIDs[[input$search_actor]]$image, height = 320, width = 213)
      ),
      column(
        width = 7, align = 'left', h2(a(href = link, title = 'IMDb profile page', input$search_actor)),
        p(b('Age: '), ifelse(is.null(infos$bdate), 'Not found', 2018 - str_replace(infos$bdate, '^.+,', '' %>% str_trim()) %>% as.numeric()), br(),
          b('Birth Date: '), ifelse(is.null(infos$bdate), 'Not found', infos$bdate), br(),
          b('Birth Place: '), ifelse(is.null(infos$bplace), 'Not found', infos$bplace), br(),
          b('Height:'), ifelse(is.null(infos$height), 'Not found', infos$height), br(),
          h4(a(href = link2TM, title = 'Trade Mark', 'Trade Mark')), HTML(trademark))
      )
    )
  })
  ## career highlight
  output$act_carir_hlt = renderUI({
    oscar_tb = actOscar[[input$search_actor]]$oscar
    oscar_won = ifelse(is.null(oscar_tb), 0, sum(oscar_tb$Result))
    oscar_nom = ifelse(is.null(oscar_tb), 0, nrow(oscar_tb))
    fluidRow(
      column(
        width = 7, align = 'left',
        box(
          width = NULL, height = 320,
          h4('Best Seller'),
          p(b('Box Office: '), stat.act2()$Title[which.max(stat.act2()$`BoxOffice`)], '- ',
            paste0('$', max(stat.act2()$`BoxOffice`, na.rm = T) %>% prettyNum(big.mark = ',', trim = T)), br()),
          h4('Highest rated movies'),
          p(b('IMDb Rating: '), stat.act2()$Title[which.max(stat.act2()$`IMDb Rating`)], '- ', max(stat.act2()$`IMDb Rating`, na.rm = T), br(),
            b('Metascore: '), stat.act2()$Title[which.max(stat.act2()$Metascore)], '- ', max(stat.act2()$Metascore, na.rm = T), br(),
            b('Tomatometer: '), stat.act2()$Title[which.max(stat.act2()$Tomatometer)], '- ', max(stat.act2()$Tomatometer, na.rm = T), br()),
          h4('Lowest rated movies'),
          p(b('IMDb Rating: '), stat.act2()$Title[which.min(stat.act2()$`IMDb Rating`)], '- ', min(stat.act2()$`IMDb Rating`, na.rm = T), br(),
            b('Metascore: '), stat.act2()$Title[which.min(stat.act2()$Metascore)], '- ', min(stat.act2()$Metascore, na.rm = T), br(),
            b('Tomatometer: '), stat.act2()$Title[which.min(stat.act2()$Tomatometer)], '- ', min(stat.act2()$Tomatometer, na.rm = T), br())
        )
      ),
      column(
        width = 5, align = 'left',
        valueBox(stat.act2()$Title %>% unique() %>% length(), 'Movies Made', icon = icon('video-camera'), color = 'aqua', width = 12),
        valueBox(oscar_won, 'Oscars Won', icon = icon('user-circle-o'), color = 'yellow', width = 12),
        valueBox(oscar_nom, 'Oscars Nominated', icon = icon('user-o'), color = 'green', width = 12)
      )
    )
  })
  ## statistics
  output$act_sumry = renderDataTable(dt_sumry(stat.act2()))
  output$act_movies = renderDataTable(dt_movies(stat.act2()))
  ## data for finding collaborators
  dat_act_dir = reactive({
    movies.all %>% group_by(imdbID) %>%
      filter(row_number() == 1) %>% select(-c(Actors, Director)) %>%
      inner_join(acts[, -3] %>% filter(Actor == input$search_actor), by = 'imdbID') %>%
      inner_join(dirs[, -3], by = 'imdbID') %>% ft_most_collab(input$search_actor, 'dir')
  })
  output$act_collab_dir = renderUI(h4(sprintf('Most Frequent Collaborator(Director) - %s', dat_act_dir()$collab)))
  output$act_ft_most_collab_dir = renderFormattable(dat_act_dir()$ft)
  dat_act_act = reactive({
    tmp = movies.all %>% group_by(imdbID) %>%
      filter(row_number() == 1) %>% select(-c(Actors, Director)) %>%
      inner_join(acts[, -3] %>% filter(Actor == input$search_actor), by = 'imdbID') %>% select(-Actor) %>%
      inner_join(acts[, -3], by = 'imdbID') %>% ft_most_collab(input$search_actor, 'act')
  })
  output$act_collab_act = renderUI(h4(sprintf('Most Frequent Collaborator(Actor) - %s', dat_act_act()$collab)))
  output$act_ft_most_collab_act = renderFormattable(dat_act_act()$ft)
  ## plots
  output$act_movies_bar = renderPlotly(bar_ratings(stat.act2(), input$act_radio))
  output$act_timeline = renderPlotly(timeline(stat.act2(), input$act_radio))
  output$act_genres_loli = renderPlotly(loli_genres(stat.act1()))

################################################################################
  ## fun facts page
################################################################################
  ## oscar lucky/unlucky
  output$dt_dir_unlucky = renderDataTable(oscar_unlucky(dirOscar_tb, n = 20))
  output$dt_act_unlucky = renderDataTable(oscar_unlucky(actOscar_tb, n = 20))
  output$dt_dir_lucky = renderDataTable(oscar_lucky(dirOscar_tb, n = 20))
  output$dt_act_lucky = renderDataTable(oscar_lucky(actOscar_tb, n = 20))
  ## movies with polarizing ratings
  ## output$polar_radio_2 = renderUI({
  ##   choices_2 = c('IMDb_Rating', 'Metascore', 'Tomatometer') %>% setdiff(input$polar_radio_1)
  ##   radioButtons(inputId = 'polar_radio_2', label = 'Select the second rating system',
  ##                choices = choices_2, selected = choices_2[1], inline = TRUE)
  ## })
  polar_tb = reactive({
    choice_1 = 'IMDb Rating' ##input$polar_radio_1
    choice_2 = 'Metascore' ##input$polar_radio_2
    tmp = movies.all %>% select(imdbID, Title, Genre, Year, `IMDb Rating`, Metascore) %>%
      mutate(Difference = abs(`IMDb Rating` - Metascore)) %>% filter(!is.na(Difference)) %>%
      arrange(desc(Difference)) %>% filter(Difference >= 30)
  })
  ## output$polar_top = renderFormattable(ft_polar(polar_tb(), n = 50))
  output$polar_top = renderDataTable(dt_polar(polar_tb(), n = 50))
  output$polar_bar = renderPlot(bar_polar(polar_tb(), movies.all))
}
