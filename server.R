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
}
