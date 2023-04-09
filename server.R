function(input, output, session) {
  theme_css <- reactiveVal(NULL)
  page_length <- reactiveVal(50)
  input_number <- reactiveVal(0)
  table_data_length <- reactiveVal(50)
  page_number <- reactiveVal(1)
  max_page_number <- reactiveVal(1)
  results_length <- reactiveVal(NULL)
  
  output$theme <- renderUI({
    req(theme_css())
    div(includeCSS(theme_css()))
  })
  
  # render the input box for input_number
  output$input_number_ui <- renderUI({
    req(input_number())
    HTML(
      sprintf('
        <div class="form-group shiny-input-container" style = "width: 70px;">
          <label class="control-label shiny-label-null" for="number" id="number-label"></label>
          <input id="number" type="text" class="form-control" value="%s" 
                 onfocus="this.setSelectionRange(0, this.value.length)"
                 autocomplete="off"/>
        </div>        
      ', input_number())
    )
  })
  
  output$error <- renderUI({
    req(input$number)
    error <- F
    if (nchar(input$number) == 0) {
      error <- T
    } else if (nchar(input$number) > 4) {
      error <- T
    } else if (is.na(as.integer(input$number))) {
      error <- T
    } else if (as.integer(input$number) < 0) {
      error <- T
    } else if (as.integer(input$number) > 9999) {
      error <- T
    } else if (input$number == '-0') {
      error <- T
    }
    if (error) {
      return(div(
        style = 'margin-bottom: 10px; color: red;',
        HTML("Invalid Number. Number must be between '0' and '9999'.")
      ))
    } else {
      return(div())
    }
  })
  
  
  # monitor changes to convert_previous button
  observeEvent(input$convert_previous, {
    int_number <- as.integer(input_number())
    len_number <- nchar(input_number())
    new_number <- min(max(0, int_number - 1), 9999)
    input_number(stringr::str_pad(new_number, len_number, pad = "0"))
  })
  
  # monitor changes to convert_next button
  observeEvent(input$convert_next, {
    int_number <- as.integer(input_number())
    len_number <- nchar(input_number())
    new_number <- min(max(0, int_number + 1), 9999)
    input_number(stringr::str_pad(new_number, len_number, pad = "0"))
  })
  
  # monitor changes to convert button
  observeEvent(input$convert, {
    req(input$number)
    error <- F
    if (nchar(input$number) == 0) {
      error <- T
    } else if (nchar(input$number) > 4) {
      error <- T
    } else if (is.na(as.integer(input$number))) {
      error <- T
    } else if (as.integer(input$number) < 0) {
      error <- T
    } else if (as.integer(input$number) > 9999) {
      error <- T
    } else if (input$number == '-0') {
      error <- T
    }
    if (!error) {
      input_number(input$number)
    }
  })
  
  input_phonemes <- reactive({
    # when input_number changes, update input_phonemes
    req(input_number())
    get_phonemes(input_number())
  })
  
  # reactive dataframe containing the results of input_number()
  # react to input_phonemes() and input$stress
  results <- reactive({
    # when input_phonemes changes, update results
    req(input_phonemes(), is.logical(input$stress))
    df <- get_results(input_phonemes(), word_phonemes_df, input$stress) %>%
      distinct(word, .keep_all = T) %>%
      mutate(
        word = paste0("<a href = 'https://www.google.com/search?q=", word, "' target = '_blank'>", word, "</a>"),
        position = as.integer(position), n_phonemes = as.integer(n_phonemes), n_letters = as.integer(n_letters),
        ` ` = (min(1, n())):n()
      ) %>%
      select(
        ` `, word, phonemes, position, n_phonemes, n_letters
      ) %>%
      arrange(` `) %>%
      as.data.table()
    # update names
    names(df) <- c(" ", "Word", "All Phonemes in Word", "Position of Matching Phonemes",
                   "Number of Phonemes in Word", "Number of Letters in Word")
    # return df
    df
  })
  
  # track the total number of results
  results_length <- reactive({
    req(results())
    nrow(results())
  })
  
  # render phonemes ui
  output$phonemes <- renderUI({
    req(input_number(), input_phonemes())
    HTML(paste0("<p> Phonemes for number ", input_number(), 
                " = "), "<nobr>[ ", str_replace_all(str_replace_all(input_phonemes(), fixed("|"), " ]</nobr> & <nobr>[ "), fixed("-"), " - "), " ]</nobr></p>")
  })
  
  # render results table
  output$table <- DT::renderDataTable({
    waiter::waiter_hide()
    req(results())
    DT::datatable(results(), escape = F, rownames = F, selection = 'none',
                  plugins = 'input', 
                  options = list(pagingType = 'input', 
                                 pageLength = isolate(page_length()),
                                 dom = "<'row'ip><'row'tr><'row'lip>",
                                 autoWidth = F, 
                                 autoHeight = FALSE, 
                                 scrollY = T, 
                                 scrollX = T))
  })
  
  observeEvent(input$table_rows_current, {
    if (input$table_state$length != page_length()) {
      page_length(input$table_state$length)
    }
  })
  
  # on session start, extract query string param and look for number and dark theme
  observeEvent(session, {
    params <- getQueryString()
    if ('theme' %in% names(params)) {
      if (params$theme == 'dark' & input$app_navbar == 'system') {
        theme_css('www/style_dark.css')
      } else {
        theme_css('www/style_light.css')
      }
    } else {
      theme_css('www/style_light.css')
    }
    if ('number' %in% names(params)) {
      error <- F
      if (nchar(params$number) == 0) {
        error <- T
      } else if (nchar(params$number) > 4) {
        error <- T
      } else if (is.na(as.integer(params$number))) {
        error <- T
      } else if (as.integer(params$number) < 0) {
        error <- T
      } else if (as.integer(params$number) > 9999) {
        error <- T
      } else if (params$number == '-0') {
        error <- T
      }
      if (!error) {
        input_number(params$number)
      }
    }
    if ('page' %in% names(params)) {
      if (params$page == 'key') {
        updateTabsetPanel(getDefaultReactiveDomain(), 'app_navbar', 'key')
      } 
    }
  })
  
  # on input$dark_theme, update theme_css
  observeEvent(c(input$app_navbar, input$dark_theme), {
    if (input$dark_theme & input$app_navbar == 'system') {
      theme_css('www/style_dark.css')
    } else {
      theme_css('www/style_light.css')
    }
  }, ignoreInit = T)
  
  # render the dark theme container
  # show if tab is the main system tab, hide if tab is the key
  output$dark_theme_container <- renderUI({
    req(input$app_navbar)
    if (input$app_navbar == 'system') {
      ui <- div(
        id = 'dark_theme_ui',
        style = '    
          flex-direction: row;
          display: flex;
          gap: 10px;
        ',
        tags$p('Dark Mode'),
        materialSwitch('dark_theme', value = ifelse(theme_css() == 'www/style_dark.css', T, F), status = 'info')
      ) 
    } else {
      ui <- div()
    }
    
    return(ui)
  })
  
  output$system_key <- renderImage({
    list(src = ifelse(theme_css() == 'www/style_dark.css', 'www/system_key_dark.png', 'www/system_key_white.png'))
  })
  
  output$keep_alive <- renderText({
    req(input$alive_count)
    input$alive_count
  })
  
}