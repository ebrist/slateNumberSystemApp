function(input, output, session) {
  theme_css <- reactiveVal(NULL)
  page_length <- reactiveVal(50)
  input_number <- reactiveVal(0)
  table_data_length <- reactiveVal(50)
  page_number <- reactiveVal(1)
  max_page_number <- reactiveVal(1)
  results_length <- reactiveVal(NULL)
  
  output$app_ui <- renderUI({
    req(theme_css())
    print('app_ui')
    fluidPage(
      id = 'app_div',
      theme = shinytheme('yeti'),
      includeCSS(theme_css()),
      navbarPage(
        title = 'Slate System App',
        id = 'app_navbar', 
        tabPanel(
          title = 'Slate System', 
          value = 'system',
          column(
            width = 10,
            offset = 1,
            fluidRow(
              style = "margin-top:5px;",
              div(style="display: inline-block;vertical-align:top;", tags$p(style = "margin-top: 10px;margin-right: 5px;", 'Enter a number:')),
              div(style="display: inline-block;vertical-align:top;", uiOutput("input_number_ui")),
              div(style="display: inline-block;vertical-align:top;", actionButton('convert', 'Convert', width = '100px')), 
              div(style="display: inline-block;vertical-align:top;", actionButton('convert_previous', 'Previous', width = '100px')),
              div(style="display: inline-block;vertical-align:top;", actionButton('convert_next', 'Next', width = '100px')),
              div(
                style="display: inline-block;vertical-align:top;margin-bottom: 15px;margin-bottom: 10px;margin-top: 10px;margin-left: 50px;", 
                materialSwitch(
                  inputId = "stress",
                  label = "Stress Markers", 
                  value = FALSE,
                  status = "primary"
                )
              )
            ),
            fluidRow(
              uiOutput('error')
            ),
            uiOutput('phonemes'),
            br(),
            fluidRow(
              div(style="display: inline-block;vertical-align:top; margin-top: 10px; font-size: 15px; margin-right: 10px;", uiOutput('results_info')),
              div(
                style="display: inline-block;vertical-align:top;font-size: 15px; margin-right: 10px;", 
                div(
                  style = '
                    display: flex;
                    flex-direction: row;
                    gap: 5px;
                  ',
                  tags$p(style = "margin-top: 10px", 'Show'),
                  uiOutput('page_length_ui'),
                  tags$p(style = "margin-top: 10px", 'results per page.')
                )
              ),
              div(
                style="display: inline-block;vertical-align:top;font-size: 15px; float: right;", 
                div(
                  style = '
                    display: flex;
                    flex-direction: row;
                    gap: 5px;
                  ',
                  div(style="display: inline-block;vertical-align:top; margin-top: 10px; margin-right: 5px;", uiOutput('page_number_info')), 
                  div(style="display: inline-block;vertical-align:top;", actionButton('previous_page', 'Previous', width = '100px')), 
                  uiOutput('page_number_ui'),
                  div(style="display: inline-block;vertical-align:top;", actionButton('go', 'Search', width = '120px')), 
                  div(style="display: inline-block;vertical-align:top;", actionButton('next_page', 'Next', width = '100px')) 
                )
              )
            ),
            fluidRow(DT::DTOutput('table')),
            fluidRow(
              style = "margin-top: 10px;",
              div(style="display: inline-block;vertical-align:top; margin-top: 10px; font-size: 15px; margin-right: 10px;", uiOutput('results_info2')),
              div(
                style="display: inline-block;vertical-align:top;font-size: 15px; margin-right: 10px;", 
                div(
                  style = '
                    display: flex;
                    flex-direction: row;
                    gap: 5px;
                  ',
                  tags$p(style = "margin-top: 10px", 'Show'),
                  uiOutput('page_length_ui2'),
                  tags$p(style = "margin-top: 10px", 'results per page.')
                )
              ),
              div(
                style="display: inline-block;vertical-align:top;font-size: 15px; float: right;", 
                div(
                  style = '
                    display: flex;
                    flex-direction: row;
                    gap: 5px;
                  ',
                  div(style="display: inline-block;vertical-align:top; margin-top: 10px; margin-right: 5px;", uiOutput('page_number_info2')), 
                  div(style="display: inline-block;vertical-align:top;", actionButton('previous_page2', 'Previous', width = '100px')), 
                  uiOutput('page_number_ui2'),
                  div(style="display: inline-block;vertical-align:top;", actionButton('go2', 'Search', width = '120px')), 
                  div(style="display: inline-block;vertical-align:top;", actionButton('next_page2', 'Next', width = '100px')) 
                )
              )
            ),
            br(),
            br(),
            br()
          )
        ),
        tabPanel('System Key', value = 'key',
                 column(12, includeHTML("pages/System Key.html"), br(), br())       
        )
      )
    )
  })
  
  # render the input box for input_number
  output$input_number_ui <- renderUI({
    req(input_number())
    print('input_number_ui')
    HTML(
      sprintf('
        <div class="form-group shiny-input-container" style="width:150px;">
          <label class="control-label shiny-label-null" for="number" id="number-label"></label>
          <input id="number" type="text" class="form-control" value="%s"
                 autocomplete="off"/>
        </div>        
      ', input_number())
    )
  })
  
  output$error <- renderUI({
    req(input$number)
    print('error')
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
  
  output$results_info <- renderUI({
    req(results_length())
    isolate(req(input_number()))
    print('results_info')
    HTML(paste0('There are <b>', results_length(), '</b> total results.'))
  })
  
  output$results_info2 <- renderUI({
    req(results_length())
    isolate(req(input_number()))
    print('results_info2')
    HTML(paste0('There are <b>', results_length(), '</b> total results.'))
  })
  
  output$page_length_ui <- renderUI({
    req(page_length())
    isolate(req(input_number()))
    print('page_length_ui')
    selectInput('page_length', NULL, choices = c(10, 25, 50, 100), selected = page_length(), width = '80px')
  })
  
  output$page_length_ui2 <- renderUI({
    req(page_length())
    isolate(req(input_number()))
    print('page_length_ui2')
    selectInput('page_length2', NULL, choices = c(10, 25, 50, 100), selected = page_length(), width = '80px')
  })
  
  output$page_number_info <- renderUI({
    req(page_number(), max_page_number())
    req(page_number() <= max_page_number())
    isolate(req(input_number()))
    print('page_number_info')
    HTML(paste0('Showing Page ', page_number(), ' of ', max_page_number()))
  })
  
  
  output$page_number_info2 <- renderUI({
    req(page_number(), max_page_number())
    req(page_number() <= max_page_number())
    isolate(req(input_number()))
    print('page_number_info2')
    HTML(paste0('Showing Page ', page_number(), ' of ', max_page_number()))
  })
  
  output$page_number_ui <- renderUI({
    req(page_number(), max_page_number())
    isolate(req(input_number()))
    print('page_number_ui')
    if (page_number() >= max_page_number()) {
      selected_page <- max_page_number()
    } else if (page_number() < 1) {
      selected_page <- 1
    } else {
      selected_page <- page_number()
    }
    HTML(
      sprintf('
        <div class="form-group shiny-input-container" style="width:100px;">
          <label class="control-label shiny-label-null" for="page" id="page-label"></label>
          <input id="page" type="text" class="form-control" value="%s"
                 autocomplete="off"/>
        </div>        
      ', selected_page)
    )
  })
  
  output$page_number_ui2 <- renderUI({
    req(page_number(), max_page_number())
    isolate(req(input_number()))
    print('page_number_ui2')
    if (page_number() >= max_page_number()) {
      selected_page <- max_page_number()
    } else if (page_number() < 1) {
      selected_page <- 1
    } else {
      selected_page <- page_number()
    }
    HTML(
      sprintf('
        <div class="form-group shiny-input-container" style="width:100px;">
          <label class="control-label shiny-label-null" for="page2" id="page2-label"></label>
          <input id="page2" type="text" class="form-control" value="%s"
                 autocomplete="off"/>
        </div>        
      ', selected_page)
    )
  })
  
  # monitor changes to convert_previous button
  observeEvent(input$convert_previous, {
    print('obs convert_previous')
    int_number <- as.integer(input_number())
    len_number <- nchar(input_number())
    new_number <- min(max(0, int_number - 1), 9999)
    input_number(stringr::str_pad(new_number, len_number, pad = "0"))
  })
  
  # monitor changes to convert_next button
  observeEvent(input$convert_next, {
    print('obs convert_next')
    int_number <- as.integer(input_number())
    len_number <- nchar(input_number())
    new_number <- min(max(0, int_number + 1), 9999)
    input_number(stringr::str_pad(new_number, len_number, pad = "0"))
  })
  
  # monitor changes to convert button
  observeEvent(input$convert, {
    print('obs convert')
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
    print('input_phonemes')
    get_phonemes(input_number())
  })
  
  # reactive dataframe containing the results of input_number()
  # react to input_phonemes() and input$stress
  results <- reactive({
    # when input_phonemes changes, update results
    req(input_phonemes(), is.logical(input$stress))
    print('results')
    page_number(1)
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
    print('results_length')
    nrow(results())
  })
  
  # render phonemes ui
  output$phonemes <- renderUI({
    req(input_number(), input_phonemes())
    print('phonemes')
    HTML(paste0("<p> Phonemes for number ", input_number(), 
                " = "), "[ ", str_replace_all(str_replace_all(input_phonemes(), fixed("|"), " ] & [ "), fixed("-"), " - "), " ]</p>")
  })
  
  # reactive dataframe containing the subset of results being displayed in the datatable
  # react to page_length(), page_number(), and results()
  table_data <- reactive({
    req(page_length(), page_number(), results())
    start_row <- page_length() * (page_number() - 1) + 1
    end_row <- page_length() * page_number()
    out <- results()[start_row:end_row, ]
    # fill NA rows with empty strings
    out[is.na(out)] <- ''
    out
  })
  
  # react to table_data() and update table_data_length()
  observeEvent(table_data(), {
    req(table_data_length(), table_data())
    if (table_data_length() != nrow(table_data())) {
      table_data_length(nrow(table_data()))  
    }
  })
  
  
  # render results table
  output$table <- DT::renderDataTable({
    print('render table request')
    waiter::waiter_hide()
    # only react to table_data_length() to redraw table, otherwise update data with replaceData
    req(table_data_length())
    print('render table submit')
    DT::datatable(isolate(table_data()), escape = F, selection = 'none', width = '100%', rownames = F,
                  options = list(dom = 't',
                                 pageLength = table_data_length(),  
                                 processing = FALSE,
                                 autoWidth = TRUE, autoHeight = FALSE, scrollY = T,
                                 columnDefs = list(list(className = 'dt-left', targets = "_all"),
                                                   list(width = '19%', targets = list(1:5)))))
  })
  
  # update datatable source data when table_data() changes
  proxy <- DT::dataTableProxy('table')
  observe({
    print('update table_data')
    DT::replaceData(proxy, table_data(), rownames = F)
  })
  
  observeEvent(input$page_length, {
    req(!is.na(suppressWarnings(is.integer(input$page_length))), page_length())
    if (as.integer(input$page_length) != page_length()) {
      page_length(as.integer(input$page_length))  
    }
  }, ignoreInit = T)
  
  observeEvent(input$page_length2, {
    req(!is.na(suppressWarnings(is.integer(input$page_length2))), page_length())
    if (as.integer(input$page_length2) != page_length()) {
      page_length(as.integer(input$page_length2))  
    }
  }, ignoreInit = T)
  
  observeEvent(page_length(), {
    req(results_length(), page_length(), max_page_number())
    if (max(ceiling(results_length() / page_length()), 1) != max_page_number()) {
      max_page_number(max(ceiling(results_length() / page_length()), 1))
    }
  })
  
  observeEvent(results_length(), {
    req(page_length(), results_length(), max_page_number())
    if (max(ceiling(results_length() / page_length()), 1) != max_page_number()) {
      max_page_number(max(ceiling(results_length() / page_length()), 1)) 
    }
  })
  
  observeEvent(max_page_number(), {
    req(page_number(), max_page_number())
    if (page_number() > max_page_number()) {
      page_number(max_page_number())
    }
  })
  
  observeEvent(input$go, {
    req(max_page_number(), input$page)
    req(!is.na(suppressWarnings(as.integer(input$page))))
    if (suppressWarnings(as.integer(input$page)) < 1) {
      updateTextInput(inputId = 'page', value = '1')
      updateTextInput(inputId = 'page2', value = '1')
      page_number(1)
    } else if (suppressWarnings(as.integer(input$page)) > max_page_number()) {
      updateTextInput(inputId = 'page', value = as.character(max_page_number()))
      updateTextInput(inputId = 'page2', value = as.character(max_page_number()))
      page_number(max_page_number())
    } else {
      page_number(suppressWarnings(as.integer(input$page)))
    }
  })
  
  observeEvent(input$go2, {
    req(max_page_number(), input$page2)
    req(!is.na(suppressWarnings(as.integer(input$page2))))
    if (suppressWarnings(as.integer(input$page2)) < 1) {
      updateTextInput(inputId = 'page', value = '1')
      updateTextInput(inputId = 'page2', value = '1')
      page_number(1)
    } else if (suppressWarnings(as.integer(input$page2)) > max_page_number()) {
      updateTextInput(inputId = 'page', value = as.character(max_page_number()))
      updateTextInput(inputId = 'page2', value = as.character(max_page_number()))
      page_number(max_page_number())
    } else {
      page_number(suppressWarnings(as.integer(input$page2)))
    }
  })
  
  observeEvent(c(input$previous_page, input$previous_page2), {
    req(page_number(), max_page_number())
    if (page_number() > max_page_number()) {
      page_number(max_page_number())
    } else if (page_number() >= 2) {
      page_number(page_number() - 1)
    } else {
      page_number(1)
    }
  })
  
  observeEvent(c(input$next_page, input$next_page2), {
    req(page_number(), max_page_number())
    if (page_number() < 1L) {
      page_number(1L)
    } else if (page_number() < max_page_number()) {
      page_number(page_number() + 1)
    } else {
      page_number(max_page_number())
    }
  })
  
  # on session start, extract query string param and look for number and dark theme
  observeEvent(session, {
    params <- getQueryString()
    if ('theme' %in% names(params)) {
      if (params$theme == 'dark') {
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
  })
  
  # on input$dark_theme, update theme_css
  observeEvent(input$dark_theme, {
    if (input$dark_theme) {
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
        materialSwitch('dark_theme', value = ifelse(theme_css() == 'www/style_dark.css', T, F))
      ) 
    } else {
      ui <- div()
    }
    
    return(ui)
  })
}