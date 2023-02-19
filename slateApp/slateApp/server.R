function(input, output, session) {
  theme_css <- reactiveVal('www/style_light.css')
  table_length <- reactiveVal(50)
  
  observeEvent(session, {
    params <- getQueryString()
    
    if ('theme' %in% names(params)) {
      if (params$theme == 'dark') {
        theme_css('www/style_dark.css')
      }
    }
  })
  
  output$app_ui <- renderUI({
    req(theme_css())
    fluidPage(
      id = 'app_div',
      theme = shinytheme('yeti'),
      includeCSS(theme_css()),
      navbarPage(
        title = 'Slate System App',
        id = 'app_navbar', 
        tabPanel('Slate System', value = 'system',
                 sidebarLayout(
                   mainPanel(
                     width = 9,
                     column(
                       width = 12,
                       fluidRow(
                         div(
                           style = "
                             display: flex;
                             flex-direction: row;
                             align-items: flex-start;
                             gap: 10px;
                           ",
                           tags$p(style = "padding-top: 10px;", 'Enter a number:'),
                           uiOutput("input_number_ui"),
                           actionButton('convert', 'Convert'), 
                           actionButton('convert_previous', 'Convert Previous'),
                           actionButton('convert_next', 'Convert Next')   
                         )
                       ),
                       fluidRow(
                         uiOutput('error')
                       ),
                       uiOutput('phonemes'),
                       br(),
                       uiOutput('page_length_ui'),
                       fluidRow(DT::DTOutput('table')),
                       br(),
                       br()
                     )
                   ),
                   sidebarPanel(
                     width = 3,
                     HTML("<h4>Options:</h4>"),
                     radioButtons('stress', "Show Stress Markers", c("Yes" = T, "No" = F), inline = T, selected = F),
                     radioButtons('swear', "Remove Swear Words", c("Yes", "No"), inline = T),
                     radioButtons('filter', "Filter Words by Category", c("Yes", "No"), inline = T, selected = "No"),
                     pickerInput(
                       inputId = "word_types",
                       label = "Word Categories",
                       choices =  sort(names(word_type_list)),
                       options = list(`actions-box` = TRUE),
                       multiple = T
                     )
                   )
                 )
        ),
        tabPanel('System Key', value = 'key',
                 column(12, includeHTML("pages/System Key.html"), br(), br())       
        )
      )
    )
  })
  
  output$page_length_ui <- renderUI({
    div(
      style = '
        display: flex;
        flex-direction: row;
        gap: 5px;
        font-size: 13px;
      ',
      tags$p(style = "padding-top: 10px", 'Show'),
      selectInput('page_length', NULL, choices = c(10, 25, 50, 100), selected = 50, 
                  width = '80px'),
      tags$p(style = "padding-top: 10px", 'entries')
    )
  })
  
  observeEvent(input$dark_theme, {
    if (input$dark_theme) {
      theme_css('www/style_dark.css')
    } else {
      theme_css('www/style_light.css')
    }
  })
  
  
  
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
  
  waiter::waiter_hide()
  
  # the input number
  input_number <- reactiveVal("0")
  
  # render the input box for input_number
  output$input_number_ui <- renderUI({
    req(input_number())
    HTML(
      sprintf('
        <div class="form-group shiny-input-container" style="width:150px;">
          <label class="control-label shiny-label-null" for="number" id="number-label"></label>
          <input id="number" type="text" class="form-control" value="%s"
                 autocomplete="off"
                 onclick = "this.setSelectionRange(0, this.value.length)"/>
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
        style = 'padding-bottom: 10px; color: red;',
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
    input_number(input$number)
  })
  
  input_phonemes <- reactive({
    # when input_number changes, update input_phonemes
    req(input_number())
    get_input_phonemes(input_number())
  })
  
  results <- reactive({
    # when input_phonemes changes, update results
    req(input_phonemes())
    # start with the raw word_phonemes_df
    df <- word_phonemes_df
    # remove swear words
    if (input$swear == "Yes") {
      df <- df %>% filter(!(word %in% c(lexicon::profanity_alvarez, 
                                        lexicon::profanity_arr_bad,
                                        lexicon::profanity_banned,
                                        lexicon::profanity_racist,
                                        lexicon::profanity_zac_anger)))
    }
    # apply word type filters
    if (input$filter == 'Yes') {
      df <- df %>%
        filter(word %in% unname(unlist(word_type_list[input$word_types])))
    }
    # run get_results
    out <- get_results(input_phonemes(), as.data.table(df), input$stress)
    out
  })
  
  # render phonemes
  output$phonemes <- renderUI(
    HTML(paste0("<p> Phonemes for number ", input_number(), 
                " = "), "[ ", str_replace_all(str_replace_all(input_phonemes(), fixed("|"), " ] & [ "), fixed("-"), " - "), " ]</p>")
  )
  
  # render results table
  output$table <- renderDataTable({
    req(results(), table_length())
    df <- results() %>%
      distinct(word, .keep_all = T) %>%
      mutate(word = paste0("<a href = 'https://www.google.com/search?q=", word, "' target = '_blank'>", word, "</a>")) %>%
      select(
        word, phonemes, position, n_phonemes, n_letters
      ) %>%
      as.data.table()
    names(df) <- c("Word", "All Phonemes in Word", "Position of Matching Phonemes",
                   "Number of Phonemes in Word", "Number of Letters in Word")
    DT::datatable(df, escape = F, selection = 'none',
                  options = list(dom = "Bfrtip",
                                 pageLength = table_length()))
  })
  
  observeEvent(input$page_length, {
    table_length(input$page_length)
  })
}