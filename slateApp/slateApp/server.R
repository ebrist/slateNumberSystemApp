function(input, output, session) {
  waiter::waiter_hide()
  # the input number
  input_number <- reactiveVal("0")
  
  # render the input box for input_number
  output$input_number_ui <- renderUI({
    textInput('number', 'Enter a Number:', 
              value = input_number(), width = '120px')
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
        style = 'padding-bottom: 5px; color: red;',
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
    HTML(paste0("<strong> Phonemes for number '", input_number(), 
                "': "), "[ ", str_replace_all(str_replace_all(input_phonemes(), fixed("|"), " ] & [ "), fixed("-"), " - "), " ]</strong>")
  )
  
  # render results table
  output$table <- renderDataTable({
    req(results())
    df <- results() %>%
      distinct(word, .keep_all = T) %>%
      select(
        word, phonemes, position, n_phonemes, n_letters
      ) %>%
      as.data.table()
    names(df) <- c("Word", "All Phonemes in Word", "Position of Matching Phonemes",
                   "Number of Phonemes in Word", "Number of Letters in Word")
    df
  })
}