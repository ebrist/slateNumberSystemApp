library(stringr)
library(data.table)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
library(waiter)
library(shinythemes)
library(shinyjs)

# import dataframe of word-phonemes mappings
word_phonemes_df = fread('data/word_phonemes.csv', colClasses = 'character')

# import dataframe of number-phonemes mappings
number_phonemes_df <- fread('data/number_phonemes.csv', colClasses = 'character')
  
# function to return the position of input phonemes within an input word
get_phonemes_position <- function(x, phonemes_search) {
  str_count(str_split_fixed(x, phonemes_search, n = 2)[,1], '-')
}

# function to return the phonemes for an input number
get_phonemes <- function(input_number) {
  number_phonemes_df %>%
    filter(number == as.character(input_number)) %>%
    head(1) %>%
    pull(phonemes)
}

# function to return a list of words containing input phonemes
get_results <- function(phonemes_search, df, stress = F) {
  df %>%
    filter(str_detect(phonemes, paste0('\\b-?', str_replace_all(phonemes_search, fixed('|'), '-?\\b|\\b-?'), '-?\\b'))) %>%
    mutate(
      position = get_phonemes_position(phonemes, phonemes_search), 
      n_phonemes = str_count(phonemes, "-") + 1,
      n_letters = str_length(word), 
      phonemes = if (stress)  phonemes_stress else  phonemes
    ) %>%
    select(-phonemes_stress) %>%
    arrange(position, n_phonemes, n_letters) %>%
    as.data.table()
}
