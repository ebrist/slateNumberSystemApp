library(stringr)
library(data.table)
library(dplyr)
library(shiny)
library(shinyWidgets)
library(DT)
library(waiter)
library(shinythemes)
library(rcorpora)
library(lexicon)

# import dataframe of word-phonemes mappings
word_phonemes_df = fread('data/word_phonemes.csv', colClasses = 'character')
# import dataframe of number-phonemes mappings
number_phonemes_df <- fread('data/number_phonemes.csv', colClasses = 'character')

# function to return the position of input phonemes within an input word
get_phonemes_position <- function(x, input_phonemes) {
  str_count(str_split_fixed(x, input_phonemes, n = 2)[,1], '-')
}

# function to return the phonemes for an input number
get_input_phonemes <- function(input_number) {
  number_phonemes_df %>%
    filter(number == as.character(input_number)) %>%
    pull(phonemes)
}

# function to return a list of words containing input phonemes
get_results <- function(input_phonemes, df, stress = F) {
  input_phonemes <- paste0('-', input_phonemes, '-')
  df %>%
    mutate(phonemes_filter = paste0('-', phonemes, '-')) %>%
    filter(str_detect(phonemes_filter, input_phonemes)) %>%
    mutate(
      position = get_phonemes_position(phonemes_filter, input_phonemes), 
      n_phonemes = str_count(phonemes, "-") + 1,
      n_letters = str_length(word), 
      phonemes = if (stress)  phonemes_stress else  phonemes
    ) %>%
    select(-phonemes_stress, -phonemes_filter) %>%
    arrange(position, n_phonemes, n_letters) %>%
    as.data.table()
}

# various lists of words
nouns <- rcorpora::corpora("words/nouns")$nouns
verbs_present <- rcorpora::corpora("words/verbs")$verbs$present
verbs_past <- rcorpora::corpora("words/verbs")$verbs$past
adjs <- rcorpora::corpora("words/adjs")$adjs
adverbs <- rcorpora::corpora("words/adverbs")$adverbs
common <- rcorpora::corpora("words/common")$common
personal_nouns <- rcorpora::corpora("words/personal_nouns")$personalNouns
first_names <- lexicon::freq_first_names$Name
last_names <- lexicon::freq_last_names$Surname
animals <- rcorpora::corpora("animals/common")$animals

# combine word lists into a single list
word_type_list = list(
  "Nouns" = tolower(nouns),
  "Verbs-Present" = tolower(verbs_present),
  "Verbs-Past" = tolower(verbs_past),
  "Adjectives" = tolower(adjs),
  "Adverbs" = tolower(adverbs),
  "Common" = tolower(common),
  "Personal Nouns" = tolower(personal_nouns),
  "First Names" = tolower(first_names),
  "Last Names" = tolower(last_names),
  "Animals" = tolower(animals)
)

