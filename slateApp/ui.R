fluidPage(
  theme = shinythemes::shinytheme('cerulean'),
  includeCSS('www/style.css'),
  useWaiter(),
  waiter::waiter_show_on_load(
    html = div(
      id = 'spin_wave_preloader',
      spin_wave(),
      br(),
      p(style = 'color:black; font-size: 16px;', 'Please wait...')
    ),
    color = 'white'
  ),
  navbarPage(
    title = 'Slate System App', 
    tabPanel('Convert Numbers',
      sidebarLayout(
        mainPanel(
          width = 9,
          column(
            width = 12,
            fluidRow(uiOutput("input_number_ui")),
            fluidRow(
              actionButton('convert', 'Convert'), 
              actionButton('convert_p', 'Convert Previous'),
              actionButton('convert_next', 'Convert Next')
            ),
            br(),
            uiOutput('phonemes'),
            br(),
            fluidRow(DT::DTOutput('table'))
          )
        ),
        sidebarPanel(
          width = 3,
          HTML("<h3>Options:</h3>"),
          radioButtons('stress', "Show Stress Markers", c("Yes" = T, "No" = F), inline = T),
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
    tabPanel('Number System Key',
      column(12, includeHTML("pages/Print Version.html"), br(), br())       
    )
  )
)