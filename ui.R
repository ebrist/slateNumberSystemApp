fluidPage(
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
  tags$head(tags$script(src = "customjs.js")),
  uiOutput('dark_theme_container'),
  uiOutput('app_ui')
)