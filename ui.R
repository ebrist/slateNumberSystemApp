fluidPage(
  includeCSS('www/style.css'),
  useWaiter(),
  shinyjs::useShinyjs(),
  waiter::waiter_show_on_load(
    html = div(
      id = 'spin_wave_preloader',
      spin_wave(),
      br(),
      p(style = 'color:black; font-size: 16px;', 'Please wait...')
    ),
    color = 'white'
  ),
  tags$head(
    tags$script(src = "customjs.js"),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1, maximum-scale=12.0, minimum-scale=.25, user-scalable=yes")
  ),
  uiOutput('theme'),
  uiOutput('dark_theme_container'),
  textOutput("keep_alive"),
  fluidPage(
    id = 'app_div',
    theme = shinytheme('yeti'),
    navbarPage(
      title = 'Slate System',
      id = 'app_navbar', 
      tabPanel(
        title = 'Word Generator', 
        value = 'system',
        fluidRow(
          style = "margin-top:5px;",
          div(style="display:inline-block;vertical-align:top;", tags$p(style = "margin-top:8px;margin-right:5px;", 'Enter a number:')),
          div(
            style = "display:inline-block;vertical-align:top;",
            div(
              style = "display:inline-block;vertical-align:top;",
              div(style="display:inline-block;vertical-align:top;margin-bottom:-10px;", uiOutput("input_number_ui")),
              div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('convert', 'Convert'))  
            ),
            div(
              style="display:inline-block;vertical-align:top;",
              div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('convert_previous', 'Previous')),
              div(style="display:inline-block;vertical-align:top;padding-bottom:4px;padding-right:7px;", actionButton('convert_next', 'Next'))
            )
          )
        ),
        fluidRow(
          uiOutput('error')
        ),
        div(
          style = "padding-bottom: 5px;",
          uiOutput('phonemes'),
        ),
        fluidRow(style = 'min-height: 370px;', DT::DTOutput('table')),
        div(
          style = 'height: 20px;'
        ),
        hr(),
        div(
          h4("Options"),
          materialSwitch(
            inputId = "stress",
            label = "Show Stress Markers", 
            value = FALSE,
            status = "info"
          )
        ),
        hr(),
        div(
          style = 'height: 20px;'
        )
      ),
      tabPanel(
        title = HTML("System Key</a></li><li><a href='Famous_Figures_Memory_System.jpg' target='_blank'>Famous Figures"), value = 'key',
        div(
          
          includeHTML("www/System_Key.html"), 
          br(), 
          br()
        )
      )
    )
  )
)
