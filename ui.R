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
  uiOutput('theme'),
  uiOutput('dark_theme_container'),
  textOutput("keep_alive"),
  fluidPage(
    id = 'app_div',
    theme = shinytheme('yeti'),
    navbarPage(
      title = 'Slate System App',
      id = 'app_navbar', 
      tabPanel(
        title = 'Slate System', 
        value = 'system',
        column(
          width = 10,
          offset = 1,
          align = 'left',
          fluidRow(
            style = "margin-top:5px;",
            div(style="display:inline-block;vertical-align:top;", tags$p(style = "margin-top:8px;margin-right:5px;", 'Enter a number:')),
            div(
              style = "display:inline-block;vertical-align:top;",
              div(
                style = "display:inline-block;vertical-align:top;",
                div(style="display:inline-block;vertical-align:top;margin-bottom:-10px;", uiOutput("input_number_ui")),
                div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('convert', 'Convert', width = '80px'))  
              ),
              div(
                style="display:inline-block;vertical-align:top;",
                div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('convert_previous', 'Previous', width = '80px')),
                div(style="display:inline-block;vertical-align:top;padding-bottom:4px;padding-right:7px;", actionButton('convert_next', 'Next', width = '80px'))
              )
            ),
            div(
              class = 'dark-theme-toggle',
              style="display:inline-block;vertical-align:top;margin-bottom:0px;margin-bottom:10px;margin-top:8px;", 
              materialSwitch(
                inputId = "stress",
                label = "Stress Markers", 
                value = FALSE,
                status = "info"
              )
            )
          ),
          fluidRow(
            uiOutput('error')
          ),
          div(
            style = "padding-bottom: 10px;",
            uiOutput('phonemes'),
          ),
          fluidRow(
            div(style="display:inline-block;vertical-align:top;margin-top:9px;font-size:15px;margin-right:10px;margin-bottom:15px;", uiOutput('results_info')),
            div(
              style="display:inline-block;vertical-align:top;font-size:15px;margin-right:10px;", 
              div(
                style='display:flex;flex-direction:row;flex-wrap:wrap;gap:5px;',
                div(style="margin-top:9px", tags$p('Show')),
                div(style="margin-bottom:-25px", uiOutput('page_length_ui')),
                div(style="margin-top:9px;margin-bottom:20px;", tags$p('results per page.'))
              )
            ),
            div(
              style="display:inline-block;vertical-align:top;font-size:15px;float:right;", 
              div(
                style = 'display:flex;flex-direction:row;flex-wrap:wrap;gap:5px;',
                div(style="display:inline-block;vertical-align:top;margin-top:9px;margin-right:5px;padding-bottom:4px;", uiOutput('page_number_info')), 
                div(
                  style="display:inline-block;vertical-align:",
                  div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('previous_page', 'Previous', width = '80px')), 
                  div(
                    style="display:inline-block;vertical-align:",
                    div(style="display:inline-block;vertical-align:top;margin-bottom:-15px;padding-bottom:4px;", uiOutput('page_number_ui')),
                    div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('go', 'Search', width = '80px'))
                  ),
                  div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('next_page', 'Next', width = '80px'))   
                )
              )
            )
          ),
          fluidRow(style = 'min-height: 370px;', DT::DTOutput('table')),
          fluidRow(
            style = "padding-top: 15px;",
            div(style="display:inline-block;vertical-align:top;margin-top:9px;font-size:15px;margin-right:10px;margin-bottom:15px;", uiOutput('results_info2')),
            div(
              style="display:inline-block;vertical-align:top;font-size:15px;margin-right:10px;", 
              div(
                style='display:flex;flex-direction:row;flex-wrap:wrap;gap:5px;',
                div(style="margin-top:9px", tags$p('Show')),
                div(style="margin-bottom:-25px", uiOutput('page_length_ui2')),
                div(style="margin-top:9px;margin-bottom:20px;", tags$p('results per page.'))
              )
            ),
            div(
              style="display:inline-block;vertical-align:top;font-size:15px;float:right;", 
              div(
                style = 'display:flex;flex-direction:row;flex-wrap:wrap;gap:5px;',
                div(style="display:inline-block;vertical-align:top;margin-top:9px;margin-right:5px;padding-bottom:4px;", uiOutput('page_number_info2')), 
                div(
                  style="display:inline-block;vertical-align:",
                  div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('previous_page2', 'Previous', width = '80px')), 
                  div(
                    style="display:inline-block;vertical-align:",
                    div(style="display:inline-block;vertical-align:top;margin-bottom:-15px;padding-bottom:4px;", uiOutput('page_number_ui2')),
                    div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('go2', 'Search', width = '80px'))
                  ),
                  div(style="display:inline-block;vertical-align:top;padding-bottom:4px;", actionButton('next_page2', 'Next', width = '80px'))   
                )
              )
            )
          ),
          div(
            style = 'height: 200px;'
          )
        )
      ),
      tabPanel(
        title = 'System Key', value = 'key',
        div(
          includeHTML("pages/System Key.html"), 
          br(), 
          br()
        )       
      )
    )
  )
)