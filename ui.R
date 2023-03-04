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
          fluidRow(
            style = "margin-top:5px;",
            div(style="display: inline-block;vertical-align:top;", tags$p(style = "margin-top: 10px;margin-right: 5px;", 'Enter a number:')),
            div(style="display: inline-block;vertical-align:top;margin-bottom:-10px;", uiOutput("input_number_ui")),
            div(style="display: inline-block;vertical-align:top;padding-bottom:4px;", actionButton('convert', 'Convert', width = '100px')), 
            div(style="display: inline-block;vertical-align:top;padding-bottom:4px;", actionButton('convert_previous', 'Previous', width = '100px')),
            div(style="display: inline-block;vertical-align:top;padding-bottom:4px;padding-right:40px;", actionButton('convert_next', 'Next', width = '100px')),
            div(
              class = 'dark-theme-toggle',
              style="display: inline-block;vertical-align:top;margin-bottom:0px;margin-bottom:10px;margin-top:10px;", 
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
          uiOutput('phonemes'),
          br(),
          fluidRow(
            div(style="display: inline-block;vertical-align:top; margin-top: 10px; font-size: 15px; margin-right: 10px;margin-bottom:5px;", uiOutput('results_info')),
            div(
              style="display: inline-block;vertical-align:top;font-size: 15px; margin-right: 10px;", 
              div(
                style = '
                    display: flex;
                    flex-direction: row;
                    flex-wrap: wrap;
                    gap: 5px;
                  ',
                div(style = "margin-top: 10px", tags$p('Show')),
                div(style = "margin-bottom: -25px", uiOutput('page_length_ui')),
                div(style = "margin-top: 10px; margin-bottom:20px;", tags$p('results per page.'))
              )
            ),
            div(
              style="display: inline-block;vertical-align:top;font-size: 15px; float: right;", 
              div(
                style = '
                    display: flex;
                    flex-direction: row;
                    flex-wrap: wrap;
                    gap: 5px;
                  ',
                div(style="display: inline-block;vertical-align:top;margin-top: 10px; margin-right: 5px;", uiOutput('page_number_info')), 
                div(style="display: inline-block;vertical-align:top;", actionButton('previous_page', 'Previous', width = '100px')), 
                div(style="display: inline-block;vertical-align:top;margin-bottom: -15px;", uiOutput('page_number_ui')),
                div(style="display: inline-block;vertical-align:top;", actionButton('go', 'Search', width = '120px')), 
                div(style="display: inline-block;vertical-align:top;", actionButton('next_page', 'Next', width = '100px')) 
              )
            )
          ),
          fluidRow(
            style = 'min-height: 370px;',
            DT::DTOutput('table')
          ),
          fluidRow(
            style = 'margin-top: 10px;',
            div(style="display:inline-block;vertical-align:top;margin-top:10px;font-size:15px;margin-right:10px;margin-bottom:5px;", uiOutput('results_info2')),
            div(
              style="display:inline-block;vertical-align:top;font-size:15px;margin-right:10px;", 
              div(
                style = '
                    display: flex;
                    flex-direction: row;
                    flex-wrap: wrap;
                    gap: 5px;
                  ',
                div(style = "margin-top: 10px", tags$p('Show')),
                div(style = "margin-bottom: -25px", uiOutput('page_length_ui2')),
                div(style = "margin-top: 10px; margin-bottom:20px;", tags$p('results per page.'))
              )
            ),
            div(
              style="display: inline-block;vertical-align:top;font-size: 15px; float: right;", 
              div(
                style = '
                    display: flex;
                    flex-direction: row;
                    flex-wrap: wrap;
                    gap: 5px;
                  ',
                div(style="display: inline-block;vertical-align:top;margin-top: 10px; margin-right: 5px;", uiOutput('page_number_info2')), 
                div(style="display: inline-block;vertical-align:top;", actionButton('previous_page2', 'Previous', width = '100px')), 
                div(style="display: inline-block;vertical-align:top;margin-bottom: -15px;", uiOutput('page_number_ui2')),
                div(style="display: inline-block;vertical-align:top;", actionButton('go2', 'Search', width = '120px')), 
                div(style="display: inline-block;vertical-align:top;", actionButton('next_page2', 'Next', width = '100px')) 
              )
            )
          ),
          div(
            style = 'height: 200px;'
          )
        )
      ),
      tabPanel('System Key', value = 'key',
               column(12, includeHTML("pages/System Key.html"), br(), br())       
      )
    )
  )
)