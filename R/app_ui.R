#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    shiny::navbarPage(
      theme = bslib::bs_theme(
        fg = "#202123", bg = "white",
        primary = "#009BC9",
        secondary = "#009BC9",
        base_font = sass::font_google("Poppins"),
        "font-size-base" = "0.9rem", "enable-rounded" = FALSE
      ),
      "SnackApp Usage Analysis",
      shiny::tabPanel(
        "Home page",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::h4("SnackApp Usage Data Upload"),
            shiny::req(shiny::fileInput(
              inputId = "filedata",
              label = "Upload SnackApp Usage data:",
              multiple = TRUE,
              accept = c(".csv")
            )),
            shiny::numericInput(
              inputId = "id_start",
              label = "Please select the start position of the ID sequence within the file name (for a default SnackApp file this is
            position 20). Note: The ID must be numeric.
            position 20.",
              value = 20
            ),
            shiny::numericInput(
              inputId = "id_end",
              label = "Please select the end position of the ID sequence within the file name (for a default SnackApp file this is
            position 21). Note: The ID must be numeric.",
              value = 21
            ),
            shiny::h4("Fitbit Usage Data Upload"),
            shiny::fileInput(
              inputId = "fb_usage_data",
              label = "Upload Fitbit Usage data:",
              multiple = TRUE,
              accept = c(".csv")
            ),
            shiny::h4("Fitbit Physical Activity Data Upload"),
            shiny::fileInput("fitbit_pa_dataload",
                      label = "Please upload your Fitbit physical activity data files",
                      multiple = TRUE, accept = c(".csv")
            ),
            shiny::actionButton("run_analysis", "Run SnackApp Usage analysis"),
            shiny::br(),
            shiny::br(),
            shiny::actionButton("run_fb_usage_analysis", "Run Fitbit Usage analysis"),
            shiny::br(),
            shiny::br(),
            shiny::actionButton("run_fb_pa_analysis", "Run Fitbit Physical Activity analysis")
          ),
          shiny::mainPanel(
            shiny::tabsetPanel(
              shiny::tabPanel("SnackApp Usage QAQC",
                              shiny::br(),
                              shiny::fixedRow(
                         shinyWidgets::useShinydashboard(),
                         shinydashboard::infoBox(title = "Number of files uploaded:", value = shiny::uiOutput("n_file"), icon = shiny::icon("file-upload"), fill = TRUE, color = "blue", width = 4),
                         shinydashboard::infoBox(title = "Number of unique ID's:", value = shiny::uiOutput("n_id"), icon = shiny::icon("id-card"), fill = TRUE, color = "blue", width = 4),
                         shinydashboard::infoBox(title = "Number of files failing:", value = shiny::uiOutput("n_fail"), icon = shiny::icon("exclamation"), fill = TRUE, color = "orange", width = 4)
                       ),
                       DT::DTOutput("sa_usage_qaqc")),
              shiny::tabPanel("Fitbit Watch Face QAQC",
                              shiny::br(),
                              shiny::fixedRow(
                         shinyWidgets::useShinydashboard(),
                         shinydashboard::infoBox(title = "Number of files uploaded:", value = shiny::uiOutput("n_file_fb_usage"), icon = shiny::icon("file-upload"), fill = TRUE, color = "blue", width = 4),
                         shinydashboard::infoBox(title = "Number of unique ID's:", value = shiny::uiOutput("n_id_fb_usage"), icon = shiny::icon("id-card"), fill = TRUE, color = "blue", width = 4),
                         shinydashboard::infoBox(title = "Number of files failing:", value = shiny::uiOutput("n_fail_fb_usage"), icon = shiny::icon("exclamation"), fill = TRUE, color = "orange", width = 4)
                       ),
                       DT::DTOutput("fb_usage_qaqc")),
              shiny::tabPanel("Fitbit Physical Activity QAQC",
                              shiny::br(),
                              shiny::fixedRow(
                         shinyWidgets::useShinydashboard(),
                         shinydashboard::infoBox(title = "Number of files uploaded:", value = shiny::uiOutput("n_file_fb_pa"), icon = shiny::icon("file-upload"), fill = TRUE, color = "blue", width = 4),
                         shinydashboard::infoBox(title = "Number of unique ID's:", value = shiny::uiOutput("n_id_fb_pa"), icon = shiny::icon("id-card"), fill = TRUE, color = "blue", width = 4),
                         shinydashboard::infoBox(title = "Number of files failing:", value = shiny::uiOutput("n_fail_fb_pa"), icon = shiny::icon("exclamation"), fill = TRUE, color = "orange", width = 4)
                       ),
                       DT::DTOutput("fb_pa_data_qaqc"))
            )
          )
        )
      ),
      shiny::navbarMenu(
        "SnackApp Usage",
        shiny::tabPanel(
          "SnackApp Usage QAQC",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::h4("QAQC Report"),
              shiny::h6("This is the QAQC page. This page helps to flag potentially spurious data. The sliders below allow you to select a value
             to flag as a minor or major error. The filter data button removes any data flagged as a major error in the data. The
             delete column can be used to remove individual rows of data."),
              shiny::sliderInput(
                inputId = "error1",
                label = "Durations over this value and lower than the value below (in minutes) will be marked as minor errors?",
                min = 0,
                max = 120,
                value = 5,
                dragRange = FALSE
              ),
              shiny::sliderInput(
                inputId = "error2",
                label = "Durations over this value in minutes will be marked as major errors?",
                min = 0,
                max = 120,
                value = 10,
                dragRange = FALSE
              ),
              shiny::downloadButton("qaqc_download", "QAQC Download"),
              shiny::br(),
              shiny::br(),
              shiny::actionButton("delete", label = "Delete Selected Rows")
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                shiny::tabPanel(
                  "SnackApp Usage QAQC",
                  tags$style(shiny::HTML("#qaqc table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}")),
                  shinycssloaders::withSpinner(DT::dataTableOutput("qaqc"),
                              type = 3, color = "#5785AD",
                              color.background = "white"
                  )
                ),
                shiny::tabPanel("SnackApp Usage QAQC Summary", shiny::tableOutput("qaqc_summary")),
                shiny::tabPanel("SnackApp Usage Deleted Rows", shinycssloaders::withSpinner(DT::DTOutput("deleted_rows"),
                                                                    type = 3, color = "#5785AD",
                                                                    color.background = "white"
                ))
              )
            )
          )
        ),
        shiny::tabPanel(
          "SnackApp Usage Summary",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::h4("Summary"),
              shiny::h6("This page produces a daily summary of SnackApp usage. The participant summary produces a one row summary for each
             participant."),
              shiny::selectizeInput("variable_select",
                             label = "Please select variables you wish to view:",
                             choices = c(
                               "total_time", "bouts", "dashboard", "stat", "goal", "profile", "forum", "faq", "planner",
                               "case_studies"
                             ), multiple = TRUE, selected = c("ID", "year", "month", "day", "total_time")
              ),
              shiny::br(),
              shiny::downloadButton("participant_download", "Participant Summary"),
              shiny::br(),
              shiny::h6("The day summary provides a summary for each day the participant used the SnackApp."),
              shiny::br(),
              shiny::downloadButton("day_download", "Day Summary"),
              shiny::br(),
              shiny::h6("The individual summary provides a separate daily summary for each participant."),
              shiny::br(),
              shiny::downloadButton("individual_download", "Individual summary"),
              shiny::br(),
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                shiny::tabPanel("Participant Summary", shinycssloaders::withSpinner(DT::DTOutput("table2"),
                                                            type = 3, color = "#5785AD",
                                                            color.background = "white"
                )),
                shiny::tabPanel("Daily Summary", shinycssloaders::withSpinner(DT::DTOutput("table3"),
                                                      type = 3, color = "#5785AD",
                                                      color.background = "white"
                )),
                shiny::tabPanel("Weekly Summary", shinycssloaders::withSpinner(DT::DTOutput("weekly_summary"),
                                                       type = 3, color = "#5785AD",
                                                       color.background = "white"
                )),
                tags$style(
                  type = "text/css",
                  ".shiny-output-error { visibility: hidden; }",
                  ".shiny-output-error:before { visibility: hidden; }"
                )
              )
            )
          )
        ),
        shiny::tabPanel(
          "SnackApp Usage Visualisation",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::h4("SnackApp Visualisations"),
              shiny::h6("The panels on the right show a range of visualisation of the SnackApp uage data entered into the app.They allow you
             to explore the data entered into the application."),
              shiny::selectInput("id_select", "Please select a participant ID:", choices = NULL),
              shiny::selectInput("metric_select_1", "Please select the first metric to plot:", choices = NULL),
              shiny::selectInput("metric_select_2", "Please select the second metric to plot:", choices = NULL)
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                type = "pills",
                shiny::tabPanel(
                  "Group",
                  shiny::br(),
                  shiny::tabsetPanel(
                    type = "pills",
                    shiny::tabPanel("Box plot", shinycssloaders::withSpinner(plotly::plotlyOutput("group_boxplot", height = 600),
                                                     type = 3, color = "#5785AD",
                                                     color.background = "white"
                    )),
                    shiny::tabPanel("Treemap", shinycssloaders::withSpinner(shiny::plotOutput("group_treemap", height = 600),
                                                    type = 3, color = "#5785AD",
                                                    color.background = "white"
                    )),
                    shiny::tabPanel("Sankey diagram", shinycssloaders::withSpinner(highcharter::highchartOutput("sankey", height = 600),
                                                           type = 3, color = "#5785AD",
                                                           color.background = "white"
                    ))
                  )
                ),
                shiny::br(),
                shiny::tabPanel(
                  "Participant",
                  shiny::tabsetPanel(
                    type = "pills",
                    shiny::tabPanel("Boxplot", shinycssloaders::withSpinner(plotly::plotlyOutput("participant_boxplot", height = 600),
                                                    type = 3, color = "#5785AD",
                                                    color.background = "white"
                    )),
                    shiny::tabPanel("Treemap", shinycssloaders::withSpinner(shiny::plotOutput("participant_treemap", height = 600),
                                                    type = 3, color = "#5785AD",
                                                    color.background = "white"
                    )),
                    shiny::tabPanel("Sankey diagram", shinycssloaders::withSpinner(highcharter::highchartOutput("participant_sankey", height = 600),
                                                           type = 3, color = "#5785AD",
                                                           color.background = "white"
                    )),
                    shiny::tabPanel("Line chart", shinycssloaders::withSpinner(plotly::plotlyOutput("line_chart", height = 600),
                                                       type = 3, color = "#5785AD",
                                                       color.background = "white"
                    ))
                  )
                )
              )
            )
          )
        )
      ),
      shiny::navbarMenu(
        "Fitbit Usage",
        shiny::tabPanel(
          "Fitbit Usage QAQC",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::h4("Fitbit Usage QAQC"),
              shiny::h6("This page allows you to load and perform a data quality assessment on the Fitbit Usage data.")
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                shiny::tabPanel(
                  "Display On QAQC",
                  tags$style(shiny::HTML("#fb_display_on table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}")),
                  shinycssloaders::withSpinner(DT::dataTableOutput("fb_display_on"),
                              type = 3, color = "#5785AD",
                              color.background = "white"
                  )
                ),
                shiny::tabPanel(
                  "Fitbit Nudge Summary", tags$style(shiny::HTML("#fb_nudge table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}")),
                  shinycssloaders::withSpinner(DT::dataTableOutput("fb_nudge"),
                              type = 3, color = "#5785AD",
                              color.background = "white"
                  )
                )
              )
            )
          )
        ),
        shiny::tabPanel(
          "Fitbit Usage Summary",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::h4("Fitbit Usage Summary"),
              shiny::h6("This page generates a summary of the Fitbit Usage data and allows the data to be downloaded in multiple formats: one row per participant (participant summary),
          one row per day the Fitbit was worn (daily summary) and a daily summary with a separate file produced for each participant (individual summary)."),
              shiny::br(),
              shiny::downloadButton("fb_participant_download", "Participant Summary"),
              shiny::br(),
              shiny::br(),
              shiny::downloadButton("fb_daily_download", "Daily Summary"),
              shiny::br(),
              shiny::br(),
              shiny::downloadButton("fb_individual_download", "Individual Summary")
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                shiny::tabPanel("Fitbit Usage Participant Summary", shinycssloaders::withSpinner(DT::DTOutput("fb_ps"),
                                                                         type = 3,
                                                                         color = "#5785AD",
                                                                         color.background = "white"
                )),
                shiny::tabPanel("Fitbit Usage Daily Summary", shinycssloaders::withSpinner(DT::DTOutput("fb_ds"),
                                                                   type = 3,
                                                                   color = "#5785AD",
                                                                   color.background = "white"
                ))
              )
            )
          )
        ),
        shiny::tabPanel(
          "Visualisations",
          shiny::sidebarLayout(
            shiny::sidebarPanel(),
            shiny::mainPanel()
          )
        )
      ),
      shiny::navbarMenu(
        "Fitbit Physical Activity",
        shiny::tabPanel(
          "Fitbit PA QAQC",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::h4("Fitbit PA QAQC"),
              shiny::h6("This page generates a more in depth QAQC of the Fitbit physical activity data.")
            ),
            shiny::mainPanel(shinycssloaders::withSpinner(DT::DTOutput("fb_pa_qaqc"),
                                  type = 3,
                                  color = "#5785AD",
                                  color.background = "white"
            ))
          )
        ),
        shiny::tabPanel(
          "Fitbit PA Summary",
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              shiny::h4("Fitbit PA Summary"),
              shiny::h6("This page shows summary statistics for the Fitbit physical activity data. Summaries are provided for
             each participant, as well as daily for each participant. ")
            ),
            shiny::mainPanel(
              shiny::tabsetPanel(
                shiny::tabPanel("Monthly", shinycssloaders::withSpinner(DT::DTOutput("fb_pa_monthly"),
                                                type = 3,
                                                color = "#5785AD",
                                                color.background = "white"
                )),
                shiny::tabPanel("Weekly", shinycssloaders::withSpinner(DT::DTOutput("fb_pa_weekly"),
                                               type = 3,
                                               color = "#5785AD",
                                               color.background = "white"
                )),
                shiny::tabPanel("Daily", shinycssloaders::withSpinner(DT::DTOutput("fb_pa_daily"),
                                              type = 3,
                                              color = "#5785AD",
                                              color.background = "white"
                ))
              )
            )
          )
        ),
        shiny::tabPanel(
          "Fitbit PA Visualisations",
          shiny::sidebarLayout(
            shiny::sidebarPanel(),
            shiny::mainPanel()
          )
        )
      ),
      tags$footer(
        shiny::fluidRow(
          shiny::column(12,
                 align = "center",
                 shiny::div(style = "display: inline-block;", tags$a(
                   href = "https://www.lboro.ac.uk/",
                   tags$img(
                     src = "https://www.lboro.ac.uk/media/media/research/iprc/loughborough-university-logo.png",
                     title = "Lboro Link",
                     width = "350",
                     height = "135"
                   )
                 )),
                 shiny::div(style = "display: inline-block;", tags$a(
                   href = "https://www.lboro.ac.uk/departments/ssehs/research/research-centres/climb/",
                   tags$img(
                     src = "https://www.lboro.ac.uk/media/wwwlboroacuk/external/content/schoolsanddepartments/ssehs/photos/670x300/78677%20CLiMB%20Logo%20RGB%20A5.png",
                     title = "Climb Link",
                     width = "350",
                     height = "135"
                   )
                 )),
                 shiny::div(style = "display: inline-block;", tags$a(
                   href = "https://www.lboro.ac.uk/news-events/news/2019/may/snacktivity-approach-to-physical-activity/",
                   tags$img(
                     src = "https://pbs.twimg.com/profile_banners/1220693573940715522/1580903982/600x200",
                     title = "Snacktivity Link",
                     width = "350",
                     height = "135"
                   )
                 ))
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'snackapp.usage'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

