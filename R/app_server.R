#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  sa_qaqc <- shiny::reactive({
    dataload_qaqc(input$filedata$datapath, input$filedata$name)
  })
  
  fb_usage_qaqc <- shiny::reactive({
    dataload_qaqc_fb_usage(input$fb_usage_data$datapath, input$fb_usage_data$name)
  })
  
  fbpa_data_qaqc <- shiny::reactive({
    fb_pa_dataload_qaqc(input$fitbit_pa_dataload$datapath, input$fitbit_pa_dataload$name)
  })
  
  rv <- shiny::reactiveValues(
    sankey_data = NULL,
    data = NULL,
    deletedRows = NULL,
    deletedRowsIndices = NULL,
    fb_usage_data = NULL,
    fb_usage_qaqc = NULL,
    fb_participant_summary = NULL,
    fb_daily_summary = NULL,
    fb_pa = NULL,
    fb_pa_qaqc = NULL,
    fb_pa_monthly = NULL,
    fb_pa_weekly = NULL,
    fb_pa_daily = NULL
  )
  
  shiny::observeEvent(input$run_analysis, {
    rv$sankey_data <- load_data(path = input$filedata$datapath, name = input$filedata$name, id_start = input$id_start, id_end = input$id_end) %>%
      dplyr::filter(Event == "viewed-screen" | Event == "app-state-changed")
    
    qaqc <- qaqc(rv$sankey_data)
    
    rv$data <- qaqc
  })
  
  shiny::observeEvent(input$run_analysis, {
    # Clear the previous deletions
    rv$deletedRows <- NULL
    rv$deletedRowIndices <- list()
  })
  
  shiny::observeEvent(input$run_analysis, {
    rv$data$error <- dplyr::case_when(
      rv$data$Event == "app-state-changed" ~ 0,
      rv$data$diff >= input$error2 * 60 ~ 2,
      rv$data$diff >= input$error1 * 60 ~ 1,
      TRUE ~ 0
    )
  })
  
  shiny::observeEvent(input$delete, {
    shiny::showModal(shiny::modalDialog(
      shiny::tagList(
        shiny::renderText(paste("Confirming this will delete", length(input$qaqc_rows_selected), "rows", sep = " "))
      ),
      title = "Are you sure you want to delete the selected rows?",
      footer = shiny::tagList(
        shiny::actionButton("confirm_delete", "Delete"),
        shiny::modalButton("Cancel")
      )
    ))
  })
  
  # consider creating two datasets, one that is original and the deletion always occurs from this data?
  # if that fails, consider whether a rownames (unique) column may help
  # remember to add in the lead further down!
  
  shiny::observeEvent(input$confirm_delete, {
    rows_to_delete <- subset(rv$data, rownames(rv$data) == rownames(rv$data)[input$qaqc_rows_selected])
    rv$deletedRows <- rbind(rv$deletedRows, rows_to_delete)
  })
  
  shiny::observeEvent(input$confirm_delete, {
    rv$data <- dplyr::anti_join(rv$data, rv$deletedRows)
  })
  
  shiny::observeEvent(input$confirm_delete, {
    shiny::removeModal()
  })
  
  qaqc_summary <- shiny::reactive({
    rv$data %>%
      dplyr::count(error)
  })
  
  participant_summary <- shiny::reactive({
    snackapp_usage_p_sum(rv$data)
  })
  
  day_summary <- shiny::reactive({
    snackapp_usage_daily_sum(rv$data)
  })
  
  weekly_summary <- shiny::reactive({
    seven_day_summary(rv$data)
  })
  
  # visualisation code
  
  shiny::observeEvent(rv$data, {
    shiny::updateSelectInput(inputId = "id_select", choices = unique(rv$data$id))
  })
  
  shiny::observeEvent(input$run_analysis, {
    choices <- sort(as.numeric(unique(data()$id))) # setting to as.numeric may break filtering later!!
    metric_1 <- day_summary() %>%
      dplyr::select(contains("total")) %>%
      colnames()
    metric_1 <- metric_1[4:13]
    shiny::updateSelectInput(inputId = "id_select", choices = choices)
    shiny::updateSelectInput(inputId = "metric_select_1", choices = metric_1)
    shiny::updateSelectInput(inputId = "metric_select_2", choices = metric_1)
  })
  
  group_summary_longer <- shiny::reactive({
    participant_summary() %>%
      dplyr::select(id, contains("total")) %>%
      tidyr::pivot_longer(3:11, values_to = "value", names_to = "metric")
  })
  
  day_summary_longer <- shiny::reactive({
    day_summary() %>%
      dplyr::select(id, contains("total")) %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(
        day = dplyr::row_number()
      ) %>%
      dplyr::relocate(id, day) %>%
      tidyr::pivot_longer(5:12, values_to = "value", names_to = "metric") %>%
      dplyr::mutate(
        id = as.factor(id)
      ) %>%
      dplyr::group_by(id, metric) %>%
      dplyr::mutate(
        cumsum = cumsum(value)
      )
  })
  
  participant_boxplot_data <- shiny::reactive({
    day_summary_longer() %>%
      dplyr::filter(id == input$id_select)
  })
  
  treemap_data <- shiny::reactive({
    participant_summary() %>%
      dplyr::select(-total_time) %>%
      tidyr::pivot_longer(contains("total"), values_to = "value", names_to = "metric") %>%
      dplyr::group_by(metric) %>%
      dplyr::summarise(
        average = mean(value)
      )
  })
  
  participant_treemap_data <- shiny::reactive({
    participant_summary() %>%
      dplyr::select(-total_time) %>%
      tidyr::pivot_longer(contains("total"), values_to = "value", names_to = "metric") %>%
      dplyr::group_by(id, metric) %>%
      dplyr::summarise(
        average = mean(value)
      ) %>%
      dplyr::filter(id == input$id_select)
  })
  
  ref <- data.frame(ref = rep(c("dashboard", "profile", "my-goals", "planner", "my-stats", "faq", "forums"), each = 6, times = 1))
  
  ref <- tidyr::crossing(ref, ref, .name_repair = "minimal")
  
  colnames(ref) <- c("source", "target")
  
  ref <- subset(ref, source != target)
  
  nodes <- data.frame(
    names = c(
      "dashboard", "profile", "my-goals", "planner", "my-stats", "faq", "forum",
      "dashboard", "profile", "my-goals", "planner", "my-stats", "faq", "forum",
      "dashboard", "profile", "my-goals", "planner", "my-stats", "faq", "forum",
      "dashboard", "profile", "my-goals", "planner", "my-stats", "faq", "forum",
      "dashboard", "profile", "my-goals", "planner", "my-stats", "faq", "forum"
    ),
    colours = rep(c("blue", "red", "green", "orange", "yellow", "purple", "grey"), each = 1, times = 5)
  )
  
  links <- shiny::reactive({
    create_sankey(data = rv$sankey_data)
  })
  
  participant_links <- shiny::reactive({
    create_participant_sankey(data = rv$sankey_data, participant = input$id_select)
  })
  
  line_chart <- shiny::reactive({
    create_line_chart(
      data = day_summary(), id = input$id_select, metric_1 = input$metric_select_1,
      metric_2 = input$metric_select_2
    )
  })
  
  # fitbit usage data

  shiny::observeEvent(input$run_fb_usage_analysis, {
    rv$fb_usage_data <- load_fb_data(path = input$fb_usage_data$datapath, name = input$fb_usage_data$name)
    rv$fb_usage_qaqc <- fb_usage_full_qaqc(rv$fb_usage_data)
    rv$fb_participant_summary <- fb_usage_participant_summary(data = rv$fb_usage_qaqc)
    rv$fb_daily_summary <- fb_usage_day_summary(data = rv$fb_usage_qaqc)
  })
  
  # fitbit physical activity
  
  shiny::observeEvent(input$run_fb_pa_analysis, {
    rv$fb_pa <- fitbit_pa_load_data(path = input$fitbit_pa_dataload$datapath, name = input$fitbit_pa_dataload$name)
  })
  
  shiny::observeEvent(input$run_fb_pa_analysis, {
    rv$fb_pa_qaqc <- fitbit_pa_qaqc(rv$fb_pa)
  })
  
  shiny::observeEvent(input$run_fb_pa_analysis, {
    rv$fb_pa_monthly <- fitbit_pa_month(rv$fb_pa)
    rv$fb_pa_weekly <- fitbit_pa_week(rv$fb_pa)
    rv$fb_pa_daily <- fitbit_pa_daily(rv$fb_pa)
  })
  
  output$fb_pa_qaqc <- DT::renderDT(
    rv$fb_pa_qaqc
  )
  output$fb_pa_monthly <- DT::renderDT(
    rv$fb_pa_monthly
  )
  output$fb_pa_weekly <- DT::renderDT(
    rv$fb_pa_weekly
  )
  output$fb_pa_daily <- DT::renderDT(
    rv$fb_pa_daily
  )
  
  
  # output rendering
  
  # snackapp usage infoboxes
  
  output$n_file <- shiny::renderText({
    length(input$filedata$datapath)
  })
  
  output$n_id <- shiny::renderText({
    length(unique(rv$data$id))
  })
  
  output$n_fail <- shiny::renderText({
    sa_qaqc() %>%
      dplyr::filter(length_check == 2 | colname_check == 2 | active_background_check == FALSE) %>%
      length()
  })
  
  # fb usage info boxes
  
  output$n_file_fb_usage <- shiny::renderText({
    length(input$fb_usage_data$datapath)
  })
  
  output$n_id_fb_usage <- shiny::renderText({
    length(unique(rv$fb_usage_data$id))
  })
  
  output$n_fail_fb_usage <- shiny::renderText({
    x <- 0
    if (fb_usage_qaqc()$length_check == 2 | fb_usage_qaqc()$colname_check == 2) {
      x <- x + 1
    } else {
      x <- x
    }
    return(x)
  })
  
  # fb pa info boxes
  
  output$n_file_fb_pa <- shiny::renderText({
    length(input$fitbit_pa_dataload$datapath)
  })
  
  output$n_id_fb_pa <- shiny::renderText({
    length(unique(rv$fb_pa$id))
  })
  
  output$n_fail_fb_pa <- shiny::renderText({
    x <- 0
    if (fbpa_data_qaqc()$length_check == 2 | fbpa_data_qaqc()$colname_check == 2) {
      x <- x + 1
    } else {
      x <- x
    }
    return(x)
  })
  
  output$sa_usage_qaqc <- DT::renderDT(
    sa_qaqc() %>%
      dplyr::mutate(
        length_check = dplyr::if_else(length_check == 1, as.character(shiny::icon("ok", lib = "glyphicon")), as.character(shiny::icon("ban-circle", lib = "glyphicon"))),
        colname_check = dplyr::if_else(colname_check == 1, as.character(shiny::icon("ok", lib = "glyphicon")), as.character(shiny::icon("ban-circle", lib = "glyphicon"))),
        active_background_check = dplyr::if_else(active_background_check == TRUE, as.character(shiny::icon("ok", lib = "glyphicon")), as.character(shiny::icon("ban-circle", lib = "glyphicon")))
      ),
    escape = FALSE, options = list(autoWidth = FALSE, scrollX = TRUE)
  )
  
  output$fb_usage_qaqc <- DT::renderDT(
    fb_usage_qaqc() %>%
      dplyr::mutate(
        length_check = dplyr::if_else(length_check == 1, as.character(shiny::icon("ok", lib = "glyphicon")), as.character(shiny::icon("ban-circle", lib = "glyphicon"))),
        colname_check = dplyr::if_else(colname_check == 1, as.character(shiny::icon("ok", lib = "glyphicon")), as.character(shiny::icon("ban-circle", lib = "glyphicon")))
      ),
    escape = FALSE, options = list(autoWidth = FALSE, scrollX = TRUE)
  )
  
  output$fb_pa_data_qaqc <- DT::renderDT(
    fbpa_data_qaqc() %>%
      dplyr::mutate(
        length_check = dplyr::if_else(length_check == 1, as.character(shiny::icon("ok", lib = "glyphicon")), as.character(shiny::icon("ban-circle", lib = "glyphicon"))),
        colname_check = dplyr::if_else(colname_check == 1, as.character(shiny::icon("ok", lib = "glyphicon")), as.character(shiny::icon("ban-circle", lib = "glyphicon")))
      ),
    escape = FALSE, options = list(autoWidth = FALSE, scrollX = TRUE)
  )
  
  output$table <- DT::renderDT(
    data() %>%
      dplyr::mutate(
        date = strftime(date)
      ),
    options = list(
      autoWidth = FALSE, scrollX = TRUE
    )
  )
  output$qaqc <- DT::renderDataTable(
    DT::datatable(rv$data[, -6:-8] %>%
                    dplyr::mutate(
                      date = strftime(date),
                      date_lag = strftime(date_lag)
                    ), options = list(
                      autoWidth = TRUE, scrollX = TRUE
                    ), filter = "top", colnames = c(
                      "Error", "ID", "Event start", "Event end", "Time difference (sec)",
                      "Event", "Metric"
                    )) %>%
      DT::formatStyle(columns = 1, target = "row", backgroundColor = DT::styleEqual(c(1, 2), c("orange", "red")))
  )
  
  output$qaqc_summary <- shiny::renderTable(
    qaqc_summary(),
    options = list(
      autoWidth = TRUE, scrollX = TRUE
    )
  )
  output$deleted_rows <- DT::renderDT(
    rv$deletedRows
  )
  output$table2 <- DT::renderDT(
    participant_summary() %>%
      dplyr::select(id, contains(input$variable_select)),
    options = list(
      autoWidth = FALSE, scrollX = TRUE
    )
  )
  output$table3 <- DT::renderDT(
    day_summary() %>%
      dplyr::select(id, year, month, day, contains(input$variable_select)),
    options = list(
      autoWidth = FALSE, scrollX = TRUE
    )
  )
  
  output$weekly_summary <- DT::renderDT(
    weekly_summary() %>%
      dplyr::select(id, week, contains(input$variable_select)) %>%
      round(digits = 2),
    options = list(
      autoWidth = FALSE, scrollX = TRUE
    )
  )
  
  output$fb_display_on <- DT::renderDataTable(
    DT::datatable(rv$fb_usage_qaqc %>%
                    dplyr::mutate(
                      event_start = strftime(event_start, format = "%Y-%m-%d %H:%M:%OS1")
                    ), options = list(
                      autoWidth = FALSE, scrollX = TRUE
                    ), filter = "top") %>%
      DT::formatStyle(columns = 5, target = "row", backgroundColor = DT::styleEqual(c(TRUE), c("red")))
  )
  
  output$fb_nudge <- DT::renderDataTable(
    DT::datatable(rv$fb_usage_qaqc %>%
                    dplyr::mutate(
                      event_start = strftime(event_start, format = "%Y-%m-%d %H:%M:%OS1")
                    ) %>%
                    dplyr::filter(Metric == "activity-snack-prompt" | Metric == "nudge-heedology-prompt") %>%
                    dplyr::select(-dups), options = list(
                      autoWidth = FALSE, scrollX = TRUE
                    ), filter = "top")
  )
  
  output$fb_ps <- DT::renderDT(
    rv$fb_participant_summary,
    options = list(
      autoWidth = FALSE, scrollX = TRUE
    ),
  )
  
  output$fb_ds <- DT::renderDT(
    rv$fb_daily_summary,
    options = list(
      autoWidth = FALSE, scrollX = TRUE
    ),
  )
  
  # rendering visualisations
  
  output$group_boxplot <- plotly::renderPlotly({
    a <- ggplot2::ggplot(group_summary_longer(), ggplot2::aes(
      x = reorder(metric, value), y = value,
      text = paste0(
        "ID: ", id,
        "<br>Value: ", value
      )
    )) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_point(alpha = 0) +
      ggplot2::scale_y_log10(labels = scales::comma) +
      ggplot2::xlab("") +
      ggplot2::ylab("Log time(sec)") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    
    plotly::style(plotly::ggplotly(a, tooltip = "text"), hoverinfo = "text")
  })
  
  output$group_treemap <- shiny::renderPlot({
    shiny::req(input$run_analysis)
    ggplot2::ggplot(treemap_data(), ggplot2::aes(
      area = log(average), fill = metric,
      label = paste0(metric, "\n", "Value: ", round((average / 60), digits = 2), " mins")
    )) +
      treemapify::geom_treemap(show.legend = FALSE) +
      treemapify::geom_treemap_text(colour = "white") +
      ggplot2::ggtitle("Treemap of average app usage per participant")
  })
  
  output$participant_treemap <- shiny::renderPlot({
    shiny::req(input$run_analysis)
    ggplot2::ggplot(participant_treemap_data(), ggplot2::aes(
      area = log(average), fill = metric,
      label = paste0(metric, "\n", "Value: ", round((average / 60), digits = 2), " mins")
    )) +
      treemapify::geom_treemap(show.legend = FALSE) +
      treemapify::geom_treemap_text(colour = "white") +
      ggplot2::ggtitle("Treemap of average app usage per participant")
  })
  
  output$line_chart <- plotly::renderPlotly({
    ggplot2::ggplot(line_chart(), ggplot2::aes(x = day, y = value, fill = metric)) +
      ggplot2::geom_col(position = "dodge", alpha = 0.6) +
      ggplot2::geom_line(ggplot2::aes(y = roll_mean, colour = metric), size = 2) +
      ggplot2::ylab("Time (sec)")
  })
  
  output$sankey <- highcharter::renderHighchart({
    highcharter::hchart(links(), type = "sankey", highcharter::hcaes(from = from, to = to, weight = weight)) %>%
      highcharter::hc_tooltip(headerFormat = "") %>%
      highcharter::hc_title(text = "Participants flow through the SnackApp") %>%
      highcharter::hc_subtitle(text = "Code available on <a href='https://github.com/jonahthomas/snackapp'>Github</a>") %>%
      highcharter::hc_exporting(enabled = TRUE) %>%
      highcharter::hc_plotOptions(sankey = list(
        chartScrollablePlotArea = TRUE, animation = list(duration = 10),
        nodes = list(
          list(id = "dashboard-1", color = "red"), list(id = "dashboard-2", color = "red"),
          list(id = "dashboard-3", color = "red"), list(id = "dashboard-4", color = "red"),
          list(id = "dashboard-5", color = "red"), list(id = "dashboard-6", color = "red"),
          list(id = "dashboard-7", color = "red"), list(id = "dashboard-8", color = "red"),
          list(id = "dashboard-9", color = "red"), list(id = "dashboard-10", color = "red"),
          list(id = "profile-1", color = "blue"), list(id = "profile-2", color = "blue"),
          list(id = "profile-3", color = "blue"), list(id = "profile-4", color = "blue"),
          list(id = "profile-5", color = "blue"), list(id = "profile-6", color = "blue"),
          list(id = "profile-7", color = "blue"), list(id = "profile-8", color = "blue"),
          list(id = "profile-9", color = "blue"), list(id = "profile-10", color = "blue"),
          list(id = "my-goals-1", color = "yellow"), list(id = "my-goals-2", color = "yellow"),
          list(id = "my-goals-3", color = "yellow"), list(id = "my-goals-4", color = "yellow"),
          list(id = "my-goals-5", color = "yellow"), list(id = "my-goals-6", color = "yellow"),
          list(id = "my-goals-7", color = "yellow"), list(id = "my-goals-8", color = "yellow"),
          list(id = "my-goals-9", color = "yellow"), list(id = "my-goals-10", color = "yellow"),
          list(id = "planner-1", color = "purple"), list(id = "planner-2", color = "purple"),
          list(id = "planner-3", color = "purple"), list(id = "planner-4", color = "purple"),
          list(id = "planner-5", color = "purple"), list(id = "planner-6", color = "purple"),
          list(id = "planner-7", color = "purple"), list(id = "planner-8", color = "purple"),
          list(id = "planner-9", color = "purple"), list(id = "planner-10", color = "purple"),
          list(id = "my-stats-1", color = "green"), list(id = "my-stats-2", color = "green"),
          list(id = "my-stats-3", color = "green"), list(id = "my-stats-4", color = "green"),
          list(id = "my-stats-5", color = "green"), list(id = "my-stats-6", color = "green"),
          list(id = "my-stats-7", color = "green"), list(id = "my-stats-8", color = "green"),
          list(id = "my-stats-9", color = "green"), list(id = "my-stats-10", color = "green"),
          list(id = "faq-1", color = "grey"), list(id = "faq-2", color = "grey"),
          list(id = "faq-3", color = "grey"), list(id = "faq-4", color = "grey"),
          list(id = "faq-5", color = "grey"), list(id = "faq-6", color = "grey"),
          list(id = "faq-7", color = "grey"), list(id = "faq-8", color = "grey"),
          list(id = "faq-9", color = "grey"), list(id = "faq-10", color = "grey"),
          list(id = "forum-1", color = "orange"), list(id = "forum-2", color = "orange"),
          list(id = "forum-3", color = "orange"), list(id = "forum-4", color = "orange"),
          list(id = "forum-5", color = "orange"), list(id = "forum-6", color = "orange"),
          list(id = "forum-7", color = "orange"), list(id = "forum-8", color = "orange"),
          list(id = "forum-9", color = "orange"), list(id = "forum-10", color = "orange")
        )
      ))
  })
  
  output$participant_boxplot <- plotly::renderPlotly({
    b <- ggplot2::ggplot(
      participant_boxplot_data(),
      ggplot2::aes(
        x = reorder(metric, value), y = value,
        text = paste0(
          "ID: ", id,
          "<br>Value: ", value
        )
      )
    ) +
      ggplot2::geom_boxplot() +
      ggplot2::geom_point(alpha = 0) +
      ggplot2::scale_y_log10(labels = scales::comma) +
      ggplot2::xlab("") +
      ggplot2::ylab("Log time(sec)") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    
    plotly::style(plotly::ggplotly(b, tooltip = "text"), hoverinfo = "text")
  })
  
  output$participant_sankey <- highcharter::renderHighchart({
    highcharter::hchart(participant_links(), "sankey", highcharter::hcaes(from = from, to = to, weight = weight)) %>%
      highcharter::hc_tooltip(headerFormat = "") %>%
      highcharter::hc_title(text = "Participants flow through the SnackApp") %>%
      highcharter::hc_subtitle(text = "Code available on <a href='https://github.com/jonahthomas/snackapp'>Github</a>") %>%
      highcharter::hc_exporting(enabled = TRUE) %>%
      highcharter::hc_plotOptions(sankey = list(
        chartScrollablePlotArea = TRUE, animation = list(duration = 10),
        nodes = list(
          list(id = "dashboard-1", color = "red"), list(id = "dashboard-2", color = "red"),
          list(id = "dashboard-3", color = "red"), list(id = "dashboard-4", color = "red"),
          list(id = "dashboard-5", color = "red"), list(id = "dashboard-6", color = "red"),
          list(id = "dashboard-7", color = "red"), list(id = "dashboard-8", color = "red"),
          list(id = "dashboard-9", color = "red"), list(id = "dashboard-10", color = "red"),
          list(id = "profile-1", color = "blue"), list(id = "profile-2", color = "blue"),
          list(id = "profile-3", color = "blue"), list(id = "profile-4", color = "blue"),
          list(id = "profile-5", color = "blue"), list(id = "profile-6", color = "blue"),
          list(id = "profile-7", color = "blue"), list(id = "profile-8", color = "blue"),
          list(id = "profile-9", color = "blue"), list(id = "profile-10", color = "blue"),
          list(id = "my-goals-1", color = "yellow"), list(id = "my-goals-2", color = "yellow"),
          list(id = "my-goals-3", color = "yellow"), list(id = "my-goals-4", color = "yellow"),
          list(id = "my-goals-5", color = "yellow"), list(id = "my-goals-6", color = "yellow"),
          list(id = "my-goals-7", color = "yellow"), list(id = "my-goals-8", color = "yellow"),
          list(id = "my-goals-9", color = "yellow"), list(id = "my-goals-10", color = "yellow"),
          list(id = "planner-1", color = "purple"), list(id = "planner-2", color = "purple"),
          list(id = "planner-3", color = "purple"), list(id = "planner-4", color = "purple"),
          list(id = "planner-5", color = "purple"), list(id = "planner-6", color = "purple"),
          list(id = "planner-7", color = "purple"), list(id = "planner-8", color = "purple"),
          list(id = "planner-9", color = "purple"), list(id = "planner-10", color = "purple"),
          list(id = "my-stats-1", color = "green"), list(id = "my-stats-2", color = "green"),
          list(id = "my-stats-3", color = "green"), list(id = "my-stats-4", color = "green"),
          list(id = "my-stats-5", color = "green"), list(id = "my-stats-6", color = "green"),
          list(id = "my-stats-7", color = "green"), list(id = "my-stats-8", color = "green"),
          list(id = "my-stats-9", color = "green"), list(id = "my-stats-10", color = "green"),
          list(id = "faq-1", color = "grey"), list(id = "faq-2", color = "grey"),
          list(id = "faq-3", color = "grey"), list(id = "faq-4", color = "grey"),
          list(id = "faq-5", color = "grey"), list(id = "faq-6", color = "grey"),
          list(id = "faq-7", color = "grey"), list(id = "faq-8", color = "grey"),
          list(id = "faq-9", color = "grey"), list(id = "faq-10", color = "grey"),
          list(id = "forum-1", color = "orange"), list(id = "forum-2", color = "orange"),
          list(id = "forum-3", color = "orange"), list(id = "forum-4", color = "orange"),
          list(id = "forum-5", color = "orange"), list(id = "forum-6", color = "orange"),
          list(id = "forum-7", color = "orange"), list(id = "forum-8", color = "orange"),
          list(id = "forum-9", color = "orange"), list(id = "forum-10", color = "orange")
        )
      ))
  })
  
  # download handlers
  
  output$qaqc_download <- shiny::downloadHandler(
    filename = function() {
      paste0("qaqc", ".csv")
    },
    content = function(file) {
      write.csv(rv$data, file)
    }
  )
  output$deleted_rows_download <- shiny::downloadHandler(
    filename = function() {
      paste0("deleted_rows", ".csv")
    },
    content = function(file) {
      write.csv(rv$deletedRows, file)
    }
  )
  output$participant_download <- shiny::downloadHandler(
    filename = function() {
      paste0("participant_summary", ".csv")
    },
    content = function(file) {
      write.csv(participant_summary(), file)
    }
  )
  output$day_download <- shiny::downloadHandler(
    filename = function() {
      paste0("day_summary", ".csv")
    },
    content = function(file) {
      write.csv(day_summary(), file)
    }
  )
  output$individual_download <- shiny::downloadHandler(
    filename = function() {
      "individual_download.zip"
    },
    content = function(file) {
      # go to temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      # create list of dataframes and NULL value to store fileNames
      listDataFrames <- shiny::reactive({
        split(day_summary(), f = day_summary()$id)
      })
      allFileNames <- NULL
      
      # loop through each dataframe
      for (i in 1:length(listDataFrames())) {
        # write each dataframe as csv and save fileName
        fileName <- paste0("participant_", names(listDataFrames())[[i]], "_summary.csv")
        write.csv(listDataFrames()[[i]], fileName)
        allFileNames <- c(fileName, allFileNames)
      }
      
      # write the zip file
      zip(file, allFileNames)
    }
  )
  
  output$fb_participant_download <- shiny::downloadHandler(
    filename = function() {
      paste0("fb_participant_summary", ".csv")
    },
    content = function(file) {
      write.csv(rv$fb_participant_summary, file)
    }
  )
  output$fb_daily_download <- shiny::downloadHandler(
    filename = function() {
      paste0("fb_day_summary", ".csv")
    },
    content = function(file) {
      write.csv(rv$fb_daily_summary, file)
    }
  )
  output$fb_individual_download <- shiny::downloadHandler(
    filename = function() {
      "fb_individual_download.zip"
    },
    content = function(file) {
      # go to temp dir to avoid permission issues
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      # create list of dataframes and NULL value to store fileNames
      listDataFrames <- shiny::reactive({
        split(rv$fb_daily_summary, f = rv$fb_daily_summary$id)
      })
      allFileNames <- NULL
      
      # loop through each dataframe
      for (i in 1:length(listDataFrames())) {
        # write each dataframe as csv and save fileName
        fileName <- paste0("participant_", names(listDataFrames())[[i]], "_fb_summary.csv")
        write.csv(listDataFrames()[[i]], fileName)
        allFileNames <- c(fileName, allFileNames)
      }
      
      # write the zip file
      zip(file, allFileNames)
    }
  )
}
