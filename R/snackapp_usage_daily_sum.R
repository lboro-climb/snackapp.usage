#' snackapp_usage_daily_sum
#' 
#' @description A function to create a summary of SnackApp usage data daily
#'
#' @param data A dataframe of SnackApp usage data
#'
#' @return A summary dataframe of SnackApp usage data
#' @export
#'
#' @examples
snackapp_usage_daily_sum <- function(data){
  df <- data
  
  # filter out app-state-changed unknown
  
  df <- df %>%
    dplyr::filter(Metric != "unknown")

  state_change_summary <- df %>%
    dplyr::select(id, date, Event, Metric) %>%
    dplyr::group_by(id) %>%
    dplyr::filter(Event == "app-state-changed") %>%
    dplyr::mutate(
      date = as.POSIXct(date),
      date_lag = dplyr::lag(date),
      diff = difftime(date, date_lag, units = "sec"),
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = lubridate::day(date)
    ) %>%
    dplyr::filter(Metric == "background")

  state_change_summary <- state_change_summary %>%
    dplyr::group_by(id, year, month, day) %>%
    dplyr::summarise(
      total_time = as.numeric(sum(diff, na.rm = TRUE)),
      average_time = as.numeric(mean(diff, na.rm = TRUE)),
      std_dev_time = as.numeric(sd(diff, na.rm = TRUE))
    )

  state_change_summary$id <- as.numeric(state_change_summary$id)
  state_change_summary <- round(state_change_summary, digits = 2)


  # add a lead to each _diff column - should not change calculations but will make intermediary products more accurate


  df$my_stat <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "my-stat"), 1, 0))
  df$stat_diff <- dplyr::lead(dplyr::if_else(df$my_stat == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$my_goals <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "my-goal"), 1, 0))
  df$goal_diff <- dplyr::lead(dplyr::if_else(df$my_goals == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$forum <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "forum"), 1, 0))
  df$forum_diff <- dplyr::lead(dplyr::if_else(df$forum == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$case_studies <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "case-studies"), 1, 0))
  df$case_studies_diff <- dplyr::lead(dplyr::if_else(df$case_studies == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$planner <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "planner"), 1, 0))
  df$planner_diff <- dplyr::lead(dplyr::if_else(df$planner == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$case_studies <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "case-studies"), 1, 0))
  df$case_studies_diff <- dplyr::lead(dplyr::if_else(df$case_studies == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$faq <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "faq"), 1, dplyr::if_else(stringr::str_detect(df$Metric, "onboarding"), 0, 0)))
  df$faq_diff <- dplyr::lead(dplyr::if_else(df$faq == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$dashboard <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "dashboard"), 1, 0))
  df$dashboard_diff <- dplyr::lead(dplyr::if_else(df$dashboard == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$profile <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "profile"), 1, 0))
  df$profile_diff <- dplyr::lead(dplyr::if_else(df$profile == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$onboarding <- dplyr::lag(dplyr::if_else(stringr::str_detect(df$Metric, "onboarding"), 1, 0))
  df$onboarding_diff <- dplyr::lead(dplyr::if_else(df$onboarding == 1, as.numeric(difftime(df$date, df$date_lag, units = "sec")), 0), 1)
  
  df$notifications <- dplyr::if_else(stringr::str_detect(df$Event, "notification"), 1, 0)

  df[, 8:29][df[, 8:29] == 0] <- NA

  usage_summary <- df %>%
    dplyr::group_by(id, year, month, day) %>%
    dplyr::summarise(
      total_stat = sum(stat_diff, na.rm = TRUE),
      total_goal = sum(goal_diff, na.rm = TRUE),
      total_forum = sum(forum_diff, na.rm = TRUE),
      total_case_studies = sum(case_studies_diff, na.rm = TRUE),
      total_planner = sum(planner_diff, na.rm = TRUE),
      total_case_studies = sum(case_studies_diff, na.rm = TRUE),
      total_faq = sum(faq_diff, na.rm = TRUE),
      total_dashboard = sum(dashboard_diff, na.rm = TRUE),
      total_profile = sum(profile_diff, na.rm = TRUE),
      total_onboarding = sum(onboarding_diff, na.rm = TRUE),
      average_stat = sum(stat_diff, na.rm = TRUE) / sum((df[cumsum(rle(df$my_stat)$lengths), ]$my_stat == 1) == TRUE, na.rm = TRUE),
      average_goal = sum(goal_diff, na.rm = TRUE) / sum((df[cumsum(rle(df$my_goals)$lengths), ]$my_goals == 1) == TRUE, na.rm = TRUE),
      average_forum = sum(forum_diff, na.rm = TRUE) / sum((df[cumsum(rle(df$forum)$lengths), ]$forum == 1) == TRUE, na.rm = TRUE),
      average_case_studies = sum(case_studies_diff, na.rm = TRUE) / sum((df[cumsum(rle(df$case_studies)$lengths), ]$case_studies == 1) == TRUE, na.rm = TRUE),
      average_planner = sum(planner_diff, na.rm = TRUE) / sum((df[cumsum(rle(df$planner)$lengths), ]$planner == 1) == TRUE, na.rm = TRUE),
      average_faq = sum(faq_diff, na.rm = TRUE) / sum((df[cumsum(rle(df$faq)$lengths), ]$faq == 1) == TRUE, na.rm = TRUE),
      average_dashboard = sum(dashboard_diff, na.rm = TRUE) / sum((df[cumsum(rle(df$dashboard)$lengths), ]$dashboard == 1) == TRUE, na.rm = TRUE),
      average_profile = sum(profile_diff, na.rm = TRUE) / sum((df[cumsum(rle(df$profile)$lengths), ]$profile == 1) == TRUE, na.rm = TRUE),
      average_onboarding = sum(onboarding_diff, na.rm = TRUE) / sum((df[cumsum(rle(df$onboarding)$lengths), ]$onboarding == 1) == TRUE, na.rm = TRUE),
      stdev_stat = sd(stat_diff, na.rm = TRUE),
      stdev_goal = sd(goal_diff, na.rm = TRUE),
      stdev_forum = sd(forum_diff, na.rm = TRUE),
      stdev_case_studies = sd(case_studies_diff, na.rm = TRUE),
      stdev_planner = sd(planner_diff, na.rm = TRUE),
      stdev_case_studies = sd(case_studies_diff, na.rm = TRUE),
      stdev_faq = sd(faq_diff, na.rm = TRUE),
      stdev_dashboard = sd(dashboard_diff, na.rm = TRUE),
      stdev_profile = sd(profile_diff, na.rm = TRUE),
      stdev_onboarding = sd(onboarding_diff, na.rm = TRUE),
      notificaition_count = sum(notifications)
    )

  usage_summary[usage_summary < 0] <- NA
  usage_summary <- do.call(data.frame, lapply(usage_summary, function(x) replace(x, is.infinite(x), NA)))
  usage_summary[is.na(usage_summary)] <- 0
  usage_summary$id <- as.numeric(usage_summary$id)
  usage_summary <- round(usage_summary, digits = 2)

  bouts <- daily_snack_count(df)

  participant_summary <- dplyr::full_join(state_change_summary, usage_summary, by = c("id" = "id", "year" = "year", "month" = "month", "day" = "day"))
  participant_summary <- dplyr::full_join(participant_summary, bouts, by = c("id" = "id", "year" = "year", "month" = "month", "day" = "day"))

  return(participant_summary)
}
