#' fb_usage_participant_summary
#' 
#' @description A function to produce a summary per participant of Fitbit usage
#'
#' @param data A dataframe
#'
#' @return A dataframe summarising each participants Fitbit usage (one row per participant)
#' @export
#'
#' @examples
fb_usage_participant_summary <- function(data) {

  data <- data %>%
    dplyr::mutate(
      id = as.numeric(id),
      event_end = dplyr::lead(event_start, 1),
      diff = as.numeric(difftime(event_end, event_start))
    ) %>%
    dplyr::filter(Metric != "body-present")

  summary1 <- data %>%
    dplyr::filter(Metric == "display-on") %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      n_display_on = sum(`Record` == TRUE),
      bouted_n_display_on = sum(`Record` == TRUE & diff >= 5)
    )

  summary2 <- data %>%
    dplyr::filter(Metric == "display-on" & `Record` != FALSE) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      watch_total_time = sum(diff, na.rm = TRUE),
      watch_max_time = max(diff, na.rm = TRUE),
      watch_min_time = min(diff[which(diff > 0)], na.rm = TRUE),
      watch_average_time = mean(diff, na.rm = TRUE)
    )

  summary3 <- data %>%
    dplyr::filter(Metric == "display-on" & `Record` != FALSE & diff >= 5) %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      bouted_watch_total_time = sum(diff, na.rm = TRUE),
      bouted_watch_max_time = max(diff, na.rm = TRUE)
    )
  summary4 <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      n_activity_snack_prompt = sum(Metric == "activity-snack-prompt", na.rm = TRUE),
      n_activity_snack_prompt_heeded = sum(Metric == "nudge-heedology-prompt", na.rm = TRUE)
    )

  summary <- dplyr::inner_join(summary1, summary2) %>%
    dplyr::inner_join(summary3) %>%
    dplyr::inner_join(summary4) %>%
    dplyr::mutate(
      percent_diff_bouted_vs_unbouted = (bouted_watch_total_time / watch_total_time)
    ) %>%
    round(digits = 2)

  summary
}
