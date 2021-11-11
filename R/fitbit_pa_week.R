#' fitbit_pa_week
#' 
#' @description A function to summarise Fitbit physical activity data weekly
#'
#' @param data A dataframe of Fitbit physical activity data
#'
#' @return Fitbit physical activity data summarised weekly
#' @export
#'
#' @examples
fitbit_pa_week <- function(data){
  weekly_summary <- data %>%
    dplyr::filter(time == "23:59") %>%
    dplyr::group_by(year, month, week) %>%
    dplyr::summarise(
    total_steps = sum(Steps, na.rm = TRUE),
    mean_steps = mean(Steps, na.rm = TRUE),
    total_snacks = sum(`Activity Snacks`, na.rm = TRUE),
    mean_snacks = mean(`Activity Snacks`, na.rm = TRUE),
    total_inactive_minutes = sum(`Inactive Minutes`, na.rm = TRUE),
    mean_inactive_minutes = mean(`Inactive Minutes`, na.rm = TRUE),
    total_active_minutes = sum(`Active Minutes`, na.rm = TRUE),
    mean_active_minutes = mean(`Active Minutes`, na.rm = TRUE),
    total_fitbit_active_minutes = sum(`Fitbit Active Minutes`, na.rm = TRUE),
    mean_fitbit_active_minutes = mean(`Fitbit Active Minutes`, na.rm = TRUE),
    total_calories = sum(Calories, na.rm = TRUE),
    mean_calories = mean(Calories, na.rm = TRUE)
  )
  return(weekly_summary)
}
