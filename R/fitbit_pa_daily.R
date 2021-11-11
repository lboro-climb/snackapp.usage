#' fitbit_pa_daily
#' 
#' @description A function to summarise Fitbit physical activity daily
#'
#' @param data A dataframe
#'
#' @return A dataframe summarising Fitbit physicalactivity metrics daily
#' @export
#'
#' @examples
fitbit_pa_daily <- function(data){
  daily_summary <- data %>%
    dplyr::filter(time == "23:59") %>%
    dplyr::group_by(year, month, week, day) %>%
    dplyr::summarise(
    total_steps = sum(Steps, na.rm = TRUE),
    total_snacks = sum(`Activity Snacks`, na.rm = TRUE),
    total_inactive_minutes = sum(`Inactive Minutes`, na.rm = TRUE),
    total_active_minutes = sum(`Active Minutes`, na.rm = TRUE),
    total_fitbit_active_minutes = sum(`Fitbit Active Minutes`, na.rm = TRUE),
    total_calories = sum(Calories, na.rm = TRUE)
  )
  return(daily_summary)
}
