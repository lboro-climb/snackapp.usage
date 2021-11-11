#' fitbit_pa_load_data
#' 
#' @description A function to load Fitbit physical activity data
#'
#' @param path The path to the folder containing the Fitbit physical activity data files
#' @param name 
#'
#' @return A dataframe of Fitbit physical activity data
#' @export
#'
#' @examples
fitbit_pa_load_data <- function(path, name){
  data <- path %>%
    purrr::map(vroom::vroom) %>%
    setNames(substr(name, 1, 2)) %>% # need to decide how to set participant ID as not in file name
    dplyr::bind_rows(.id = "id") %>%
    dplyr::mutate(
      Date = as.POSIXct(Date, format = "%d/%m/%Y %H:%M"),
      year = lubridate::year(Date),
      month = lubridate::month(Date),
      week = lubridate::week(Date),
      day = lubridate::day(Date)
    ) %>%
    tidyr::complete(Date = tidyr::full_seq(Date, 60)) %>% # use complete to fill in any missing rows - may need to revisit due to size of datasets and speed
    mutate(
      time = strftime(Date, format = "%H:%M")
    ) %>%
    tidyr::fill(matches(c("Steps", "Activity.Snacks", "Inactive.Minutes", "Active.Minutes", "Fitbit.Active.Minutes", "BMR", "Calories", "Weight",
                 "Watch.MET")), .direction = "down")

  return(data)
}
