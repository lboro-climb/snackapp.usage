#' load_fb_data
#' 
#' @description A function to load Fitbit usage data
#'
#' @param path The path to the folder containing the Fitbit usage data files
#' @param name 
#'
#' @return A dataframe containing Fitbit usage data
#' @export
#'
#' @examples
load_fb_data <- function(path, name) {
  (path) %>%
    purrr::map(vroom::vroom, col_types = vroom::cols(`Date` = vroom::col_character(), Metric = vroom::col_character(), 
                                                     Record = vroom::col_logical())) %>%
    setNames(substr(name, 22, 23)) %>%
    dplyr::bind_rows(.id = "id") %>%
    dplyr::mutate(
      event_start = lubridate::parse_date_time(Date, orders = "YmdHMOS")
    ) %>%
    dplyr::select(-`Date`) %>%
    dplyr::relocate(id, event_start, Metric, Record)
}
