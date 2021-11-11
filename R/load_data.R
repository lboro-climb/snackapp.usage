#' load_data
#' 
#' @description A function to load SnackApp usage data 
#'
#' @param path The path to the folder containing SnackApp usage data files
#' @param name 
#' @param id_start The numeric location where the participant ID starts in each data file
#' @param id_end The numeric location where the participant ID ends in each data file
#'
#' @return A dataframe containing SnackApp usage data
#' @export
#'
#' @examples
load_data <- function(path, name, id_start, id_end) {
  data <- path %>%
    purrr::map(vroom::vroom, col_types = list(vroom::col_character(), vroom::col_character(), vroom::col_character(), 
                                              vroom::col_integer(), vroom::col_integer())) %>%
    setNames(substr(name, id_start, id_end))

  Map(function(x) {
    dplyr::mutate(x,
      Date = lubridate::parse_date_time(Date, orders = "YmdHMS")
    )
  }, data)

  data <- data %>%
    dplyr::bind_rows(.id = "id") %>%
    dplyr::select(-contains("goal"))

  data
}
