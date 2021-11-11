#' create_line_chart
#' 
#' @description create a rolling mean line chart
#'
#' @param data A dataframe to create the line chart from
#' @param id_selected Participant ID selected to filter the line chart by
#' @param metric_1 The first metric to show in the line chart
#' @param metric_2 The second metric to show in the line chart
#'
#' @return a ggplot line chart
#' @export
#'
#' @examples
create_line_chart <- function(data, id_selected, metric_1, metric_2){
  data %>%
    dplyr::select(id, year, month, day, dplyr::contains("total")) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      day = dplyr::row_number()
    ) %>%
    tidyr::pivot_longer(5:14, names_to = "metric", values_to = "value") %>%
    dplyr::ungroup() %>%
    dplyr::filter(metric == metric_1 | metric == metric_2) %>%
    dplyr::group_by(id, metric) %>%
    dplyr::mutate(
      roll_mean = zoo::rollmean(value, 10, fill = c("extend", "extend", "extend"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(id == id_selected)
}
