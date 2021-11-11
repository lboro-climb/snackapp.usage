#' fb_usage_full_qaqc
#' 
#' @description A function to run a complete QAQC for Fitbit usage
#'
#' @param data A dataframe
#'
#' @return A dataframe containing full QAQC data on Fitbit usage 
#' @export
#'
#' @examples
fb_usage_full_qaqc <- function(data){
  fb_qaqc <- data %>%
    dplyr::filter(Metric != "body-present")

  rl <- rle(fb_qaqc$Record)
  dups <- rep( rl$lengths != 1 , times = rl$lengths )

  fb_qaqc <- fb_qaqc %>%
    cbind(dups)

  fb_qaqc
}
