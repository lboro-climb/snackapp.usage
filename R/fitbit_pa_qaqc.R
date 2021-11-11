#' fitbit_pa_qaqc
#' 
#' @description A function to perform QAQC on Fitbit physical activity data
#'
#' @param data A dataframe containing Fitbit physical activity data
#'
#' @return A dataframe containing QAQC information 
#' @export
#'
#' @examples
fitbit_pa_qaqc <- function(data){
  wear <- data %>%
    dplyr::count(`Body Present`)
  return(wear)
}
