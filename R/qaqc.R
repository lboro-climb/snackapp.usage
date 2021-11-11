#' qaqc
#' 
#' @description a function to perform QAQC on the SnackApp usage summary
#'
#' @param data A dataframe of SnackApp usage data
#'
#' @return A QAQC summary of SnackApp usage data
#' @export
#'
#' @examples
qaqc <- function(data){
  qaqc <- data

  qaqc$date <- as.POSIXct(qaqc$Date, tz = "GMT")
  qaqc$Date <- NULL
  qaqc$year <- lubridate::year(qaqc$date)
  qaqc$month <- lubridate::month(qaqc$date)
  qaqc$day <- lubridate::day(qaqc$date)
  qaqc$date_lag <- dplyr::lag(qaqc$date)
  qaqc$diff <- difftime(qaqc$date, qaqc$date_lag, units = "sec") # added a lead by 1 - I think this makes it better? Needs to be checked! If it does work, need to add throughout!
  qaqc$diff[qaqc$diff < 0] <- NA

  qaqc$error <- 0

  qaqc$error[is.na(qaqc$error)] <- 0

  qaqc <- dplyr::relocate(qaqc, error, id, date, date_lag, diff, year, month, day)

  return(qaqc)
}
