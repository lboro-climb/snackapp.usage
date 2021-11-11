#' dataload_qaqc_fb_usage
#'
#' @param path path to folder where data files are stored
#' @param name 
#'
#' @return A QAQC dataframe summarising the Fitbit usage data files uploaded
#' @export
#'
#' @examples
dataload_qaqc_fb_usage <- function(path, name){
  list_data <- path %>%
    purrr::map(vroom::vroom)

  # set empty lists to store results in
  x <- list()
  length_check <- list()
  colname_check <- list()

  # extract the column names from each dataframe
  for(i in seq_along(list_data)){
    x[i] <- list(colnames(list_data[[i]]))
  }

  # check the length of each series of column names
  for(i in 1:length(x)){
    if(length(x[[i]]) == 3){
      length_check[i] <- 1
    } else{
      length_check[i] <- 2
    }
  }

  # check the column names for each dataframe
  for(i in 1:length(x)){
    if(x[[i]][1] == "Date" & x[[i]][2] == "Metric" & x[[i]][3] == "Record"){
      colname_check[i] <- 1
    } else{
      colname_check[i] <- 2
    }
  }

  fb_qaqc <- data.frame(name = name) %>%
    cbind(unlist(length_check), unlist(colname_check))
  colnames(fb_qaqc) <- c("name", "length_check", "colname_check")
  return(fb_qaqc)
}
