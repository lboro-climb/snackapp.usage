#' dataload_qaqc
#' 
#' @description 
#'
#' @param path Path to folder that contains the data files 
#' @param name 
#'
#' @return A QAQC dataframe summarising the SnackApp usage data uploaded
#' @export
#'
#' @examples
dataload_qaqc <- function(path, name){
  list_data <- path %>%
    purrr::map(vroom::vroom)

  # set empty lists to store results in
  x <- list()
  length_check <- list()
  colname_check <- list()
  active_background_check <- list()
  active_background_position <- list()

  # extract the column names from each dataframe
  for(i in seq_along(list_data)){
    x[i] <- list(colnames(list_data[[i]]))
  }

  # check the length of each series of column names
  for(i in 1:length(x)){
    if(length(x[[i]]) == 5){
      length_check[i] <- 1
    } else{
      length_check[i] <- 2
    }
  }

  # check the column names for each dataframe
  for(i in 1:length(x)){
    if(x[[i]][1] == "Date" & x[[i]][2] == "Event" & x[[i]][3] == "Metric" & x[[i]][4] == "Goal Is Automatic Set" & x[[i]][5] == "Goal Value Set"){
      colname_check[i] <- 1
    } else{
      colname_check[i] <- 2
    }
  }
  
  # check active background
  
  for(i in 1:length(list_data)){
    data <- list_data[[i]] %>%
      dplyr::filter(Event == "app-state-changed" & Metric == "active" | Metric == "background")
    
      if(dplyr::first(data$Metric) == "active") {
        data$metric_check <- rep(c("active", "background"), length.out = nrow(data))
      } else {
        data$metric_check <- rep(c("background", "active"), length.out = nrow(data))
      }
    active_background_check[[i]] <- all(data$Metric == data$metric_check)
    active_background_position[[i]] <- min(which((data$Metric == data$metric_check) == FALSE))
    }

  sa_qaqc <- data.frame(name = name) %>%
    cbind(unlist(length_check), unlist(colname_check), unlist(active_background_check), unlist(active_background_position))
  colnames(sa_qaqc) <- c("name", "length_check", "colname_check", "active_background_check", "active_background_position")
  return(sa_qaqc)
}
