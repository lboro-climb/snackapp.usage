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
  background_inactive_swap <- list()

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
  
  # background inactive swap
  
  for(i in 1:length(list_data)) {
    data <- list_data[[i]] %>%
      dplyr::filter(Event == "app-state-changed") %>%
      dplyr::select(Metric) %>%
      dplyr::mutate(
        background_flag = dplyr::lag(dplyr::if_else(Metric == "background", 1, 0)),
        inactive_flag = dplyr::if_else(Metric == "inactive", 1, 0),          
        error = dplyr::if_else(background_flag == 1 & inactive_flag == 1, 1, 0)
      ) 
      
     background_inactive_swap <- sum(data$error, na.rm = TRUE)
      
  }

  sa_qaqc <- data.frame(name = name) %>%
    cbind(unlist(length_check), unlist(colname_check), unlist(active_background_check), unlist(active_background_position), unlist(background_inactive_swap))
  colnames(sa_qaqc) <- c("name", "length_check", "colname_check", "active_background_check", "active_background_position", "background_inactive_swap")
  return(sa_qaqc)
}
