#' fb_pa_dataload_qaqc 
#' 
#' @description A function to run QAQC on the Fitbit physical activity data uploaded
#'
#' @param path Path to the folder containing the Fitbit physical actvity data files
#' @param name 
#'
#' @return A QAQC dataframe summarising the files uploaded
#' @export
#'
#' @examples
fb_pa_dataload_qaqc <- function(path, name){
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
    if(length(x[[i]]) == 39){
      length_check[i] <- 1
    } else{
      length_check[i] <- 2
    }
  }

  # check the column names for each dataframe
  for(i in 1:length(x)){
    if(x[[i]][1] == "Date" & x[[i]][2] == "Steps" & x[[i]][4] == "Activity Snacks" & x[[i]][6] == "Inactive Minutes" & x[[i]][7] == "Active Minutes" &
       x[[i]][8] == "Fitbit Active Minutes" & x[[i]][10] == "Calories"){
      colname_check[i] <- 1
    } else{
      colname_check[i] <- 2
    }
  }

  fb_pa_qaqc <- data.frame(name = name) %>%
    cbind(unlist(length_check), unlist(colname_check))
  colnames(fb_pa_qaqc) <- c("name", "length_check", "colname_check")
  return(fb_pa_qaqc)
}
