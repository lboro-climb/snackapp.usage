#' weekly_snack_count
#' 
#' @description A function to generate a weekly summary of SnackApp usage data
#'
#' @param data A dataframe of SnackApp usage data
#'
#' @return A dataframe summarising SnackApp usage weekly
#' @export
#'
#' @examples
weekly_snack_count <- function(data){
  df <- data %>%
    dplyr::group_by(id, week) %>%
    dplyr::filter(Event == "viewed-screen") %>%
    dplyr::group_split()

  total <- data %>% dplyr::group_by(id, week) %>% dplyr::filter(Metric == "background") %>% dplyr::count()
  dashboard <- vector("integer", length(df))
  my_stat <- vector("integer", length(df))
  my_goal <- vector("integer", length(df))
  forum <- vector("integer", length(df))
  case_studies <- vector("integer", length(df))
  faq <- vector("integer", length(df))
  profile <- vector("integer", length(df))
  onboarding <- vector("integer", length(df))
  planner <- vector("integer", length(df))

  for (i in 1:length(df)){
    dashboard[i] <- sum((df[[i]][cumsum(rle(df[[i]]$dashboard)$lengths), ]$dashboard == 1) == TRUE, na.rm = TRUE)
    my_stat[i] <- sum((df[[i]][cumsum(rle(df[[i]]$my_stat)$lengths), ]$my_stat == 1) == TRUE, na.rm = TRUE)
    my_goal[i] <- sum((df[[i]][cumsum(rle(df[[i]]$my_goals)$lengths), ]$my_goals == 1) == TRUE, na.rm = TRUE)
    forum[i] <- sum((df[[i]][cumsum(rle(df[[i]]$forum)$lengths), ]$forum == 1) == TRUE, na.rm = TRUE)
    case_studies[i] <- sum((df[[i]][cumsum(rle(df[[i]]$case_studies)$lengths), ]$case_studies == 1) == TRUE, na.rm = TRUE)
    faq[i] <- sum((df[[i]][cumsum(rle(df[[i]]$faq)$lengths), ]$faq == 1) == TRUE, na.rm = TRUE)
    profile[i] <- sum((df[[i]][cumsum(rle(df[[i]]$profile)$lengths), ]$profile == 1) == TRUE, na.rm = TRUE)
    onboarding[i] <- sum((df[[i]][cumsum(rle(df[[i]]$onboarding)$lengths), ]$onboarding == 1) == TRUE, na.rm = TRUE)
    planner[i] <- sum((df[[i]][cumsum(rle(df[[i]]$planner)$lengths), ]$planner == 1) == TRUE, na.rm = TRUE)
  }

  days <- data %>%
    dplyr::group_by(id, week) %>%
    dplyr::summarise()

  results <- cbind(days, dashboard, my_stat, my_goal, forum, case_studies, faq, profile, onboarding, planner)
  results <- dplyr::full_join(results, total, by = c("id" = "id", "week" = "week"))
  colnames(results) <- c("id", "week", "bouts_dashboard", "bouts_my_stat", "bouts_my_goal", "bouts_forum",
                          "bouts_case_studies", "bouts_faq", "bouts_profile", "bouts_onboarding", "bouts_planner", "bouts_total")

  return(results)
}
