#' create_participant_sankey
#' 
#' @description A function to create a highcharter sankey diagram for individual participants
#'
#' @param data A dataframe of SnackApp usage data
#' @param participant The participant ID of the participant to create the sankey diagram for
#'
#' @return A highcharter sankey diagram 
#' @export
#'
#' @examples
create_participant_sankey <- function(data, participant){
  viewed_screen <- data %>%
    dplyr::filter(id == participant) %>%
    dplyr::filter(Event == "viewed-screen" | Event == "app-state-changed") %>%
    dplyr::mutate(
      date = as.POSIXct(Date),
      Metric = dplyr::if_else(stringr::str_detect(Metric, "planner"), "planner", Metric),
      Metric = dplyr::if_else(stringr::word(Metric, sep = "-") == "my", stringr::str_sub(Metric, start = 1, end = 8), 
                              stringr::word(Metric, sep = "-")),
      Goal.Is.Automatic.Set = NULL,
      Goal.Value.Set = NULL,
      active = dplyr::if_else(Metric == "active", 1, 0)
    )

  viewed_screen <- viewed_screen[cumsum(rle(viewed_screen$Metric)$lengths), ]

  list_data <- split(viewed_screen, cumsum(viewed_screen$active == 1))

  flowlines <- lapply(list_data, "[", "Metric")
  flowlines <- purrr::map(flowlines, dplyr::filter, Metric == "dashboard" | Metric == "my-stats" | Metric == "my-goals" | 
                            Metric == "profile" | Metric == "forums" | Metric == "faq" | Metric == "planner")

  flowlines <- purrr::map(flowlines, t)

  flowlines <- lapply(flowlines, `length<-`, 1000) %>% dplyr::bind_rows()

  flowlines <- as.data.frame(t(flowlines))
  colnames(flowlines) <- 1:length(flowlines)

  # create reference dataframe

  ref <- data.frame(ref = rep(c("dashboard", "profile", "my-goals", "planner", "my-stats", "faq", "forums"), each = 6, times = 1))

  ref <- tidyr::crossing(ref, ref, .name_repair = "minimal")

  colnames(ref) <- c("source", "target")

  ref <- subset(ref, source != target)

  # create sankey 1

  sank1 <- flowlines[, 1:2]

  sank1 <- sank1 %>%
    count(`1`, `2`)

  colnames(sank1) <- c("source", "target", "value")

  sank1 <- dplyr::left_join(ref, sank1)

  sank1 <- sank1 %>%
    dplyr::mutate(
      source = dplyr::case_when(
        source == "dashboard" ~ "dashboard-1",
        source == "profile" ~ "profile-1",
        source == "my-goals" ~ "my-goals-1",
        source == "planner" ~ "planner-1",
        source == "my-stats" ~ "my-stats-1",
        source == "faq" ~ "faq-1",
        source == "forums" ~ "forums-1"
      ),
      target = dplyr::case_when(
        target == "dashboard" ~ "dashboard-2",
        target == "profile" ~ "profile-2",
        target == "my-goals" ~ "my-goals-2",
        target == "planner" ~ "planner-2",
        target == "my-stats" ~ "my-stats-2",
        target == "faq" ~ "faq-2",
        target == "forums" ~ "forum-2"
      )
    )

  sank1 <- sank1 %>%
    dplyr::arrange(source, target)

  # create sankey 2

  sank2 <- flowlines[, 2:3]

  sank2 <- sank2 %>%
    dplyr::count(`2`, `3`)

  colnames(sank2) <- c("source", "target", "value")

  sank2 <- dplyr::left_join(ref, sank2)

  sank2 <- sank2 %>%
    dplyr::mutate(
      source = dplyr::case_when(
        source == "dashboard" ~ "dashboard-2",
        source == "profile" ~ "profile-2",
        source == "my-goals" ~ "my-goals-2",
        source == "planner" ~ "planner-2",
        source == "my-stats" ~ "my-stats-2",
        source == "faq" ~ "faq-2",
        source == "forums" ~ "forum-2"
      ),
      target = dplyr::case_when(
        target == "dashboard" ~ "dashboard-3",
        target == "profile" ~ "profile-3",
        target == "my-goals" ~ "my-goals-3",
        target == "planner" ~ "planner-3",
        target == "my-stats" ~ "my-stats-3",
        target == "faq" ~ "faq-3",
        target == "forums" ~ "forum-3"
      )
    )

  sank2 <- sank2 %>%
    dplyr::arrange(source, target)

  # create sankey 3

  sank3 <- flowlines[, 3:4]

  sank3 <- sank3 %>%
    dplyr::count(`3`, `4`)

  colnames(sank3) <- c("source", "target", "value")



  sank3 <- dplyr::left_join(ref, sank3)

  sank3 <- sank3 %>%
    dplyr::mutate(
      source = dplyr::case_when(
        source == "dashboard" ~ "dashboard-3",
        source == "profile" ~ "profile-3",
        source == "my-goals" ~ "my-goals-3",
        source == "planner" ~ "planner-3",
        source == "my-stats" ~ "my-stats-3",
        source == "faq" ~ "faq-3",
        source == "forums" ~ "forum-3"
      ),
      target = dplyr::case_when(
        target == "dashboard" ~ "dashboard-4",
        target == "profile" ~ "profile-4",
        target == "my-goals" ~ "my-goals-4",
        target == "planner" ~ "planner-4",
        target == "my-stats" ~ "my-stats-4",
        target == "faq" ~ "faq-4",
        target == "forums" ~ "forum-4"
      )
    )

  sank3 <- sank3 %>%
    dplyr::arrange(source, target)

  # create sankey 4

  sank4 <- flowlines[, 4:5]

  sank4 <- sank4 %>%
    dplyr::count(`4`, `5`)

  colnames(sank4) <- c("source", "target", "value")

  sank4 <- dplyr::left_join(ref, sank4)

  sank4 <- sank4 %>%
    dplyr::mutate(
      source = dplyr::case_when(
        source == "dashboard" ~ "dashboard-4",
        source == "profile" ~ "profile-4",
        source == "my-goals" ~ "my-goals-4",
        source == "planner" ~ "planner-4",
        source == "my-stats" ~ "my-stats-4",
        source == "faq" ~ "faq-4",
        source == "forums" ~ "forum-4"
      ),
      target = dplyr::case_when(
        target == "dashboard" ~ "dashboard-5",
        target == "profile" ~ "profile-5",
        target == "my-goals" ~ "my-goals-5",
        target == "planner" ~ "planner-5",
        target == "my-stats" ~ "my-stats-5",
        target == "faq" ~ "faq-5",
        target == "forums" ~ "forum-5"
      )
    )

  sank4 <- sank4 %>%
    dplyr::arrange(source, target)

  # create sankey 5

  sank5 <- flowlines[, 5:6]

  sank5 <- sank5 %>%
    dplyr::count(`5`, `6`)

  colnames(sank5) <- c("source", "target", "value")



  sank5 <- dplyr::left_join(ref, sank5)

  sank5 <- sank5 %>%
    dplyr::mutate(
      source = dplyr::case_when(
        source == "dashboard" ~ "dashboard-5",
        source == "profile" ~ "profile-5",
        source == "my-goals" ~ "my-goals-5",
        source == "planner" ~ "planner-5",
        source == "my-stats" ~ "my-stats-5",
        source == "faq" ~ "faq-5",
        source == "forums" ~ "forum-5"
      ),
      target = dplyr::case_when(
        target == "dashboard" ~ "dashboard-6",
        target == "profile" ~ "profile-6",
        target == "my-goals" ~ "my-goals-6",
        target == "planner" ~ "planner-6",
        target == "my-stats" ~ "my-stats-6",
        target == "faq" ~ "faq-6",
        target == "forums" ~ "forum-6"
      )
    )

  sank5 <- sank5 %>%
    dplyr::arrange(source, target)

  # create sankey 6

  sank6 <- flowlines[, 6:7]

  sank6 <- sank6 %>%
    dplyr::count(`6`, `7`)

  colnames(sank6) <- c("source", "target", "value")

  sank6 <- dplyr::left_join(ref, sank6)

  sank6 <- sank6 %>%
    dplyr::mutate(
      source = dplyr::case_when(
        source == "dashboard" ~ "dashboard-6",
        source == "profile" ~ "profile-6",
        source == "my-goals" ~ "my-goals-6",
        source == "planner" ~ "planner-6",
        source == "my-stats" ~ "my-stats-6",
        source == "faq" ~ "faq-6",
        source == "forums" ~ "forum-6"
      ),
      target = dplyr::case_when(
        target == "dashboard" ~ "dashboard-7",
        target == "profile" ~ "profile-7",
        target == "my-goals" ~ "my-goals-7",
        target == "planner" ~ "planner-7",
        target == "my-stats" ~ "my-stats-7",
        target == "faq" ~ "faq-7",
        target == "forums" ~ "forum-7"
      )
    )

  sank6 <- sank6 %>%
    dplyr::arrange(source, target)

  # create sankey 7

  sank7 <- flowlines[, 7:8]

  sank7 <- sank7 %>%
    dplyr::count(`7`, `8`)

  colnames(sank7) <- c("source", "target", "value")



  sank7 <- dplyr::left_join(ref, sank7)

  sank7 <- sank7 %>%
    dplyr::mutate(
      source = dplyr::case_when(
        source == "dashboard" ~ "dashboard-7",
        source == "profile" ~ "profile-7",
        source == "my-goals" ~ "my-goals-7",
        source == "planner" ~ "planner-7",
        source == "my-stats" ~ "my-stats-7",
        source == "faq" ~ "faq-7",
        source == "forums" ~ "forum-7"
      ),
      target = dplyr::case_when(
        target == "dashboard" ~ "dashboard-8",
        target == "profile" ~ "profile-8",
        target == "my-goals" ~ "my-goals-8",
        target == "planner" ~ "planner-8",
        target == "my-stats" ~ "my-stats-8",
        target == "faq" ~ "faq-8",
        target == "forums" ~ "forum-8"
      )
    )

  sank7 <- sank7 %>%
    dplyr::arrange(source, target)

  # create sankey 8

  sank8 <- flowlines[, 8:9]

  sank8 <- sank8 %>%
    dplyr::count(`8`, `9`)

  colnames(sank8) <- c("source", "target", "value")



  sank8 <- dplyr::left_join(ref, sank8)

  sank8 <- sank8 %>%
    dplyr::mutate(
      source = dplyr::case_when(
        source == "dashboard" ~ "dashboard-8",
        source == "profile" ~ "profile-8",
        source == "my-goals" ~ "my-goals-8",
        source == "planner" ~ "planner-8",
        source == "my-stats" ~ "my-stats-8",
        source == "faq" ~ "faq-8",
        source == "forums" ~ "forum-8"
      ),
      target = dplyr::case_when(
        target == "dashboard" ~ "dashboard-9",
        target == "profile" ~ "profile-9",
        target == "my-goals" ~ "my-goals-9",
        target == "planner" ~ "planner-9",
        target == "my-stats" ~ "my-stats-9",
        target == "faq" ~ "faq-9",
        target == "forums" ~ "forum-9"
      )
    )

  sank8 <- sank8 %>%
    dplyr::arrange(source, target)

  # create sankey 9

  sank9 <- flowlines[, 9:10]

  sank9 <- sank9 %>%
    dplyr::count(`9`, `10`)

  colnames(sank9) <- c("source", "target", "value")

  sank9 <- dplyr::left_join(ref, sank9)

  sank9 <- sank9 %>%
    dplyr::mutate(
      source = dplyr::case_when(
        source == "dashboard" ~ "dashboard-9",
        source == "profile" ~ "profile-9",
        source == "my-goals" ~ "my-goals-9",
        source == "planner" ~ "planner-9",
        source == "my-stats" ~ "my-stats-9",
        source == "faq" ~ "faq-9",
        source == "forums" ~ "forum-9"
      ),
      target = dplyr::case_when(
        target == "dashboard" ~ "dashboard-10",
        target == "profile" ~ "profile-10",
        target == "my-goals" ~ "my-goals-10",
        target == "planner" ~ "planner-10",
        target == "my-stats" ~ "my-stats-10",
        target == "faq" ~ "faq-10",
        target == "forums" ~ "forum-10"
      )
    )

  sank9 <- sank9 %>%
    dplyr::arrange(source, target)

  # creation of sankey diagram

  sank <- rbind(sank1, sank2, sank3, sank4, sank5, sank6, sank7, sank8, sank9)

  colnames(sank) <- c("from", "to", "weight")

  sank
}
