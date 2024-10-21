#' Analyze voter turnout
#'
#' This function provides a summary of voter turnout, including both counts and percentages
#'
#' @param data A data frame containing voter data, with a `turnout` column that contains binary values (0 or 1)
#' @return A data frame summarizing the turnout with counts and percentages
#' @export
analyze_turnout <- function(data) {

  # Ensure the turnout column contains only 0s and 1s
  data <- data %>%
    filter(turnout %in% c(0, 1))

  # Create a summary table for voter turnout
  turnout_summary <- data %>%
    group_by(turnout) %>%
    summarise(
      count = n(),
      percentage = (n() / nrow(data)) * 100
    )

  # Add descriptive labels for turnout
  turnout_summary$turnout <- ifelse(turnout_summary$turnout == 1, "Voted", "Did Not Vote")

  return(turnout_summary)
}
