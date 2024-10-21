#' Visualize party distribution
#'
#' This function creates a bar plot showing the distribution of party affiliations.
#'
#' @param data A data frame containing party affiliation data, with a `party` column.
#' @return A ggplot object representing the party distribution.
#' @export
visualize_party_distribution <- function(data) {

  # Create the bar plot with custom colors for each party
  ggplot(data, aes(x = party, fill = party)) +
    geom_bar() +
    scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red", "Independent" = "green")) +
    labs(
      title = "Distribution of Party Affiliations",
      x = "Political Party",
      y = "Count"
    ) +
    theme_minimal()  # Use a clean theme for the plot
}
