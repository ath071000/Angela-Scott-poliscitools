#' Clean political science voter data
#'
#' This function cleans a dataset by handling missing values, ensuring valid party affiliations,
#' and filtering the age range for valid voter data.
#'
#' @param data A data frame containing voter data with columns like `age`, `party`, and `turnout`.
#' @return A cleaned data frame ready for analysis.
#' @export
clean_political_data <- function(data) {

  # 1. Remove rows with missing values (NA)
  data <- na.omit(data)

  # 2. Ensure the age is within a valid range (18 to 90 years)
  data <- data %>%
    filter(age >= 18 & age <= 90)

  # 3. Standardize party names (handling any possible case issues)
  valid_parties <- c("Democrat", "Republican", "Independent")
  data$party <- case_when(
    tolower(data$party) == "democrat" ~ "Democrat",
    tolower(data$party) == "republican" ~ "Republican",
    tolower(data$party) == "independent" ~ "Independent",
    TRUE ~ NA_character_  # Assign NA to any invalid party entries
  )

  # 4. Remove rows with invalid party affiliations
  data <- data %>%
    filter(party %in% valid_parties)

  # 5. Ensure turnout is binary (0 or 1)
  data <- data %>%
    filter(turnout %in% c(0, 1))

  return(data)
}

