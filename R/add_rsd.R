#' Calculate Row-wise Relative Standard Deviation (RSD)
#'
#' @description
#' This function calculates the Relative Standard Deviation (RSD) for each row
#' of a dataframe and adds the results as a new column.
#'
#' @param df A dataframe containing numeric values
#' @param decimal_places Integer specifying the number of decimal places (default = 2)
#' @param as_percentage Logical indicating whether to format output as percentage (default = TRUE)
#'
#' @return A dataframe with an additional column named 'RSD' containing the calculated values
#'
#' @examples
#' test_df <- data.frame(
#'   A = c(10, 12, 15, 11, 13),
#'   B = c(20, 22, 19, 21, 23),
#'   C = c(30, 33, 31, 32, 34)
#' )
#' result <- add_rsd(test_df)
#'
#' @export
add_rsd <- function(df, decimal_places = 2, as_percentage = TRUE) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a dataframe")
  }
  
  if (ncol(df) < 2) {
    stop("Dataframe must contain at least 2 columns to calculate RSD")
  }
  
  # Internal RSD calculation function
  calculate_rsd <- function(x) {
    (sd(x) / mean(x)) * 100
  }
  
  # Calculate RSD for each row
  rsd_values <- apply(df, 1, calculate_rsd)
  
  # Format output based on parameters
  if (as_percentage) {
    df$RSD <- sprintf(paste0("%.", decimal_places, "f%%"), rsd_values)
  } else {
    df$RSD <- round(rsd_values, decimal_places)
  }
  
  return(df)
}