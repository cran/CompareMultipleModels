#' @title Finding the Best Column or Model Using Eight Metrics Values
#'
#' @param input_df Data Frame Containing Eight Metrics Values In Different Columns
#' @import CEEMDANML
#' @return
#' \itemize{
#'   \item best_column_results: A list containing four data frames. Details can be found in description section.
#'   }
#' @export
#'
#' @examples
#' set.seed(123) # for reproducibility
#' Y <- rnorm(100, 100, 10)
#' model_1 <-CEEMDANML::carigaan(Y, ratio = 0.8, n_lag = 4)
#' model_2 <- CEEMDANML::carigas(Y, ratio = 0.8, n_lag = 4)
#' model_1_metrics_values <- model_1$Accuracy[,2]
#' model_2_metrics_values <- model_2$Accuracy[,2]
#' combined_results <- data.frame(cbind(model_1_metrics_values, model_2_metrics_values))
#' colnames(combined_results) <- c("model_1", "model_2")
#' best_model <- best_model_finder(input_df = combined_results)
#' @references
#' \itemize{
#'   \item Paul, R. K., & Garai, S. (2021). Performance comparison of wavelets-based machine learning technique for forecasting agricultural commodity prices. Soft Computing, 25(20), 12857-12873.
#'   \item Paul, R. K., & Garai, S. (2022). Wavelets based artificial neural network technique for forecasting agricultural prices. Journal of the Indian Society for Probability and Statistics, 23(1), 47-61.
#'   \item Garai, S., & Paul, R. K. (2023). Development of MCS based-ensemble models using CEEMDAN decomposition and machine intelligence. Intelligent Systems with Applications, 18, 200202.
#'   \item Garai, S., Paul, R. K., Rakshit, D., Yeasin, M., Paul, A. K., Roy, H. S., Barman, S. & Manjunatha, B. (2023). An MRA Based MLR Model for Forecasting Indian Annual Rainfall Using Large Scale Climate Indices. International Journal of Environment and Climate Change, 13(5), 137-150.
#'   \item Garai, S. (2023). Package 'AllMetrics' Type Package Title Calculating Multiple Performance Metrics of a Prediction Model Version 0.1.0, Repository: https://cran.r-project.org/web/packages/AllMetrics/index.html.
#'   }
best_model_finder <- function(input_df) {
  # Create a copy of the input_df to avoid modifying the original
  output_df <- input_df
  # Loop over the first five rows
  for (i in 1:5) {
    # Find the minimum value in the row
    min_value <- min(input_df[i, ])

    # Loop over each column in the row
    for (j in 1:ncol(input_df)) {
      # If the value is the minimum, replace it with 'MIN'
      if (input_df[i, j] == min_value) {
        output_df[i, j] <- 'MIN'
      }
      # Otherwise, replace it with 'NA'
      else {
        output_df[i, j] <- 'NA'
      }
    }
  }

  # Loop over the last three rows
  for (i in (nrow(input_df) - 2):nrow(input_df)) {
    # Find the maximum value in the row
    max_value <- max(input_df[i, ])

    # Loop over each column in the row
    for (j in 1:ncol(input_df)) {
      # If the value is the maximum, replace it with 'MAX'
      if (input_df[i, j] == max_value) {
        output_df[i, j] <- 'MAX'
      }
      # Otherwise, replace it with 'NA'
      else {
        output_df[i, j] <- 'NA'
      }
    }
  }
  metrics <- c("RMSE", "RRMSE", "MAE", "MAPE", "MASE", "NSE", "WI", "LME")
  rownames(output_df) <- metrics
  # Count the number of 'NA' values in each column
  na_counts <- apply(output_df, 2, function(x) sum(x == 'NA'))

  # Find the column(s) with the minimum number of 'NA' values
  min_NA_cols <- names(na_counts)[na_counts == min(na_counts)]

  # If there is more than one column with the same number of 'NA' values
  if (length(min_NA_cols) > 1) {
    # Get the number of 'NA' values for the first five rows in each column
    na_counts_5rows <- apply(output_df[1:5, min_NA_cols], 2, function(x) sum(x == 'NA'))

    # Find the column(s) with the least number of 'NA' values in the first five rows
    min_NA_cols_5rows <- min_NA_cols[na_counts_5rows == min(na_counts_5rows)]

    # Choose the first column from the list of columns with least 'NA' values in the first five rows
    min_NA_col <- min_NA_cols_5rows[1]
  } else {
    # If there is only one column with the minimum number of 'NA' values, choose that column
    min_NA_col <- min_NA_cols[1]
  }
  # Get the values from the input_df for the min_NA_col
  min_NA_values <- input_df[[min_NA_col]]
  BestColumn_metrics <- data.frame(cbind(min_NA_values))
  dim(BestColumn_metrics)
  colnames(BestColumn_metrics) <- min_NA_col
  rownames(BestColumn_metrics) <- metrics
  # Return the column name and its corresponding values
  best_column_results <- list(output_df = output_df, min_NA_col = min_NA_col, min_NA_values = min_NA_values, BestColumn_metrics= BestColumn_metrics)

  return(best_column_results)
}
