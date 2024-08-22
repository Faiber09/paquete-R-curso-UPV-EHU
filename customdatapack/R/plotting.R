# =============================================================
# plot_entropy
# =============================================================

#' Function to visualize the normalized entropy for each factor column
#'
#' @description This function calculates the normalized entropy for each factor column in an object from CustomData and visualizes the results using a bar plot.
#' @param custom_data A data frame where at least one column is factor
#' @details This function does not require extra packages
#' @return A list containing a message confirming the plot generation, a vector of normalized entropies, and the names of the columns.
#' @examples
#' df <- data.frame(a = factor(c(rep('a', 80), rep('b', 20))), 
#'                  b = factor(c(rep('x', 50), rep('y', 50))),
#'                  c = factor(c(rep('k', 70), rep('l', 30))))
#' obj <- customdata(df)                 
#' plot_entropy(obj)
#' 

plot_entropy <- function(custom_data) {
  # Ensure input is of class CustomData
  if (!inherits(custom_data, "CustomData")) {
    stop("Input must be a CustomData object.")
  }
  
  # Calculate entropy values for factor columns
  entropy_values <- applyEntropy(custom_data)
  entropy_values <- entropy_values@data
  
  # Check if entropy_values is empty
  if (nrow(entropy_values) == 0) {
    stop("No factor columns found. Cannot generate plot.")
  }
  
  # Extract normalized entropy values
  norm_entropy <- entropy_values$norm_entropy
  
  # Extract column names (which correspond to factor columns)
  column_names <- rownames(entropy_values)
  
  # Ensure norm_entropy is a numeric vector
  norm_entropy <- as.numeric(norm_entropy)
  
  # Plot the results using barplot
  gr <- graphics::barplot(norm_entropy, names.arg = column_names, xlab = "Columns", ylab = "Normalized Entropy", main = "Normalized Entropy by Column", las = 2)
  
  # Return the plot
  return(gr)
}

# =============================================================
# plot ROC curve from rocpoints
# =============================================================

#' Plot ROC Curve
#'
#' @description This function plots the ROC curve given the ROC points and optionally displays the AUC value.
#' @param roc_points A matrix where each row represents a point on the ROC curve, with columns for False Positive Rate (FPR) and True Positive Rate (TPR).
#' @param auc Optional; the Area Under the Curve (AUC) value to be displayed on the plot.
#' @return No return value; the function plots the ROC curve.
#' @examples
#' df <- data.frame(prob = c(0.1, 0.4, 0.35, 0.8), label = c(0, 1, 0, 1))
#' result <- rocAuc(df, "prob", "label")
#' plot_roc_curve(result$roc_points, result$auc)
#' @seealso \code{\link{rocAuc}} for extracting ROC points and calculate AUC.
#' @export
#'
plot_roc_curve <- function(roc_points, auc = NULL) {
  # Create the plot
  plot(roc_points[, 1], roc_points[, 2], type = "n", xlab = "False Positive Rate", ylab = "True Positive Rate", main = "ROC Curve", xlim = c(0, 1), ylim = c(0, 1))
  lines(roc_points[, 1], roc_points[, 2], col = "blue", lwd = 2)
  points(roc_points[, 1], roc_points[, 2], col = "red", pch = 19)
  
  # Add a diagonal line representing random prediction
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # Display the AUC on the plot if provided
  if (!is.null(auc) && !is.nan(auc)) {
    text(0.6, 0.2, paste("AUC =", round(auc, 2)), col = "black")
  }
}

# =============================================================
# plot ROC curve from data.frame or CustomData
# =============================================================

#' Plot ROC Curve from DataFrame or CustomData
#'
#' @description This function calculates ROC AUC and plots the ROC curve for a given data frame or CustomData object.
#' @param data An object of class \code{\linkS4class{CustomData}} or a data frame containing probability and binary label columns.
#' @param prob_col The name of the column in the data frame or CustomData object that contains probability values.
#' @param label_col The name of the column in the data frame or CustomData object that contains the binary labels.
#' @return No return value; the function plots the ROC curve.
#' @examples
#' df <- data.frame(prob = c(0.1, 0.4, 0.35, 0.8), label = c(0, 1, 0, 1))
#' plot_roc(df, "prob", "label")
#' @seealso \code{\link{rocAuc}} for extracting ROC points and calculate AUC, and \code{\link{plot_roc_curve}} for plotting the roc curve from the roc points
#' @export
#'
plot_roc <- function(data, prob_col, label_col) {
  if (is(data, "CustomData")) {
    # Extract the data frame from CustomData
    df <- data@data
  } else if (is.data.frame(data)) {
    # Use the provided data frame
    df <- data
  } else {
    stop("Input must be a data frame or an object of class CustomData.")
  }
  
  # Calculate ROC AUC
  result <- rocAuc(df, prob_col, label_col)
  
  # Plot ROC curve
  plot_roc_curve(result$roc_points, result$auc)
}

# =============================================================
# plot correlation matrix from numeric columns
# =============================================================

#' Plot Correlation Matrix using ggplot2
#'
#' @description This function plots a correlation matrix using ggplot2 for either a data frame or a CustomData object.
#' @param data Either a data frame or a CustomData object containing continuous variables.
#' @details
#' The function filters out non-numeric columns before calculating the correlation matrix. It calculates the correlation matrix and converts it to long format for plotting. The plot displays the correlation values with a color gradient and labels.
#' The function requires the ggplot2 and reshape2 packages to be installed.
#' @return A ggplot2 object representing the correlation matrix.
#' @examples
#' df <- data.frame(a = rnorm(100), b = rnorm(100), c = rnorm(100), d = as.factor(sample(letters, 100, replace = TRUE)))
#' plotCorrMatrix(df)
#' 
#' # Example with CustomData
#' custom_data <- customdata(df)
#' plotCorrMatrix(custom_data)
#' @export
plotCorrMatrix <- function(data) {
  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("reshape2", quietly = TRUE)) {
    stop("The function 'plotCorrMatrix' requires the ggplot2 and reshape2 packages. Please install these packages and try again.")
  }
  
  # Check if input is a data frame or CustomData object
  if (!(is.data.frame(data) || inherits(data, "CustomData"))) {
    stop("Input must be a data frame or an object of the class CustomData.")
  }
  
  # Extract the data frame if the input is a CustomData object
  if (inherits(data, "CustomData")) {
    data <- data@data
  }
  
  # Filter out non-numeric columns
  numeric_data <- data[sapply(data, is.numeric)]
  
  # Ensure that there are numeric columns left after filtering
  if (ncol(numeric_data) == 0) {
    stop("No numeric columns found in the data.")
  }
  
  # Calculate the correlation matrix
  cor_matrix <- cor(numeric_data)
  
  # Convert the correlation matrix to long format for plotting
  cor_melt <- reshape2::melt(cor_matrix)
  
  # Create the correlation matrix plot using ggplot2
  ggplot2::ggplot(cor_melt, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", value)), vjust = 1) +
    ggplot2::labs(x = "Variables", y = "Variables", fill = "Correlation") +
    ggplot2::ggtitle("Correlation Matrix") +
    ggplot2::theme_bw() + 
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

