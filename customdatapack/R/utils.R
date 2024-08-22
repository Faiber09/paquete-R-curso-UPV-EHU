# =============================================================
# normalize method
# =============================================================

#define normalize as a generic function
setGeneric(
  name = "normalize",
  def = function(x) standardGeneric("normalize")
)

# Define the normalization function
# This rescales values from 0 to 1
v.norm <- function(v) {
  if (length(unique(v)) == 1) {
    return(v)  # Return the original column if it has only one unique value
  }
  return((v - min(v)) / (max(v) - min(v)))
}

#' Method to normalize numeric columns of CustomData
#'
#' @description This method normalizes numeric columns in a \code{\linkS4class{CustomData}} object.
#' @param x An object of class \code{\linkS4class{CustomData}}.
#' @return A new \code{\linkS4class{CustomData}} object with normalized numeric columns.
#' @details The normalization is applied only to numeric columns within the data frame of the \code{CustomData} object. Non-numeric columns remain unchanged. Numeric columns with only one unique value are left unchanged.
#' @export
#' @examples
#' # Create a CustomData object
#' df <- data.frame(a = 1:5, b = c(5, 5, 5, 5, 5), c = 10:14)
#' obj <- customdata(df)
#' 
#' # Normalize numeric columns
#' normalized_obj <- normalize(obj)
#' print(normalized_obj)
#' @seealso \code{\link{customdata}}
#'

setMethod(
  f = "normalize",
  signature = "CustomData",
  definition = function(x) {
    # Get the data frame from CustomData
    data <- x@data
    
    # Apply normalization only to numeric columns
    numeric_cols <- sapply(data, is.numeric)
    data[numeric_cols] <- lapply(data[numeric_cols], v.norm)
    
    # Return a new CustomData object with normalized data
    return(new("CustomData", data = data))
  }
)

##=============================================================
## standardize method
##=============================================================
setGeneric(
  name = "standardize",
  def = function(x) standardGeneric("standardize")
)

# Define the standardization function
# This standardizes values to have mean 0 and standard deviation 1
v.est <- function(v) {
  # Remove NA values if any
  v <- na.omit(v)
  # Check if there is only one unique value
  if (length(unique(v)) <= 1) {
    return(v)  # Return the original column if it has only one unique value or no variation
  }
  # Calculate the standard deviation
  sd_v <- sd(v)
  # Handle the case where sd_v is NA or zero
  if (is.na(sd_v) || sd_v == 0) {
    return(v)  # Return the original column if standard deviation is NA or zero
  }
  
  return((v - mean(v)) / sd_v)
}

#' Method to standardize numeric columns of CustomData
#'
#' @description This method standardizes numeric columns in a \code{\linkS4class{CustomData}} object.
#' @param x An object of class \code{\linkS4class{CustomData}}. 
#' @return A new \code{\linkS4class{CustomData}} object with standardized numeric columns.
#' @details The standardization is applied only to numeric columns within the data frame of the \code{CustomData} object. Non-numeric columns remain unchanged. Numeric columns with zero or NA standard deviation are left unchanged.
#' @export
#' @examples
#' # Create a CustomData object
#' df <- data.frame(a = 1:5, b = c(5, 5, 5, 5, 5), c = 10:14)
#' obj <- customdata(df)
#' 
#' # Standardize numeric columns
#' standardized_obj <- standardize(obj)
#' print(standardized_obj)
#' 
#' @seealso \code{\link{customdata}}
#' 
setMethod(
  f = "standardize",
  signature = "CustomData",
  definition = function(x) {
    # Get the data frame from CustomData
    data <- x@data
    
    # Apply standardization only to numeric columns
    numeric_cols <- sapply(data, is.numeric)
    data[numeric_cols] <- lapply(data[numeric_cols], v.est)
    
    # Return a new CustomData object with standardized data
    return(new("CustomData", data = data))
  }
)

##=============================================================
## Discretize EF Function
##=============================================================

# Function to discretize a numeric vector into equal frequency bins
discretizeEF <- function(x, num.bins) {
  # Check that x is numeric
  if (!is.numeric(x) || !is.vector(x)) {
    stop("x must be a numeric vector.")
  }
  
  # Check that num.bins is a positive integer
  if (!is.numeric(num.bins) || floor(num.bins) != num.bins || num.bins <= 0) {
    stop("num.bins must be a positive integer.")
  }
  
  # Calculate cut points using quantiles
  quantiles <- seq(0, 1, length.out = num.bins + 1)
  cut_points <- quantile(x, probs = quantiles, na.rm = TRUE)
  
  # Replace the first and last cut points with -Inf and Inf
  cut_points[1] <- -Inf
  cut_points[length(cut_points)] <- Inf
  
  # Initialize the interval labels
  interval_labels <- paste0("(", head(cut_points, -1), ", ", tail(cut_points, -1), "]")
  
  # Assign each value to its corresponding interval
  categories <- cut(x, breaks = cut_points, labels = interval_labels, include.lowest = TRUE, right = TRUE)
  
  result <- list(x.discretized = categories, cut.points = cut_points)
  return(result)
}

# Define the function to apply discretizeEF to numeric columns

#' Apply Discretization by Equal Frequency to Numeric Columns of CustomData
#'
#' @description This function applies discretization by equal frequency to the numeric columns of a \code{\linkS4class{CustomData}} object. It discretizes each numeric column into a specified number of bins.
#' @param custom_data An object of class \code{\linkS4class{CustomData}}. This object contains a data frame with columns to be discretized.
#' @param num.bins An integer specifying the number of bins for discretization. Each numeric column will be divided into this number of equal frequency intervals.
#' @return A new \code{\linkS4class{CustomData}} object with discretized numeric columns. The discretized columns will replace the original numeric columns, while non-numeric columns remain unchanged.
#' @details The resulting data frame has the same column order as the original data frame, with numeric columns replaced by their discretized versions.
#' @export
#' @examples
#' # Create a CustomData object
#' df <- data.frame(a = rnorm(100), b = rnorm(100), c = letters[1:100])
#' obj <- customdata(df)
#' 
#' # Apply discretization to numeric columns
#' discretized_obj <- applyDiscretizeEF(obj, num.bins = 4)
#' print(discretized_obj)
#' 

applyDiscretizeEF <- function(custom_data, num.bins) {
  # Check that custom_data is an object of class CustomData
  if (!is(custom_data, "CustomData")) {
    stop("custom_data must be an object of class CustomData.")
  }
  data <- custom_data@data
  # Identify numeric columns in the data frame
  numeric_cols <- sapply(data, is.numeric)
  # Discretize each numeric column using discretizeEF
  discretized_data <- lapply(names(data)[numeric_cols], function(col_name) {
    discretize_result <- discretizeEF(data[[col_name]], num.bins)
    discretize_result$x.discretized
  })
  # Convert the list of discretized columns into a data frame
  discretized_df <- as.data.frame(discretized_data)
  # Assign original column names to the discretized data frame
  colnames(discretized_df) <- names(data)[numeric_cols]
  # Combine discretized numeric columns with non-numeric columns
  result_data <- cbind(discretized_df, data[!numeric_cols])
  # Reorder columns to match the original order
  result_data <- result_data[, names(data)]
  # Return a new CustomData object with the updated data frame
  return(new("CustomData", data = result_data))
}

##=============================================================
## Discretize EW Function
##=============================================================

# Function to discretize a numeric vector into equal width bins
discretizeEW <- function(x, num.bins) {
  # Check that x is numeric  
  if (!is.numeric(x) || !is.vector(x)) {
    stop("x must be a numeric vector.")
  }
  
  # Check that num.bins is a positive integer
  if (!is.numeric(num.bins) || floor(num.bins) != num.bins || num.bins <= 0) {
    stop("num.bins must be a positive integer.")
  }
  
  # Generate cut points
  cut.points <- seq(min(x), max(x), length.out = num.bins + 1)
  
  # Replace the first and last cut points with -Inf and Inf
  cut.points <- cut.points[-c(1, length(cut.points))]
  cut.points <- c(-Inf, cut.points, Inf)
  
  # Create interval labels
  interval_labels <- paste0("(", head(cut.points, -1), ", ", tail(cut.points, -1), "]")
  
  # Use cut to assign each value to its corresponding interval
  categories <- cut(x, breaks = cut.points, labels = interval_labels, include.lowest = TRUE, right = TRUE)
  
  result <- list(x.discretized = categories, cut.points = cut.points)
  return(result)
}

# Define the function to apply discretizeEW to numeric columns

#' Apply Discretization by Equal Width to Numeric Columns of CustomData
#'
#' @description This function applies discretization by equal width to the numeric columns of a \code{\linkS4class{CustomData}} object. 
#' It discretizes each numeric column into a specified number of bins.
#' @param custom_data An object of class \code{\linkS4class{CustomData}}. This object contains a data frame with columns to be discretized.
#' @param num.bins An integer specifying the number of bins for discretization. Each numeric column will be divided into this number of equal width intervals.
#' @return A new \code{\linkS4class{CustomData}} object with discretized numeric columns. The discretized columns will replace the original numeric columns, while non-numeric columns remain unchanged.
#' @details The resulting data frame has the same column order as the original data frame, with numeric columns replaced by their discretized versions.
#' @export
#' @examples
#' # Create a CustomData object
#' df <- data.frame(a = rnorm(100), b = rnorm(100), c = letters[1:100])
#' obj <- customdata(df)
#' 
#' # Apply discretization to numeric columns
#' discretized_obj <- applyDiscretizeEW(obj, num.bins = 4)
#' print(discretized_obj)
#' 
applyDiscretizeEW <- function(custom_data, num.bins) {
  # Check that custom_data is an object of class CustomData
  if (!is(custom_data, "CustomData")) {
    stop("custom_data must be an object of class CustomData.")
  }
  data <- custom_data@data
  # Identify numeric columns in the data frame
  numeric_cols <- sapply(data, is.numeric)
  # Discretize each numeric column using discretizeEW
  discretized_data <- lapply(names(data)[numeric_cols], function(col_name) {
    discretize_result <- discretizeEW(data[[col_name]], num.bins)
    discretize_result$x.discretized
  })
  # Convert the list of discretized columns into a data frame
  discretized_df <- as.data.frame(discretized_data)
  # Assign original column names to the discretized data frame
  colnames(discretized_df) <- names(data)[numeric_cols]
  # Combine discretized numeric columns with non-numeric columns
  result_data <- cbind(discretized_df, data[!numeric_cols])
  # Reorder columns to match the original order
  result_data <- result_data[, names(data)]
  # Return a new CustomData object with the updated data frame
  return(new("CustomData", data = result_data))
}

##=============================================================
## Calculate Entropy for factor columns
##=============================================================

# Function to calculate the entropy and normalized entropy of a factor
entropy <- function(x) {
  # Verify that x is a factor
  if (!is.factor(x)) {
    stop("x debe ser un factor.")
  }
  
  # Create a frequency table and convert it to a data frame
  df <- as.data.frame(table(x))
  
  # Number of levels in x
  niveles <- nlevels(x)
  
  # If there is only one level, return 0 for both entropy and normalized entropy
  if (niveles == 1) {
    return(list('entropy_value' = 0, 'norm_entropy' = 0))
  }
  
  # Calculate probabilities and add to a new column 'prob'
  total <- sum(df$Freq)
  df$prob <- df$Freq / total
  
  # Apply entropy formula
  entropy_value <- -sum(df$prob * log2(df$prob))
  
  # Normalize entropy by dividing by log2(niveles)
  norm_entropy <- entropy_value / log2(niveles)
  
  salida <- list('entropy_value' = entropy_value, 'norm_entropy' = norm_entropy)
  return(salida)
}

# Define the function to apply entropy to factor columns of CustomData

#' Calculate Entropy for Factor Columns of CustomData
#'
#' @description This function calculates the entropy and normalized entropy for each factor column of a \code{\linkS4class{CustomData}} object.
#' @param custom_data An object of class \code{\linkS4class{CustomData}}. This object contains a data frame with columns to be processed.
#' @return A CustomData object with the entropy and normalized entropy values for each factor column. The columns of the returned data frame are 'entropy_value' and 'norm_entropy'.
#' @details Only factor columns are considered. If no factor columns are present, the function returns an empty data frame.
#' @export
#' @examples
#' # Create a CustomData object
#' df <- data.frame(a = factor(sample(letters[1:5], 100, replace = TRUE)), b = factor(sample(letters[6:10], 100, replace = TRUE)), c = rnorm(100))
#' obj <- customdata(df)
#' 
#' # Calculate entropy for factor columns
#' entropy_values <- applyEntropy(obj)
#' print(entropy_values)

applyEntropy <- function(custom_data) {
  # Check that custom_data is an object of class CustomData
  if (!is(custom_data, "CustomData")) {
    stop("argument must be an object of the class CustomData.")
  }
  
  data <- custom_data@data
  
  # Identify factor columns in the data frame
  factor_cols <- sapply(data, is.factor)
  
  # Identify non-factor columns
  non_factor_cols <- names(data)[!factor_cols]
  
  # Warning for non-factor columns
  if (length(non_factor_cols) > 0) {
    warning("The following columns are not factors and will be ignored: ", paste(non_factor_cols, collapse = ", "))
  }
  
  # Check if there are no factor columns
  if (!any(factor_cols)) {
    warning("No factor columns found in the data. Returning an empty data frame.")
    return(data.frame(entropy_value = numeric(0), norm_entropy = numeric(0)))
  }
  
  # Calculate entropy for each factor column
  entropy_results <- lapply(names(data)[factor_cols], function(col_name) {
    entropy(data[[col_name]])
  })
  
  # Convert the list of entropy results into a data frame
  entropy_df <- do.call(rbind, lapply(entropy_results, as.data.frame))
  colnames(entropy_df) <- c("entropy_value", "norm_entropy")
  
  rownames(entropy_df) <- names(data)[factor_cols]
  
  return(customdata(entropy_df))
}
##=============================================================
## Calculate ROC and AUC
##=============================================================

#' Calculate ROC AUC from CustomData o data.frame
#'
#' @description This function calculates the ROC curve and AUC from a given data frame or CustomData object containing probability and binary label columns.
#' @param df A data frame containing the columns specified by `prob_col` and `label_col`.
#' @param prob_col The name of the column in the data frame that contains probability values.
#' @param label_col The name of the column in the data frame that contains the binary labels.
#' @details
#' The function handles missing values by removing rows with NA values in either the probability or label columns. It also converts logical labels to numeric values if necessary and checks that the labels are binary (0 or 1). The ROC points are calculated for unique thresholds of probabilities, and the AUC is computed using the trapezoidal rule.
#' @return A list with two elements:
#' \item{roc_points}{A matrix where each row represents a point on the ROC curve, with columns for False Positive Rate (FPR) and True Positive Rate (TPR).}
#' \item{auc}{The Area Under the Curve (AUC) value, representing the performance of the classifier.}
#' @examples
#' df <- data.frame(prob = runif(100), label = sample(c(0, 1), 100, replace = TRUE))
#' result <- rocAuc(df, "prob", "label")
#' print(result)
#' @export
#' 

rocAuc <- function(df, prob_col, label_col) {
  if (is(df, "CustomData")) {
    # Extract the data frame from CustomData
    df <- df@data
  } else if (is.data.frame(df)) {
    # Use the provided data frame
    df <- df
  } else {
    stop("Input must be a data frame or an object of class CustomData.")
  }
  
  # Ensure that the specified columns exist in the DataFrame
  if (!(prob_col %in% names(df)) || !(label_col %in% names(df))) {
    stop("The specified columns do not exist in the DataFrame.")
  }
  
  # Extract the columns for probabilities and true labels
  probabilities <- df[[prob_col]]
  true_labels <- df[[label_col]]
  
  # Remove rows with missing values in either column
  complete_cases <- complete.cases(probabilities, true_labels)
  probabilities <- probabilities[complete_cases]
  true_labels <- true_labels[complete_cases]
  
  # Convert TRUE/FALSE to 1/0 for the label column
  if (is.logical(true_labels)) {
    true_labels <- as.numeric(true_labels)
  }
  
  # Check if true_labels contain only binary values (0 or 1)
  if (!all(true_labels %in% c(0, 1))) {
    stop("Label column must contain binary values (0 or 1) or boolean values (TRUE or FALSE).")
  }
  
  # Check if there are any non-NA values left after removing missing values
  if (length(probabilities) == 0 || length(true_labels) == 0) {
    stop("No complete cases available for ROC curve calculation.")
  }
  
  # Calculate unique thresholds for classification
  thresholds <- unique(sort(probabilities, decreasing = TRUE))
  
  # Calculate ROC points for each threshold
  roc_points <- sapply(thresholds, function(threshold) {
    predicted <- ifelse(probabilities >= threshold, 1, 0)
    TP <- sum(predicted == 1 & true_labels == 1)
    FN <- sum(predicted == 0 & true_labels == 1)
    FP <- sum(predicted == 1 & true_labels == 0)
    TN <- sum(predicted == 0 & true_labels == 0)
    
    # Calculate True Positive Rate (TPR) and False Positive Rate (FPR)
    TPR <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
    FPR <- ifelse((FP + TN) == 0, 0, FP / (FP + TN))
    
    c(FPR, TPR)
  })
  
  # Transpose and sort ROC points by FPR
  roc_points <- t(roc_points)
  roc_points <- roc_points[order(roc_points[, 1]), ]  # Sort by FPR
  
  # Calculate the Area Under the Curve (AUC) using the trapezoidal rule
  auc <- 0
  for (i in 2:nrow(roc_points)) {
    auc <- auc + (roc_points[i, 1] - roc_points[i - 1, 1]) * (roc_points[i, 2] + roc_points[i - 1, 2]) / 2
  }
  
  # Return a list containing ROC points and the AUC value
  list(roc_points = roc_points, auc = auc)
}

