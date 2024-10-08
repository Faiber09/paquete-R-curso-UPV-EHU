% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{applyEntropy}
\alias{applyEntropy}
\title{Calculate Entropy for Factor Columns of CustomData}
\usage{
applyEntropy(custom_data)
}
\arguments{
\item{custom_data}{An object of class \code{\linkS4class{CustomData}}. This object contains a data frame with columns to be processed.}
}
\value{
A CustomData object with the entropy and normalized entropy values for each factor column. The columns of the returned data frame are 'entropy_value' and 'norm_entropy'.
}
\description{
This function calculates the entropy and normalized entropy for each factor column of a \code{\linkS4class{CustomData}} object.
}
\details{
Only factor columns are considered. If no factor columns are present, the function returns an empty data frame.
}
\examples{
# Create a CustomData object
df <- data.frame(a = factor(sample(letters[1:5], 100, replace = TRUE)), b = factor(sample(letters[6:10], 100, replace = TRUE)), c = rnorm(100))
obj <- customdata(df)

# Calculate entropy for factor columns
entropy_values <- applyEntropy(obj)
print(entropy_values)
}
