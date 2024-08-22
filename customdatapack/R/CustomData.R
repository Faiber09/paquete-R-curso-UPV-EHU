##=============================================================
## MY PROJECT
##=============================================================

#' An S4 class to represent CustomData
#' 
#' @slot data A data frame
#'

setClass(
  Class = "CustomData",
  slots = c(data = "data.frame")
)

#' Validity function for the CustomData class
#' 
#' @description This function checks if the input data is a data frame
#' @param object An object of class \code{\linkS4class{CustomData}}
#' @return TRUE if the input data is valid, otherwise an error message
#' 

checkValiditycustomdata <- function(object) {
  if (!is.data.frame(object@data)) {
    return("Input must be a data frame")
  }
  return(TRUE)
}

setValidity(Class = "CustomData", method = checkValiditycustomdata)

#' Basic constructor of the CustomData class
#'
#' @description This function creates an object of class \code{\linkS4class{CustomData}}. The input data is converted to a data frame before being used to create the object.
#'
#' @param data A data frame, vector, factor, or matrix to be used as the content of the CustomData object. All inputs will be converted to a data frame.
#' @return An object of class \code{\linkS4class{CustomData}} with the specified data.
#' @details
#' Any input \code{data} will be internally converted to a data frame using \code{\link{as.data.frame}} before being assigned to the \code{data} slot of the \code{CustomData} object.
#' @examples
#' # Create a CustomData object with different types of data
#' df <- data.frame(x = 1:3, y = letters[1:3])
#' obj_df <- customdata(df)
#' vec <- c(10, 20, 30)
#' obj_vec <- customdata(vec)
#' fac <- factor(c("A", "B", "A"))
#' obj_fac <- customdata(fac)
#' mat <- matrix(1:4, nrow = 2)
#' obj_mat <- customdata(mat)
#' @seealso \code{\link{checkValiditycustomdata}}
#'

customdata <- function(data) {
  if (!(is.data.frame(data) || is.vector(data) || is.factor(data) || is.matrix(data))) {
    stop("Input must be either a data frame, a vector, a factor, or a matrix")
  }
  data <- as.data.frame(data)
  return(new("CustomData", data = data))
}

# Disable basic arithmetic operations for CustomData
setMethod(
  f = "Ops",
  signature = c(e1 = "CustomData", e2 = "CustomData"),
  definition = function(e1, e2) {
    stop("Operators not defined for class CustomData")
  }
)

#' Print method for CustomData
#'
#' @description This method prints information about the CustomData object
#' @param object An object of class \code{\linkS4class{CustomData}}
#' @return Prints details of the CustomData object, including its type and content
#' @examples
#' df <- data.frame(a = 1:5, b = letters[1:5])
#' obj <- customdata(df)
#' show(obj)
#' vec <- c(1, 2, 3)
#' obj_vec <- customdata(vec)
#' show(obj_vec)
#' @seealso \code{\link{customdata}}
#'

setMethod(
  f = "show",
  signature = "CustomData",
  definition = function(object) {
    cat("CustomData containing a data frame with", nrow(object@data), "rows and", ncol(object@data), "columns.\n")
    print(object@data)
  }
)

#' Method to get column names
#'
#' @description This method returns the column names if the CustomData object contains a data frame
#' @param x An object of class \code{\linkS4class{CustomData}}
#' @return Column names of the data frame contained in the CustomData object, or NULL if not applicable
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3])
#' obj <- customdata(df)
#' colnames(obj)
#' @seealso \code{\link{customdata}}
#'

setMethod(
  f = "colnames",
  signature = "CustomData",
  definition = function(x) {
    return(colnames(x@data))
  }
)

# Define column_classes as a generic function
setGeneric(
  name = "column_classes",
  def = function(x) standardGeneric("column_classes")
)

#' Method to get column classes
#'
#' @description This method returns the class of each column in the data frame contained in the CustomData object
#' @param x An object of class \code{\linkS4class{CustomData}}
#' @return A named vector with the class of each column in the data frame
#' @examples
#' df <- data.frame(a = 1:3, b = letters[1:3])
#' obj <- customdata(df)
#' column_classes(obj)
#' @seealso \code{\link{customdata}}
#'

setMethod(
  f = "column_classes",
  signature = "CustomData",
  definition = function(x) {
    return(sapply(x@data, class))
  }
)