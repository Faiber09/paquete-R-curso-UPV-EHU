## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(customdatapack)

df <- data.frame(columna1 = 1:3, columna2 = letters[1:3])
obj_df <- customdata(df)
print(obj_df)

vec <- c(10, 20, 30)
obj_vec <- customdata(vec)
print(obj_vec)

fac <- factor(c("A", "B", "A"))
obj_fac <- customdata(fac)
print(obj_fac)

mat <- matrix(1:4, nrow = 2)
obj_mat <- customdata(mat)
print(obj_mat)

## -----------------------------------------------------------------------------
colnames(obj_df)
column_classes(obj_df)

colnames(obj_fac)
column_classes(obj_fac)

## -----------------------------------------------------------------------------
df <- data.frame(a = 1:5, b = rep(5, 5), c = 10:14, d = factor(c('a','b','a','b','b')))
obj <- customdata(df)
print(obj)

normalized_obj <- normalize(obj)
print(normalized_obj)
 
standardized_obj <- standardize(obj)
print(standardized_obj)

## -----------------------------------------------------------------------------
df <- data.frame(a = c(1,2,3,3,4,4,4,6,8,10), b = 1:10, c = letters[1:10])
obj <- customdata(df)

discretized_objEF <- applyDiscretizeEF(obj, num.bins = 3)
print(discretized_objEF)

discretized_objEW <- applyDiscretizeEW(obj, num.bins = 3)
print(discretized_objEW)

## ----fig.width=10, fig.height=6, out.width="100%"-----------------------------
df <- data.frame(a = factor(c(rep('a', 95), rep('b', 5))), 
                  b = factor(c(rep('x', 50), rep('y', 50))),
                  c = factor(c(rep('k', 70), rep('l', 30))),
                  d = 1:100)
obj <- customdata(df)

entropy_values <- applyEntropy(obj)
print(entropy_values)

plot_entropy(obj)

## ----fig.width=10, fig.height=6, out.width="100%"-----------------------------
set.seed(113)  # For reproducibility
df <- data.frame(
  prob = c(
    runif(50, 0.4, 1),   # High probabilities for positive class
    runif(50, 0, 0.6)    # Low probabilities for negative class
  ),
  label = c(
    rep(1, 50),  # Positive class labels
    rep(0, 50)   # Negative class labels
  )
)

obj <- customdata(df)

result <- rocAuc(obj, "prob", "label")  # Calculate ROC points and AUC

plot_roc(obj, "prob", "label") # Plot the ROC curve

## ----fig.width=10, fig.height=6, out.width="100%"-----------------------------
set.seed(113)  # For reproducibility
df <- data.frame(a = rnorm(10), b = rnorm(10), c = rnorm(10), d = as.factor(sample(letters, 10, replace = TRUE)))
custom_data <- customdata(df)
print(df)
gg <- plotCorrMatrix(custom_data)
gg

## ----fig.width=10, fig.height=6, out.width="100%"-----------------------------
gg + ggplot2::scale_fill_viridis_c()


