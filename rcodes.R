# install.packages(c("lavaan","semTools","psych","semPlot","readr","dplyr"))


# =====================================================
# 1. Load packages
# =====================================================
library(lavaan)
library(semTools)
library(psych)
library(semPlot)
library(readr)
library(dplyr)

# =====================================================
# 2. Import the simulated CSV dataset
# =====================================================
data <- read_csv(file.choose(), show_col_types = FALSE)

# Check dataset
dim(data)
names(data)
summary(data)

# =====================================================
# 3. Specify CFA and SEM model
# =====================================================
model <- '
  # Measurement model
  LAB  =~ LAB1 + LAB2 + LAB3 + LAB4 + LAB5
  LATO =~ LATO1 + LATO2 + LATO3 + LATO4 + LATO5
  EIA  =~ EIA1 + EIA2 + EIA3 + EIA4 + EIA5
  TLGC =~ TLGC1 + TLGC2 + TLGC3 + TLGC4 + TLGC5
  IFE  =~ IFE1 + IFE2 + IFE3 + IFE4 + IFE5

  # Structural model
  IFE ~ LAB + LATO + EIA + TLGC

  # Correlations among independent constructs
  LAB  ~~ LATO + EIA + TLGC
  LATO ~~ EIA + TLGC
  EIA  ~~ TLGC
'
# variables name check
names(data1)
# =====================================================
# 4. Identify ordinal Likert-scale items
# =====================================================
items <- names(data1)
items

# =====================================================
# 5. Estimate the SEM model
# =====================================================
fit <- sem(
  model,
  data = data1,
  estimator = "WLSMV",
  ordered = items,
  std.lv = TRUE
)

# =====================================================
# 6. Display model results
# =====================================================
summary(
  fit,
  fit.measures = TRUE,
  standardized = TRUE,
  rsquare = TRUE
)

# =====================================================
# 7. Extract model-fit indices
# =====================================================
fitMeasures(
  fit,
  c(
    "chisq",
    "df",
    "pvalue",
    "cfi",
    "tli",
    "rmsea",
    "srmr"
  )
)

# =====================================================
# 8. Display standardized factor loadings and paths
# =====================================================
standardizedSolution(fit)

# =====================================================
# 9. Extract structural path coefficients
# =====================================================
paths <- standardizedSolution(fit) |>
  filter(op == "~", lhs == "IFE")

print(paths)

# =====================================================
# 10. Extract explained variance
# =====================================================
inspect(fit, "r2")

# =====================================================
# 11. Visualize the SEM model
# =====================================================
semPaths(
  fit,
  what = "std",
  whatLabels = "std",
  style = "lisrel",
  layout = "tree",
  residuals = FALSE,
  intercepts = FALSE,
  edge.label.cex = 0.8,
  nCharNodes = 0
)
