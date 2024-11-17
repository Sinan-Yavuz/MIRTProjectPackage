# MIRTProjectPackage

MIRTProjectPackage is an R package designed to simulate multidimensional item response theory (MIRT) and unidimensional item response theory (UIRT) data, estimate parameters, and evaluate performance metrics like bias and RMSE across different conditions.

## Features

- Simulate student responses based on 2PL item response models.
- Estimate parameters using both UIRT and MIRT models.
- Evaluate bias and RMSE for theta scores, item difficulty, and item discrimination parameters.
- Compare true and estimated factor correlations.

---

## Installation

You can install the package directly from GitHub:

```r
# Install devtools if not already installed
install.packages("devtools")

# Install the package from GitHub
devtools::install_github("Sinan-Yavuz/MIRTProjectPackage")
```

## Usage

Here is an example workflow using MIRTProjectPackage:

1. Load the Package
```
library(MIRTProjectPackage)
```

2. Define Simulation Conditions

Set the number of students and items to simulate:

```
conditions <- expand.grid(
  n_students = c(500, 1000, 5000),
  n_items = c(12, 24, 48)
)
```
3. Run Simulations

Run replications for each condition:

```
# Run 10 replications per condition in parallel
results <- run_parallel_replications(conditions, n_replications = 10, parallel = TRUE)
```

4. Access Results

The results include three main summaries: theta scores, item parameters, and correlation metrics.

```
# Theta Scores
theta_results <- results$Theta_Results
head(theta_results)

# Item Parameter Results
item_results <- results$Item_Results
head(item_results)

# Factor Correlations
correlation_results <- results$Correlation_Results
head(correlation_results)
```
