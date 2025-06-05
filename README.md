# ebdm: Estimating Binary Dependency from Marginal Data

This repository accompanies the R package **ebdm**, available on CRAN.

The package implements maximum likelihood estimation for recovering the joint distribution of two binary variables using marginal summary data. The method is described in our paper:

> Shang, Tsao and Zhang (2025) <doi:10.48550/arXiv.2505.03995>:
> *"Estimating the Joint Distribution of Two Binary Variables from Their Marginal Summaries"*

## 📦 Installation

```r
install.packages("ebdm")

## 🌰 Usage Example
library(ebdm)
data(eg_data)
ebdm_estimate(eg_data$ni, eg_data$xi, eg_data$yi, ci_method = "lr")
