# ebdm: Estimating Binary Dependency from Marginal Data

This repository accompanies the R package **ebdm**, available on CRAN.

The package implements a **maximum likelihood method** to estimate the joint distribution of two binary variables using only **marginal summary data** from multiple independent studies. This setting arises frequently in **clinical trial simulation (CTS)** where only aggregated data (e.g., proportions, sample sizes) are publicly available due to privacy constraints.

The method is detailed in our manuscript:

> Shang, Tsao, and Zhang (2025).  
> *Estimating the Joint Distribution of Two Binary Variables from Their Marginal Summaries*.  
> [arXiv:2505.03995](https://doi.org/10.48550/arXiv.2505.03995)

---

## 🔍 Introduction

In many real-world scenarios such as drug development, access to individual-level patient data is limited. Researchers often only observe summary-level data—such as marginal proportions of demographic or risk variables across multiple studies. Estimating the dependency structure (i.e., the joint distribution) between two such binary variables is critical for realistic clinical trial simulations, but challenging under privacy constraints.

This package provides a statistically sound and computationally efficient method to estimate the **joint probability mass function** of two binary variables given their marginal summaries across multiple studies with varying sample sizes.

---

## 📦 Installation

The `ebdm` package is available on CRAN. Install it using:

```r
install.packages("ebdm")
```

---

## 🌰 Usage Example

```r
library(ebdm)

# Load an example dataset
data(eg_data)

# Estimate joint distribution using likelihood ratio method for CI
result <- ebdm_estimate(
  ni = eg_data$ni,
  xi = eg_data$xi,
  yi = eg_data$yi,
  ci_method = "lr"
)

print(result)
```

---

## 📈 Reproducing Simulation Studies

## 📚 Citation

If you use this package or method in your work, please cite:
```bibtex
@article{shang2025ebdm,
 title = {Estimating the Joint Distribution of Two Binary Variables from Their Marginal Summaries},
 author = {Shang, Longwen and Tsao, Min and Zhang, Xuekui},
 journal = {arXiv preprint arXiv:2505.03995},
 year = {2025}
}
```


## 💬 Contact

For questions or feedback, please contact:
- 📧 **Xuekui Zhang** – xuekui@uvic.ca
- 📧 **Longwen Shang** – shanglongwen0918@gmail.com









