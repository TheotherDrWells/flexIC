# flexIC

**flexIC** is a high-precision Iman–Conover engine for generating continuous variables that preserve **rank correlation** with **marginal fidelity**. It offers **tunable convergence control**, allowing you to aggressively reduce rank-correlation distortion—at the cost of a few extra milliseconds.

Use it to:
- Simulate data with a target Spearman or Kendall structure
- Preserve original variable distributions via back-ranking
- Validate or stress-test statistical methods under structured dependence

---

## 🚀 Why use `flexIC`?

Most Iman–Conover implementations:
- Run once with no convergence check
- Do not guarantee low error
- Break marginal shapes in edge cases

**flexIC**:
- Iterates until **max abs rank-correlation error ≤ ε**
- Keeps original marginal shapes intact
- Returns detailed error diagnostics
- Finishes in milliseconds on typical datasets

---

## 📦 Installation

```r
# Development version (until on CRAN)
remotes::install_github("yourusername/flexIC")
