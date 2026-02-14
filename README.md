# LEO.BASIC
[![](https://laleoarrow.r-universe.dev/badges/leo.basic)](https://laleoarrow.r-universe.dev/leo.basic)

## Installation

You can install `leo.basic` from [r-universe](https://laleoarrow.r-universe.dev) like so:

``` r
# Enable the r-universe repository
options(repos = c(
  laleoarrow = "https://laleoarrow.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))

# Install leo.basic
install.packages("leo.basic")
```

### Dependencies

To install all dependencies (including Bioconductor packages like `clusterProfiler`, `ReactomePA`, `org.Hs.eg.db`), you should use `pak`:

``` r
# Install pak if not installed
if (!require("pak")) install.packages("pak")

# Install leo.basic with all dependencies
pak::pkg_install("laleoarrow/leo.basic")
```

Or manually using `BiocManager`:

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("laleoarrow/leo.basic")
```
