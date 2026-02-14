# LEO.BASIC
[![](https://laleoarrow.r-universe.dev/badges/leo.basic)](https://laleoarrow.r-universe.dev/leo.basic)
[![ResearchGate](https://img.shields.io/badge/ResearchGate-Ao%20Lu-00CCBB?logo=researchgate&logoColor=white)](https://www.researchgate.net/profile/Ao-Lu-5)
[![ORCID](https://img.shields.io/badge/ORCID-0009--0001--0927--4468-A6CE39?logo=orcid&logoColor=white)](https://orcid.org/0009-0001-0927-4468)
[![Lifecycle: Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

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
