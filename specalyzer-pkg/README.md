# Specalyzer R package 

## Installation

1. Install the **0.5.1** version of [hsdar:](https://cran.r-project.org/web/packages/hsdar/index.html)

The easiest way to do this is using `install_version()` from the `devtools` library, which you can install in R using the command `install.packages("devtools")`.

```{R}
library(devtools)
install_version("hsdar", version = "0.5.1", repos = "https://cloud.r-project.org/")
```

2. Install **asdreader**, **dplyr**, **reshape2**, **plotly**, **magrittr**:

```{R}
install.packages("asdreader")
install.packages("dplyr")
install.packages("reshape2")
install.packages("plotly")
install.packages("magrittr")
```

3. And now, finally, install the specalyzer package:

```
devtools::install_github("alkc/specalyzer", subdir = "specalyzer-pkg")
```
