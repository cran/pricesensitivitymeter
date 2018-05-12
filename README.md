pricesensitivitymeter
=====================

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/pricesensitivitymeter)](https://cran.r-project.org/package=pricesensitivitymeter)

Overview
--------

pricesensitivitymeter is an implementation of the van Westendorp Price
Sensitivity Meter (PSM) in R, which is a popular method in market
research to analyze consumer price preferences and price sensitivity. It
also covers the so-called Newton Miller Smith Extension which allows to
estimate prices that maximize the trial rate and the revenue.

Installation
------------

As of version 0.2.1, this package is hosted on CRAN.

``` r
# install the stable release from CRAN
install.packages("pricesensitivitymeter")

# install the development version from Github
devtools::install_github("alletsee/pricesensitivitymeter")
```

Usage
-----

The main function of the package is psm\_analysis() which performs all
necessary analyses.

``` r
## creating example data

tch <- round(rnorm(n = 250, mean = 5, sd = 0.5), digits = 2)
ch <- round(rnorm(n = 250, mean = 8.5, sd = 0.5), digits = 2)
ex <- round(rnorm(n = 250, mean = 13, sd = 0.75), digits = 2)
tex <- round(rnorm(n = 250, mean = 17, sd = 1), digits = 2)

data.psm.demo <- data.frame(tch, ch, ex, tex)

## running the analysis
output.psm.demo <- psm_analysis(toocheap = "tch",
  cheap = "ch",
  expensive = "ex",
  tooexpensive = "tex",
  data = data.psm.demo)

summary(output.psm.demo)
```

Additional Information
----------------------

-   Please [report any issues or
    bugs](https://github.com/alletsee/pricesensitivitymeter/issues).
-   License: MIT

References
----------

Van Westendorp, P (1976) “NSS-Price Sensitivity Meter (PSM) – A new
approach to study consumer perception of price” *Proceedings of the
ESOMAR Congress*, 139–167. Online available at [the ESOMAR
website](https://rwconnect.esomar.org/a-new-approach-to-study-consumer-perception-of-price/).

Newton, D, Miller, J, Smith, P, (1993) “A market acceptance extension to
traditional price sensitivity measurement” *Proceedings of the American
Marketing Association Advanced Research Techniques Forum*.
