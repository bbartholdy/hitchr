
# hitchr

Random sample generator inspired by Douglas Adams’ The Hitchhiker’s
Guide to the Galaxy

<!-- badges start -->

[![build
status](https://github.com/bbartholdy/hitchr/workflows/R-CMD-check/badge.svg)](https://github.com/bbartholdy/hitchr/actions)
[![Codecov test
coverage](https://codecov.io/gh/bbartholdy/hitchr/branch/master/graph/badge.svg)](https://codecov.io/gh/bbartholdy/hitchr?branch=master)
<!-- badges end -->

Currently in development.

To install:

``` r
remotes::install_github("bbartholdy/hitchr")
```

Example:

``` r
hitchr::h2g2(10) # generates a random sample of 10 individuals
```

    ##             race    sex age   height    weight  IQ                occupation
    ## 3          human   male  67 162.7929  67.30787 111          Dental Hygienist
    ## 25 golgafrinchan   male  55 175.2826  87.95943  77               hairdresser
    ## 20         vogon     no 146 233.3638 361.81738 126         Budget Accountant
    ## 15         vogon     no 145 232.0269 341.78153 121   Personnel Administrator
    ## 8          human female  61 157.5434  52.40789 100     Skin Care Specialists
    ## 27 golgafrinchan female  76 157.0075  52.57430  99               hairdresser
    ## 10         human female 101 164.6813  56.81682 112 Employee Benefits Analyst
    ## 28 golgafrinchan female  60 170.0193  66.83398 113       telephone sanitiser
    ## 6          human female   6 176.4122  80.12405 111         Fire Investigator
    ## 29 golgafrinchan female  43 167.6272  72.65791  95               hairdresser
