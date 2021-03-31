
<!-- README.md is generated by README.Rmd. Please edit .Rmd file. -->

# hitchr <img src="./README_files/thumb.jpg" align="right" height="139" />

<!-- badges: start -->

[![build
status](https://github.com/bbartholdy/hitchr/workflows/R-CMD-check/badge.svg)](https://github.com/bbartholdy/hitchr/actions)
[![Codecov test
coverage](https://codecov.io/gh/bbartholdy/hitchr/branch/master/graph/badge.svg)](https://codecov.io/gh/bbartholdy/hitchr?branch=master)
<!-- badges: end -->

Random sample generator inspired by Douglas Adams’ The Hitchhiker’s
Guide to the Galaxy.

Currently in development.

To install:

``` r
remotes::install_github("bbartholdy/hitchr")
```

:exclamation:**Important Note**:exclamation:

This package deals with the concept of ‘race’ in a strictly biological
sense; i.e., there is no way to biologically distinguish discrete races
within the *Homo sapiens* species (despite claims to the contrary).

This should not distract from the very real concept of social ‘race’,
and the social injustice that currently persists across the globe.

[BlackLivesMatter](https://blacklivesmatter.com/)

[StopAsianHate](https://www.stopasianhate.info/)

## Examples

``` r
hitchr::infinite_improbability_drive(10) # generates a random sample of 10 individuals
```

| race          | sex    | age |   height |    weight |  IQ | occupation                           |
|:--------------|:-------|----:|---------:|----------:|----:|:-------------------------------------|
| golgafrinchan | male   |  48 | 181.4208 |  92.60239 | 102 | Lawyer                               |
| human         | female |  64 | 169.1606 |  69.90690 | 102 | Disk Jockey                          |
| human         | male   | 111 | 169.6223 |  77.83919 |  94 | Purchasing Manager                   |
| human         | female |  23 | 160.9582 |  54.17273 | 109 | Medical Photographer                 |
| dentrassi     | female |  52 | 220.3259 | 147.24609 | 115 | Pantry Chef                          |
| vogon         | male   | 163 | 234.9890 | 358.00241 | 120 | Compliance Officer                   |
| dentrassi     | female |  66 | 214.4455 | 147.62405 | 120 | Bartender (Full-Time)                |
| vogon         | male   | 144 | 230.6695 | 349.59985 | 117 | Education and Training Administrator |
| human         | female |  72 | 163.3778 |  65.67301 |  97 | Tax Examiner                         |
| vogon         | female | 108 | 213.6132 | 323.56344 | 127 | Compensation Administrator           |

Races currently available:

``` r
hitchr:::race_index()
```

    ## [1] "humans"         "vogons"         "golgafrinchans" "dentrassi"

Stats currently available:

``` r
hitchr:::stats_index()
```

    ## [1] "race"       "sex"        "age"        "height"     "weight"    
    ## [6] "IQ"         "occupation"

Sexual dimorphism in the height of males and females of different races:

``` r
hitchr_sample <- hitchr::inf_improb_dr(1000)
hitchr_sample %>%
  filter(sex == "male" | sex == "female") %>%
  group_by(race) %>%
  ggplot(aes(x = sex, y = height, fill = race)) +
    geom_boxplot() +
    theme_minimal() +
    scale_fill_viridis_d() +
    facet_wrap(~ race) +
    theme(legend.position = "none")
```

![](README_files/figure-gfm/sex-dim-1.png)<!-- -->
