[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

# `contactr`: Drillhole contact analysis

A contact is the meeting point/plane of two different geological units. A geological unit in this case can be any characteristic such as rock type, alteration type, weathering type, or, more frequently, grade estimation domains. Contact analysis is an examination of the behaviour of one or more continuous variables (e.g., gold grade) as they approach a contact. Usually we look at the mean value of the variable as it approaches a contact.

The package contains a function to generate contact analysis data (`contact_data`) and a function to produce a basic plot from this data (`contact_plot`). See help for each function for examples (e.g. `?contact_data`).

## Installation

`contactr` is not available on CRAN but can be installed from Github.

``` r
# install.packages("devtools")
devtools::install_github("truemoid/contactr")
```

## Contributing

Please log all issues though the Github interface. Alternatively, make the required changes yourself and submit a pull request.


### Code of conduct

Please note that the `contactr` project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
