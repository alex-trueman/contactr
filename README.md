# `contactr`: Drillhole contact analysis

A contact is the meeting point/plane of two different geological units. A geological unit in this case can be any characteristic such as rock type, alteration type, weathering type, or, more frequently, grade estimation domains. Contact analysis is an examination of the behaviour of one or more continuous variables (e.g., gold grade) as they approach a contact. Usually we look at the mean value of the variable as it approaches a contact, but it may be useful to look at other statistics such as variance or correlation.

## Drillholes and samples

Contact analysis is performed on drillholes. Drillholes, also called boreholes or wells, are holes drilled into the earth to take samples of soil, sand, or rock below the surface (they may also be drilled into other materials such as ice). The samples returned from the drillhole may be in the form of continuous lengths of core or rock chips (cuttings) depending on the type of drilling. This material is physically described ("logged") by geologists noting such characteristics as colour, hardness, texture, rock type, weathering, and alteration. Other types of geophysical analysis, such as spectral analysis and magnetic susceptibility may also be conducted on the samples. Normally the geologist will select samples from zones of interest to be chemically or physically analysed for characteristics such as chemical concentrations of elements and compounds such as gold, silver, and copper.

The position of samples is described by their distance from the starting point of the drillhole (the collar). Typically, this location along the drillhole is given by the distance to the start of the sample `from` and the end of the sample `to`, but they may also be defined by the distance from the collar to the centre of the sample (often called `at` or `depth`).

```R
dholes <- tribble(
  ~bhid, ~from,   ~to, ~au, ~domain,
  DD001,  0.00,  1.00,
  )
```

## Installation

`contactr` is not available on CRAN yet but can be installed from Github.

``` r
# install.packages("devtools")
devtools::install_github("truemoid/contactr")
