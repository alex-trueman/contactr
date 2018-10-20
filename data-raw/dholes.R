# Import dholes data.
dholes <- readr::read_csv(
  file = "data-raw/dholes.csv",
  skip = 1, na = "",
  col_names = c("bhid", "from", "to", "length", "x", "y", "z", "dip", "dipdir",
      "domain", "grade"),
  col_types = "cdddddddddd"
)
usethis::use_data(dholes, overwrite = TRUE, compress = 'xz')
