#' Extract the midpoint of an interval assigned to a data frame.
#'
#' \code{interval_mid} Calculates the mid-point of an interval to be used as an
#' attractive label for plot axes when binning an axis. Intervals as defined by
#' functions such as base-R \code{\link{cut}}.
#'
#' @author Alex M Trueman
#'
#' @param x A numeric vector or data frame column with intervals typically
#'   applied using the base-R \code{cut} function.
#' @param dp A numeric scalar (default 1) defining the number of decimal places
#'   for the base-R \code{round} function. May be needed to ensure that
#'   mid-points are unique.
#' @return A numeric vector containing the mid points of the intervals.
#' @keywords internal
interval_mid <- function(x, dp = 1) {
  # Extract lower and upper bounds of the intervals.
  lower <- as.double(gsub(",.*", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  upper <- as.double(gsub(".*,", "", gsub("\\(|\\[|\\)|\\]", "", x)))
  # Return midpoint.
  return(as.double(round(lower + (upper - lower) / 2, dp)))
}

#' Calculate Data for Contact Analysis
#'
#' \code{contact_data} generates data from domain-coded drillhole data suitable
#' for plotting contact analysis plots. For each possible pairing of categorical
#' domain codes it reports the distance and grade of drillhole samples from each
#' contact of the domain pairing. The distance for the first domain in the
#' pairing ('left' side) is reported as a negative distance for plotting
#' purposes.
#'
#' Uses nested for loops, which is not ideal for speed. I am sure that more can
#' be done by vecorizing one or more loops, but, for now, it works and is fast
#' enought to be practical. For example: the dataset 'dholes' contains 24,673
#' records in 371 drillholes with 13 valid domain contact pairings. This dataset
#' takes <50 seconds to process on a fast laptop (2018 i9 CPU).
#'
#' @param df Data frame with columns defined in the following arguments.
#' @param grade Name of column in \code{df} containing numeric values to be
#'   averaged by contact distance.
#' @param bhid Name of column in \code{df} containing character or numeric
#'   categorical hole ID (default `bhid`) identifying each unique drillhole.
#' @param from,to Name of numeric columns of \code{df} with the downhole
#'   distance to the start and end of sample intervals (defaults \code{from} and
#'   \code{to}).
#' @param x,y,z Name of numeric columns of \code{df} with the cartesian
#'   coordinates of the sample mid-points (defaults \code{x}, \code{y}, and
#'   \code{z}).
#' @param domain Name of the column in \code{df} containing the character or
#'   numeric categorical codes for domains across which contact analysis will be
#'   performed (default \code{domain}).
#' @param max_dist Positive numeric scalar for the maximum contact distance to
#'   be calculated (default \code{15}).
#' @param min_samp Positive integer scalar for the minimum number of samples on
#'   each side of the contact (default \code{5}). If less than this the data
#'   won't be returned. This helps remove domain pairings with too few data.
#'
#' @return A list of two data frames 'detail' and 'summary'. 'detail' contains
#'   all data points while 'summary' contains mean grade by binned distances.
#' @export
#' @importFrom assertthat assert_that is.scalar
#' @importFrom dplyr group_by select mutate summarise
#' @importFrom magrittr %<>% %>%
#' @importFrom rlang !! .data enquo quo_name
#' @importFrom utils combn
#' @examples
#' # Smaller dataset for speed.
#' dholes_sub <- dholes[dholes$domain < 4000,]
#' ca <- contact_data(dholes_sub, grade, max_dist = 20)
#'
#' # Extract only certain contacts from contact data.
#' ca_1100 <- purrr::map(ca, ~dplyr::filter(.x, grepl("1100", contact)))
contact_data <- function(df, grade, bhid = bhid, from = from, to = to,
  x = x, y = y, z= z, domain = domain, max_dist = 15, min_samp = 5) {

  grade <- enquo(grade)
  grade_str <- quo_name(grade)
  domain <- enquo(domain)
  domain_str <- quo_name(domain)

  # All args passed as symbols (rather than strings) to be consistent.
  bhid_str <- quo_name(enquo(bhid))
  from_str <- quo_name(enquo(from))
  to_str <- quo_name(enquo(to))
  x_str <- quo_name(enquo(x))
  y_str <- quo_name(enquo(y))
  z_str <- quo_name(enquo(z))

  # Assertions on arguments.
  # 'df' is a data frame containing the columns defined by other arguments.
  assert_that(is.data.frame(df),
    msg = "Argument `df` is not a data frame.")
  assert_that(all(
    c(bhid_str, domain_str, grade_str, from_str, to_str, x_str, y_str, z_str)
    %in% colnames(df)),
    msg = paste0(
      "One of the required columns not in the supplied data frame."))
  # 'grade' , 'from', 'to', 'x', 'y', 'z' are all numeric.
  assert_that(is.numeric(df[[grade_str]]),
    msg = "`grade` must be numeric.")
  assert_that(is.numeric(df[[from_str]]),
    msg = "`from` must be numeric.")
  assert_that(is.numeric(df[[to_str]]),
    msg = "`to` must be numeric.")
  assert_that(is.numeric(df[[x_str]]),
    msg = "`x` must be numeric.")
  assert_that(is.numeric(df[[y_str]]),
    msg = "`y` must be numeric.")
  assert_that(is.numeric(df[[z_str]]),
    msg = "`z` must be numeric.")
  # 'max_dist' is positive numeric scalar > 0.
  assert_that(is.scalar(max_dist))
  assert_that(max_dist > 0, msg = "`max_dist` must be > 0")
  # 'min_samp' is positive whole number scalar >= 1.
  assert_that(is.scalar(min_samp))
  assert_that(
    (function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol)
    (min_samp),
    msg = "`min_samp` must be a whole number")
  assert_that(min_samp >= 1, msg = "`min_samp` must be >= 1")


  domain_pairs <- combn(sort(unique(df[[domain_str]])), 2)
  all_holes <- sort(unique(df[[bhid_str]]))
  cnt <- 0

  for (dom in 1:ncol(domain_pairs)) {
    # Reset for each domain pairing.
    counti <- 0
    countj <- 0
    disti <- c()
    valuei <- c()
    distj <- c()
    valuej <- c()
    for (hole in 1:length(all_holes)) {
      samples <- df[df[[bhid_str]] == all_holes[hole],]
      samples <- samples[order(samples[[from_str]], samples[[to_str]]),]
      count_samples <- nrow(samples)
      for (sample in 2:count_samples) {
        this_samp <- samples[sample,]
        last_samp <- samples[sample - 1,]
        if (this_samp[[domain_str]] == domain_pairs[2, dom] &
            last_samp[[domain_str]] == domain_pairs[1, dom]) {
          xc <- (this_samp[[x_str]] + last_samp[[x_str]]) * 0.5
          yc <- (this_samp[[y_str]] + last_samp[[y_str]]) * 0.5
          zc <- (this_samp[[z_str]] + last_samp[[z_str]]) * 0.5
          for (consamp in (sample - 1):1) {
            con_samp <- samples[consamp,]
            if (con_samp[[domain_str]] == domain_pairs[1, dom]) {
              d <-
                sqrt(
                  (con_samp[[x_str]] - xc)^2 *
                    (con_samp[[y_str]] - yc)^2 *
                    (con_samp[[z_str]] - zc)^2
                )
              if (d <= max_dist) {
                counti <- counti + 1
                disti[counti] <- d
                valuei[counti] <- con_samp[[grade_str]]
              }
            }
          }
          for (consamp in sample:count_samples) {
            con_samp <- samples[consamp,]
            if (con_samp[[domain_str]] == domain_pairs[2, dom]) {
              d <-
                sqrt(
                  (con_samp[[x_str]] - xc)^2 *
                    (con_samp[[y_str]] - yc)^2 *
                    (con_samp[[z_str]] - zc)^2
                )
              if (d <= max_dist) {
                countj <- countj + 1
                distj[countj] <- d
                valuej[countj] <- con_samp[[grade_str]]
              }
            }
          }
        }
        if (this_samp[[domain_str]] == domain_pairs[1, dom] &
            last_samp[[domain_str]] == domain_pairs[2, dom]) {
          xc <- (this_samp[[x_str]] + last_samp[[x_str]]) * 0.5
          yc <- (this_samp[[y_str]] + last_samp[[y_str]]) * 0.5
          zc <- (this_samp[[z_str]] + last_samp[[z_str]]) * 0.5
          for (consamp in (sample - 1):1) {
            con_samp <- samples[consamp,]
            if (con_samp[[domain_str]] == domain_pairs[2, dom]) {
              d <-
                sqrt(
                  (con_samp[[x_str]] - xc)^2 *
                    (con_samp[[y_str]] - yc)^2 *
                    (con_samp[[z_str]] - zc)^2
                )
              if (d <= max_dist) {
                countj <- countj + 1
                distj[countj] <- d
                valuej[countj] <- con_samp[[grade_str]]
              }
            }
          }
          for (consamp in sample:count_samples) {
            con_samp <- samples[consamp,]
            if (con_samp[[domain_str]] == domain_pairs[1, dom]) {
              d <-
                sqrt(
                  (con_samp[[x_str]] - xc)^2 *
                    (con_samp[[y_str]] - yc)^2 *
                    (con_samp[[z_str]] - zc)^2
                )
              if (d <= max_dist) {
                counti <- counti + 1
                disti[counti] <- d
                valuei[counti] <- con_samp[[grade_str]]
              }
            }
          }
        }
      }
    }
    # Skip to next if not enough samples.
    if(counti < min_samp & countj < min_samp) next
    # Build the detailed dataframe.
    cnt <- cnt + 1
    tempdatai <- data.frame(dist = -disti, value = valuei)
    tempdatai[,"domain"] <- domain_pairs[1, dom]
    tempdataj <- data.frame(dist = distj, value = valuej)
    tempdataj[,"domain"] <- domain_pairs[2, dom]
    tempdata <- rbind(tempdatai, tempdataj)
    # Create a contact label with the total number of samples.
    tempdata[, "contact"] <-
      paste0(
        domain_pairs[1, dom],
        "(n=",
        nrow(tempdata[tempdata[[domain_str]] == domain_pairs[1, dom], ]),
        ") : ",
        domain_pairs[2, dom],
        "(n=",
        nrow(tempdata[tempdata[[domain_str]] == domain_pairs[2, dom], ]),
        ")"
      )
    if(cnt == 1) {
      cdata_detail <- tempdata
    } else {
      cdata_detail <- rbind(cdata_detail, tempdata)
    }
  }

  cdata_detail  <- cdata_detail[,c("contact", "domain", "dist", "value")]

  # Create summary data.
  cdata_summary <- cdata_detail %>%
    group_by(.data$contact, .data$domain, bin = cut(.data$dist, seq(-max_dist, max_dist))) %>%
    summarise(value = mean(.data$value, na.rm = TRUE)) %>%
    mutate(dist = interval_mid(.data$bin, 1)) %>%
    select(-.data$bin)

  return(list(detail = cdata_detail, summary = cdata_summary))
}

#' Produce Basic Contact Analysis Plots.
#'
#' \code{contact_plot} produces a minimally formatted contact analysis plot
#' facetted by domain using \code{\link{ggplot2}}. The input data is created by
#' \code{\link{contact_data}}. Additonal formatting can be applied using
#' \code{\link{ggplot2}} functions (see example).
#'
#' @param data List of data frames created by \code{\link{contact_data}} with
#'   'detail' and 'summary' contact data.
#'
#' @return A \code{\link{ggplot2}} plot object.
#' @export
#' @importFrom ggplot2 aes ggplot geom_smooth geom_line geom_vline facet_wrap
#' @importFrom rlang .data
#' @examples
#' library(ggplot2)
#' cdata <- contact_data(dholes, grade)
#' p <- contact_plot(cdata)
#' p +
#'  labs(x = "Distance (m)", y = "Mean grade (g/t)") +
#'  theme_bw()
contact_plot <- function(data) {
  ggplot() +
    geom_smooth(
      data = data$detail,
      aes(x = .data$dist, y = .data$value, group = .data$domain),
      method = "lm",
      linetype = "dashed",
      colour = "black",
      fill = "grey75",
      alpha = 0.5,
      size = 0.25
    ) +
    geom_line(
      data = data$summary,
      aes(.data$dist, .data$value, group = .data$domain),
      size = 0.25, colour = "black"
    ) +
    geom_vline(xintercept = 0, colour = "black") +
    facet_wrap(~contact, scales = "free_y")
}
