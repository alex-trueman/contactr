#' Generate Contact Analysis Data from Drilhole Samples.
#'
#' \code{contat_analysis} calculates the change in the mean of samples for a
#' specified value (e.g., grade) with distance eiter side of the contact between
#' two domins.
#'
#' @author Alex M. Trueman, 2018-03-21
#'
#' @param x Data frame containing sample data with required fields identified in
#'   the following arguments.
#' @param id Name of column in \code{x} containing numeric or character ID code
#'   for each line or drillhole (e.g., bhid).
#' @param pos Name of column in \code{x} containing the numeric downhole depth
#'   to the samples (i.e., depth; \code{[to - from] / 2})
#' @param group Name of column in \code{x} contaning the numeric or character
#'   categorical code for the domains whose contacts are to be analysed.
#' @param codes Numeric or character vector with two values identifying the two
#'   specific groups in \code{group} that will have their contacts analysed.
#' @param value Name of numeric column in \code{x} that will be analysed across
#'   the contacts of \code{codes}.
#' @param max_dist Numeric, scalar maximum contact evaluation distance (default
#'   is 50 m)
#' @return A list of two data frames: 'detail' contains un-averaged raw data and
#'   'summary' is averaged by distance.
#' @export
#' @importFrom assertthat assert_that
#' @importFrom dplyr arrange between case_when filter group_by lag lead mutate
#'   n select summarise
#' @importFrom magrittr %>%
#' @importFrom stats var
#' @importFrom rlang .data enquo quo_text
#' @examples
#' dhdata <-  dplyr::mutate(dholes, depth = (to - from) / 2)
#' domains <- c(1100, 1001)
#' condata <- contact_analysis(dhdata, bhid, depth, domain, domains, grade, 20)
#' plot(condata$summary$distance, condata$summary$mean, type = "l")
contact_analysis <- function(
    x, id, pos, group, codes, value, max_dist = 50) {

  # Capture passed data frame fields as R code rather than literal values.
  id <- enquo(id)
  pos <- enquo(pos)
  group <- enquo(group)
  value <- enquo(value)

  # Assertions on arguments.
  # `x` is a data frame.
  assert_that(is.data.frame(x),
    msg = "Argument `x` is not a data frame.")
  # `x` contains all of the required columns.
  assert_that(all(
    c(quo_text(id), quo_text(pos), quo_text(group), quo_text(value)) %in%
      colnames(x)),
    msg = paste0(
      "One of the required columns (",
      quo_text(id), ",", quo_text(pos), ",", quo_text(group), ",",
      quo_text(value),
      ") not in the supplied data frame."))
  # `pos` and `value` are numeric columns.
  assert_that(is.numeric(x[[quo_text(pos)]]),
    msg = "`pos` must be numeric.")
  assert_that(is.numeric(x[[quo_text(value)]]),
    msg = "`value` must be numeric.")
  # `codes` is a two-element numeric vector.
  assert_that(length(codes) == 2,
    msg = "`codes` must contian two elements.")
  assert_that(is.numeric(c(codes)),
    msg = "`codes` must be a numeric vector.")

  # Process the input data frame.
  data <- x %>%
    # Sort by hole id and sample position.
    arrange(!!id, !!pos) %>%
    # Group by hole as analysis is per hole.
    group_by(!!id) %>%
    mutate(
      # Traverse along hole in increasing position (left to right) looking for
      # code 1 to code 2 contacts.
      t1_t2_lr = case_when(
        # Find a code 1 code where the next code is code 2 set variable to
        # difference in position.
        !!group == codes[1] & lead(!!group, 1) == codes[2] ~
          lead(!!pos, 1) - !!pos,
        # Repeat above but for two samples ahead, then three, up to ten...
        !!group == codes[1] & lead(!!group, 2) == codes[2] ~
          lead(!!pos, 2) - !!pos,
        !!group == codes[1] & lead(!!group, 3) == codes[2] ~
          lead(!!pos, 3) - !!pos,
        !!group == codes[1] & lead(!!group, 4) == codes[2] ~
          lead(!!pos, 4) - !!pos,
        !!group == codes[1] & lead(!!group, 5) == codes[2] ~
          lead(!!pos, 5) - !!pos,
        !!group == codes[1] & lead(!!group, 6) == codes[2] ~
          lead(!!pos, 6) - !!pos,
        !!group == codes[1] & lead(!!group, 7) == codes[2] ~
          lead(!!pos, 7) - !!pos,
        !!group == codes[1] & lead(!!group, 8) == codes[2] ~
          lead(!!pos, 8) - !!pos,
        !!group == codes[1] & lead(!!group, 9) == codes[2] ~
          lead(!!pos, 9) - !!pos,
        !!group == codes[1] & lead(!!group, 10) == codes[2] ~
          lead(!!pos, 10) - !!pos
      ),
      # Now traverse back the other way (right to left) in the same fasion.
      t1_t2_rl = case_when(
        !!group == codes[1] & lag(!!group, 1) == codes[2] ~
          !!pos - lag(!!pos, 1),
        !!group == codes[1] & lag(!!group, 2) == codes[2] ~
          !!pos - lag(!!pos, 2),
        !!group == codes[1] & lag(!!group, 3) == codes[2] ~
          !!pos - lag(!!pos, 3),
        !!group == codes[1] & lag(!!group, 4) == codes[2] ~
          !!pos - lag(!!pos, 4),
        !!group == codes[1] & lag(!!group, 5) == codes[2] ~
          !!pos - lag(!!pos, 5),
        !!group == codes[1] & lag(!!group, 6) == codes[2] ~
          !!pos - lag(!!pos, 6),
        !!group == codes[1] & lag(!!group, 7) == codes[2] ~
          !!pos - lag(!!pos, 7),
        !!group == codes[1] & lag(!!group, 8) == codes[2] ~
          !!pos - lag(!!pos, 8),
        !!group == codes[1] & lag(!!group, 9) == codes[2] ~
          !!pos - lag(!!pos, 9),
        !!group == codes[1] & lag(!!group, 10) == codes[2] ~
          !!pos - lag(!!pos, 10)
      ),
      # Record the minimum distance from the two traversals.
      t1_t2 = pmin(.data$t1_t2_lr, .data$t1_t2_rl, na.rm = TRUE),
      # Traverse along hole in increasing position looking for group 2 to group 1
      # contacts.
      t2_t1_lr = case_when(
        !!group == codes[2] & lead(!!group, 1) == codes[1] ~
          lead(!!pos, 1) - !!pos,
        !!group == codes[2] & lead(!!group, 2) == codes[1] ~
          lead(!!pos, 2) - !!pos,
        !!group == codes[2] & lead(!!group, 3) == codes[1] ~
          lead(!!pos, 3) - !!pos,
        !!group == codes[2] & lead(!!group, 4) == codes[1] ~
          lead(!!pos, 4) - !!pos,
        !!group == codes[2] & lead(!!group, 5) == codes[1] ~
          lead(!!pos, 5) - !!pos,
        !!group == codes[2] & lead(!!group, 6) == codes[1] ~
          lead(!!pos, 6) - !!pos,
        !!group == codes[2] & lead(!!group, 7) == codes[1] ~
          lead(!!pos, 7) - !!pos,
        !!group == codes[2] & lead(!!group, 8) == codes[1] ~
          lead(!!pos, 8) - !!pos,
        !!group == codes[2] & lead(!!group, 9) == codes[1] ~
          lead(!!pos, 9) - !!pos,
        !!group == codes[2] & lead(!!group, 10) == codes[1] ~
          lead(!!pos, 10) - !!pos
      ),
      t2_t1_rl = case_when(
        !!group == codes[2] & lag(!!group, 1) == codes[1] ~
          !!pos - lag(!!pos, 1),
        !!group == codes[2] & lag(!!group, 2) == codes[1] ~
          !!pos - lag(!!pos, 2),
        !!group == codes[2] & lag(!!group, 3) == codes[1] ~
          !!pos - lag(!!pos, 3),
        !!group == codes[2] & lag(!!group, 4) == codes[1] ~
          !!pos - lag(!!pos, 4),
        !!group == codes[2] & lag(!!group, 5) == codes[1] ~
          !!pos - lag(!!pos, 5),
        !!group == codes[2] & lag(!!group, 6) == codes[1] ~
          !!pos - lag(!!pos, 6),
        !!group == codes[2] & lag(!!group, 7) == codes[1] ~
          !!pos - lag(!!pos, 7),
        !!group == codes[2] & lag(!!group, 8) == codes[1] ~
          !!pos - lag(!!pos, 8),
        !!group == codes[2] & lag(!!group, 9) == codes[1] ~
          !!pos - lag(!!pos, 9),
        !!group == codes[2] & lag(!!group, 10) == codes[1] ~
          !!pos - lag(!!pos, 10)
      ),
      t2_t1 = pmin(.data$t2_t1_lr, .data$t2_t1_rl, na.rm = TRUE),
      # Record contact distance. If code 1 to code 2 contact it is negative
      # otherwise positive.
      distance = ifelse(is.na(.data$t1_t2), -.data$t2_t1, .data$t1_t2),
      # Create a group field to identify code 1 or code 2 distance. Used mainly
      # for plotting.
      group = factor(
        ifelse(is.na(.data$t1_t2), codes[2], codes[1]),
        levels = c(codes[2], codes[1]), ordered = TRUE)
    ) %>%
    # Remove temporary fields.
    select(-c(.data$t1_t2_lr, .data$t1_t2_rl, .data$t1_t2, .data$t2_t1_lr,
      .data$t2_t1_rl, .data$t2_t1)) %>%
    # Remove NA distances (for types other than 1 or 2). Filter to maximum
    # distance.
    filter(!is.na(.data$distance), between(.data$distance, -max_dist, max_dist))

  # Create averaged data per distance (distance rounded to nearest 1).
  sum_data <- data %>%
    mutate(distance = round(.data$distance, 0)) %>%
    group_by(.data$distance, .data$group) %>%
    summarise(mean = mean(!!value), var = var(!!value), n = n())

  return(list(detail = data, summary = sum_data))

}
