# Functions for contact analysis.

#' Generate contact analysis data from samples.
#' @author Alex M. Trueman, 2018-03-21
#'
#' @param df Data frame containing sample data with required fields.
#' @param id ID code for line or drillhole (e.g., bhid).
#' @param pos Position of sample along the hole (i.e., depth; [to - from] / 2)
#' @param type Domain or zone code field (character or numeric field)
#' @param type1 Type code 1 for analysis, negative distance in final data.
#' @param type2 Type code 2 for analysis, positive distance in final data.
#' @param value A value field such as grade to analyse.
#' @param max_dist Maximum contact evaluation distance (default is 50 m)
#'
#' @return A list of two data frames. Detail contains un-averaged raw data.
#'     Summary is averaged by distance.
#' @export
#' @importFrom dplyr arrange between case_when filter group_by lag lead mutate
#'     select summarise
#' @importFrom magrittr %>%
#' @importFrom stats var
#' @importFrom rlang enquo
#' @examples
#' x <-  dplyr::mutate(dholes, depth = (to - from) / 2)
#' contact_analysis(x, bhid, depth, domain, 1100, 1001, grade)
contact_analysis <- function(
    df, id, pos, type, type1, type2, value, max_dist = 50) {

  # Capture passed data frame fields as R code rather than literal values.
  id <- enquo(id)
  pos <- enquo(pos)
  type <- enquo(type)
  value <- enquo(value)

  # Process the input data frame.
  data <- df %>%

    # Sort by hole id and sample position.
    arrange(!!id, !!pos) %>%

    # Group by hole as analysis is per hole.
    group_by(!!id) %>%

    mutate(
      # Traverse along hole in increasing position (left to right) looking for
      # type 1 to type 2 contacts.
      t1_t2_lr = case_when(
        # Find a type 1 code where the next code is type 2 set variable to
        # difference in position.
        !!type == type1 & lead(!!type, 1) == type2 ~ lead(!!pos, 1) - !!pos,
        # Repeat above but for two samples ahead, then three, up to ten...
        !!type == type1 & lead(!!type, 2) == type2 ~ lead(!!pos, 2) - !!pos,
        !!type == type1 & lead(!!type, 3) == type2 ~ lead(!!pos, 3) - !!pos,
        !!type == type1 & lead(!!type, 4) == type2 ~ lead(!!pos, 4) - !!pos,
        !!type == type1 & lead(!!type, 5) == type2 ~ lead(!!pos, 5) - !!pos,
        !!type == type1 & lead(!!type, 6) == type2 ~ lead(!!pos, 6) - !!pos,
        !!type == type1 & lead(!!type, 7) == type2 ~ lead(!!pos, 7) - !!pos,
        !!type == type1 & lead(!!type, 8) == type2 ~ lead(!!pos, 8) - !!pos,
        !!type == type1 & lead(!!type, 9) == type2 ~ lead(!!pos, 9) - !!pos,
        !!type == type1 & lead(!!type, 10) == type2 ~ lead(!!pos, 10) - !!pos
      ),
      # Now traverse back the other way (right to left) in the same fasion.
      t1_t2_rl = case_when(
        !!type == type1 & lag(!!type, 1) == type2 ~ !!pos - lag(!!pos, 1),
        !!type == type1 & lag(!!type, 2) == type2 ~ !!pos - lag(!!pos, 2),
        !!type == type1 & lag(!!type, 3) == type2 ~ !!pos - lag(!!pos, 3),
        !!type == type1 & lag(!!type, 4) == type2 ~ !!pos - lag(!!pos, 4),
        !!type == type1 & lag(!!type, 5) == type2 ~ !!pos - lag(!!pos, 5),
        !!type == type1 & lag(!!type, 6) == type2 ~ !!pos - lag(!!pos, 6),
        !!type == type1 & lag(!!type, 7) == type2 ~ !!pos - lag(!!pos, 7),
        !!type == type1 & lag(!!type, 8) == type2 ~ !!pos - lag(!!pos, 8),
        !!type == type1 & lag(!!type, 9) == type2 ~ !!pos - lag(!!pos, 9),
        !!type == type1 & lag(!!type, 10) == type2 ~ !!pos - lag(!!pos, 10)
      ),

      # Record the minimum distance from the two traversals.
      t1_t2 = pmin(t1_t2_lr, t1_t2_rl, na.rm = TRUE),

      # Traverse along hole in increasing position looking for type 2 to type 1
      # contacts.
      t2_t1_lr = case_when(
        !!type == type2 & lead(!!type, 1) == type1 ~ lead(!!pos, 1) - !!pos,
        !!type == type2 & lead(!!type, 2) == type1 ~ lead(!!pos, 2) - !!pos,
        !!type == type2 & lead(!!type, 3) == type1 ~ lead(!!pos, 3) - !!pos,
        !!type == type2 & lead(!!type, 4) == type1 ~ lead(!!pos, 4) - !!pos,
        !!type == type2 & lead(!!type, 5) == type1 ~ lead(!!pos, 5) - !!pos,
        !!type == type2 & lead(!!type, 6) == type1 ~ lead(!!pos, 6) - !!pos,
        !!type == type2 & lead(!!type, 7) == type1 ~ lead(!!pos, 7) - !!pos,
        !!type == type2 & lead(!!type, 8) == type1 ~ lead(!!pos, 8) - !!pos,
        !!type == type2 & lead(!!type, 9) == type1 ~ lead(!!pos, 9) - !!pos,
        !!type == type2 & lead(!!type, 10) == type1 ~ lead(!!pos, 10) - !!pos
      ),
      t2_t1_rl = case_when(
        !!type == type2 & lag(!!type, 1) == type1 ~ !!pos - lag(!!pos, 1),
        !!type == type2 & lag(!!type, 2) == type1 ~ !!pos - lag(!!pos, 2),
        !!type == type2 & lag(!!type, 3) == type1 ~ !!pos - lag(!!pos, 3),
        !!type == type2 & lag(!!type, 4) == type1 ~ !!pos - lag(!!pos, 4),
        !!type == type2 & lag(!!type, 5) == type1 ~ !!pos - lag(!!pos, 5),
        !!type == type2 & lag(!!type, 6) == type1 ~ !!pos - lag(!!pos, 6),
        !!type == type2 & lag(!!type, 7) == type1 ~ !!pos - lag(!!pos, 7),
        !!type == type2 & lag(!!type, 8) == type1 ~ !!pos - lag(!!pos, 8),
        !!type == type2 & lag(!!type, 9) == type1 ~ !!pos - lag(!!pos, 9),
        !!type == type2 & lag(!!type, 10) == type1 ~ !!pos - lag(!!pos, 10)
      ),
      t2_t1 = pmin(t2_t1_lr, t2_t1_rl, na.rm = TRUE),

      # Record contact distance. If type 1 to type 2 contact it is negative
      # otherwise positive.
      distance = ifelse(is.na(t1_t2), -t2_t1, t1_t2),

      # Create a group field to identify type 1 or type 2 distance. Used mainly
      # for plotting.
      group = factor(
        ifelse(is.na(t1_t2), type2, type1),
        levels = c(type2, type1), ordered = TRUE)
    ) %>%

    # Remove temporary fields.
    select(-c(t1_t2_lr, t1_t2_rl, t1_t2, t2_t1_lr, t2_t1_rl, t2_t1)) %>%

    # Remove NA distances (for types other than 1 or 2). Filter to maximum
    # distance.
    filter(!is.na(distance), between(distance, -max_dist, max_dist))

  # Create averaged data per distance (distance rounded to nearest 1).
  sum_data <- data %>%
    mutate(distance = round(distance, 0)) %>%
    group_by(distance, group) %>%
    summarise(mean = mean(!!value), var = var(!!value), n = n())

  return(list(detail = data, summary = sum_data))

}
