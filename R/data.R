#' Drillhole data
#'
#' Drillhole data for providing examples of contact analysis.
#'
#' @docType data
#'
#' @usage data(dholes)
#'
#' @format A data frame with 24,673 rows and 11 columns:
#' \describe{
#' \item{\code{bhid}}{Drillhole identification}
#' \item{\code{from}}{Downhole depth to top of sample}
#' \item{\code{to}}{Downhole depth to botom of sample}
#' \item{\code{length}}{Length of sample}
#' \item{\code{x}}{Easting coordinate of sample centroid}
#' \item{\code{y}}{Northing coordinate of sample centroid}
#' \item{\code{z}}{Elevation coordinate of sample centroid}
#' \item{\code{dip}}{Sample dip from horizontal (positive down)}
#' \item{\code{dipdir}}{Dip direction}
#' \item{\code{domain}}{Stationary estimation domain code}
#' \item{\code{grade}}{Measured grade (concentration) of metal}
#' }
"dholes"
