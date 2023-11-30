#' GPS data for Belcho
#'
#' A dataset containing the GPS data for Belcho, a white stork in Bulgaria.
#' This dataset was obtained from the White Stork Bulgaria project, funded
#' by the Green Balkans Federation of Nature Conservation NGOs.
#' Data was accessed through movebank on November 2023.
#'
#' @format A data frame with 3756 rows and 4 variables:
#' \describe{
#'   \item{x}{Longitude coordinates in WGS84}
#'   \item{y}{Latitude coordinate in WGS84}
#'   \item{temp}{External temperature in Celsius degrees}
#'   \item{timestamp}{Character with the time of the GPS records:
#'    YY-MM-DD hh:mm::ss.dd}
#'   ...
#' }
"belcho"

#' GPS data for Capra Ibex in Western Alps
#'
#' A dataset containing the GPS data for one individual of Capra ibex from
#' Western Alps.
#' This dataset was obtained from the  project ALCOTRA LEMED-IBEX.
#' Data was accessed through movebank on November 2023.
#'
#' @format A data frame with 7932 rows and 3 variables:
#' \describe{
#'   \item{x}{Longitude coordinates in EPSG:7791}
#'   \item{y}{Latitude coordinate in EPSG:7791}
#'   \item{timestamp}{Character with the time of the GPS records:
#'    YY-MM-DD hh:mm::ss.dd}
#'   ...
#' }
"capra"
