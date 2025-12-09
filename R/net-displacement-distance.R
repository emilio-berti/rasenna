#' @title Net Squared Displacement
#' @description The net squared displacement (NSD) quantifies how
#'  far away is the animal from the starting point of the track.
#' @details NSD is calculated as the distance between the start of the 
#'  track and the relocation.
#' @param df data.frame of the track.
#' @return Numeric vector of NSD.
#' @examples
#' data(capra)
#' capra$timestamp <- as.POSIXct(capra[["timestamp"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#' capra <- capra[-c(1, 2), ] # first two locations are likely release
#' capra <- resample(capra, freq = 24, units = "hours") # daily
#' nsd <- net_squared_displ(capra)
#' plot(capra$timestamp, nsd, xlab = "Date", ylab = "NSD")
net_squared_displ <- function(df) {

  stopifnot("x" %in% colnames(df))
  stopifnot("y" %in% colnames(df))

  dx <- df$x - df$x[1]
  dy <- df$y - df$y[1]
  d <- sqrt(dx ^ 2 + dy ^ 2)
  return(d)
  
}

#' @title Straightness Index
#' @description The straightness Index quantifies how straight is the 
#'  movement of the animal.
#' @details Straightness Index is calculated as the ratio of the squared root of
#'  NSD and the path length (sum of relocation distance) of the track.
#' @param df data.frame of the track.
#' @return Numeric vector of straightness Index.
#' @examples
#' data(capra)
#' capra$timestamp <- as.POSIXct(capra[["timestamp"]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
#' capra <- capra[-c(1, 2), ] # first two locations are likely release
straightness <- function(df) {

  stopifnot("x" %in% colnames(df))
  stopifnot("y" %in% colnames(df))

  d <- distance(df)[-1]
  NSD <- net_squared_displ(df)[-1]

  out <- sqrt(NSD) / cumsum(d)

  return(out)
  
}
