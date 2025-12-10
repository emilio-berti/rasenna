#' @title Cluster Locations
#' @description Cluster the location data into groups.
#' @details Clustering is done using the K-mean algorithm from
#'  `stats::kmean()`. The optimal number of clusters is automatically
#'  calculated as the number of clusters that give an explained
#'  variance in the data greater or equal to 0.80. Explained variance
#'  is the ratio of the between sum of squared divided by the total 
#'  sum of squares.
#' @importFrom stats kmeans
#' @param df data.frame of the track.
#' @return The original data.frame with a new column 'cluster'.
#' @examples
#'  data(capra)
#'  capra <- capra[-c(1, 2), ] # drop release
#'  capra <- clusterize(capra)
#'  plot(capra$x, capra$y, col = as.factor(capra$cluster))
clusterize <- function(df) {
  
  stopifnot("x" %in% colnames(df))
  stopifnot("y" %in% colnames(df))

  xy <- df[, c("x", "y")]

  K <- seq(1, 100)
  expl_var <- 0
  k <- 0

  while(expl_var < 0.8) {
    k <- k + 1
    cl <- kmeans(xy, centers = k)
    expl_var <- cl$betweenss / cl$totss
  }

  df[["cluster"]] <- cl$cluster

  return(df)

}
