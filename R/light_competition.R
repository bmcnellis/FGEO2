#' @title Light competition calculation function
#'
#' @description NA
#'
#' @rdname light_competition
#' @name light_competition
NULL
#' @rdname light_competition
#' @export
calc_light_index <- function(x, y, z, cz = 5, maxx = 200, maxy = 200) {
  # crs = 'epsg:32605'
  # z is height
  stopifnot(
    length(x) == length(y),
    length(y) == length(z)
  )

  c_ind <- sapply(seq_along(x), \(i) {

    # find trees that are +/- cz x and +/- cz y, because points are UTM
    # this step speeds up euclidian distance calculation
    xin <- which(x <= (x[i] + cz) & x >= (x[i] - cz))
    yin <- which(y <= (y[i] + cz) & y >= (y[i] - cz))

    # get trees within square boundary
    sq_ind <- xin[which(xin %in% yin)]
    sq_ind <- c(which(sq_ind == i), which(sq_ind != i))
    # fix edge effects
    if (F) FGEO2::fix_edge()
    #find euclidian distance
    sq_mat <- matrix(data = c(x[sq_ind], y[sq_ind]), ncol = 2)
    sq_vec <- as.matrix(dist(sq_mat))[, 1]
    # which trees are within the canopy circle?
    circ_vec <- sq_ind[which(sq_vec < cz)]

    # which of the potential trees are taller than the focus tree?
    sum(z[circ_vec] > z[i])

  })

  return(c_ind)

}
#' @rdname light_competition
#' @export
fix_edge <- function() {

  stop()

  xedgeL <- (x[i] - cz) < 0
  xedgeR <- (x[i] + cz) > maxx
  yedgeL <- (y[i] - cz) < 0
  yedgeR <- (y[i] + cz) > maxy

  if (xedgeL == T) {
    browser()
  }
  if (xedgeR == T) {
    browser()
  }
  if (yedgeL == T) {
    browser()
  }
  if (yedgeR == T) {
    browser()
  }
}
