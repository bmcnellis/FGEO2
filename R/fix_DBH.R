#' @title Fix DBH
#'
#' @description Fixes negative DBHs by applying a `truncnorm` adjustment
#'
#' @rdname fix_DBH
#' @name fix_DBH
NULL
#' @rdname fix_DBH
#' @export
fix_DBH <- function(ddbh, dbh, fun = c('median', 'mode')) {

  stopifnot(
    require(truncnorm)
  )

  if (length(fun) != 1) {
    fun <- 'median'
  } else {
    stopifnot(fun %in% c('median', 'mode'))
  }

  r0 <- sapply(ddbh, \(x) {

    if (is.na(x)) {
      ret <- NA
    } else {

      # sd comes from paper on DBH measureent error, units are mm
      sd0 <- 0.927 + (0.0038 * (dbh[i] * 10))

      d0 <- truncnorm::rtruncnorm(1000, a = 0, mean = x * 10, sd = sd0)

      if (fun == 'median') {
        ret <- median(d0) / 10
      } else {
        ret <- FGEO2::mode0(d0) / 10
      }

    }

    ret

  })

}
#' @rdname fix_DBH
#' @export
mode0 <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#' @rdname fix_DBH
#' @export
fix_height <- function(height, max_h = 5, sd0 = 0.25) {
  # fixes unreasonably large heights of CIB* measurements from weird allometry
  # designed with same strategy as above
  require(truncnorm)

  r0 <- sapply(height, \(xx) {
    if (is.na(xx)) {
      NA
    } else {
      d0 <- truncnorm::rtruncnorm(1000, a = 0, b = max_h, mean = xx, sd = sd0)
      median(d0)
    }
  })

}
