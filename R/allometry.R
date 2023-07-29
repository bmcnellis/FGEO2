#' @title Allometry helper functions
#'
#' @description NA
#'
#' @rdname allometry
#' @name allometry
NULL
#' @rdname allometry
#' @export
dbh_to_height <- function(species, dbh) {
  # this is for WET species, add another nested switch for dry species
  # equations from Asner et al. (2011)
  stopifnot(length(dbh) == length(species))

  sapply(seq_along(species), \(xx) {
    sp <- species[xx]
    d <- dbh[xx]

    eq <- switch(sp,
                 'ACAKOA' = {
                   exp(0.1795 + (1.0160 * log(d)) - (0.0800 * (log(d)^2))) * 1.0156
                 },
                 'ANTPLA' = {
                   1.1240 + (8.5503 * (1 - exp(-0.1079 * d)))
                 },
                 'CHETRI' = {
                   12.6477 * (1 - exp(-0.1365 * d))
                 },
                 'CIBSPP' = {
                   exp(-0.4531 + (1.6955 * log(d))) * (1.2071 / 100)
                 },
                 'CIBGLA' = {
                   exp(-0.6277 + (1.6910 * log(d))) * (1.1386 / 100)
                 },
                 'CIBMEN' = {
                   exp(-0.6549 + (1.8683 * log(d))) * (1.1705 / 100)
                 },
                 'CIBCHA' = {
                   exp(0.6457 + (1.5932 * log(d))) * (1.2763 / 100)
                 },
                 'COPRHY' = {
                   10.2252 * (1 - exp(-0.2257 * d))
                   # same as COPSPP
                 },
                 'COPSPP' = {
                   10.2252 * (1 - exp(-0.2257 * d))
                 },
                 'ILEANO' = {
                   13.0821 * (1 - exp(-0.1339 * d))
                 },
                 'METPOL' = {
                   22.9975 * (1 - exp(-0.0452 * d))
                 },
                 'PSYHAW' = {
                   9.2527 * (1 - exp(-0.1863 * d))
                 },
                 {
                   # general wet as default
                   exp(0.5120 + (0.7583 * log(d)) - (0.0322 * (log(d)^2))) * 1.0409
                 }
    )

    return(eq)

  })
}
