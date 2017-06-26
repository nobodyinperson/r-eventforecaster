#' Get a PRNG from a sample
#'
#' \code{prng_from_sample} takes an arbitrary sample of data and returns a
#' pseudo-random number generator.
#'
#' @param x the sample, an numeric vector
#' @param cdf the cdf of the sample's distribution. If unspecified, the
#'     empirical cdf is determined with \code{ecdf}.
#' @param xmin the lowest possible value of the sample's distribution.
#'     Defaults to \code{min(x)}.
#' @param xmax the highest possible value of the sample's distribution.
#'     Defaults to \code{max(x)}.
#'
#' @export
prng_from_sample <- function(
        x,
        cdf=ecdf(x),
        xmin=min(x,na.rm=T),
        xmax=max(x,na.rm=T)
        ) {
    x_s <- c(xmin,sort(x),xmin)
    y_s <- c(0,cdf(x),1)
    inv <- approxfun( x = y_s, y = x_s, rule = 1)
    prng <- function(n) inv(runif(n))
    return(prng)
    }
