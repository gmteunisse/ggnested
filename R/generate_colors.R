#' Generate a color palette.
#'
#' This function generates \code{n} maximally separated colors with equal
#' saturation and brightness from a base color.
#'
#' @param n The number of colors to generate.
#' @param clr The base color from which to generate other colors.
#' @return A vector of \code{n} colors.
generate_colors <- function(n, clr = "#6495ed"){
  if (n == 1){
    return(clr)
  }
  clr.rgb <- t(col2rgb(clr))
  clr.hsv <- t(rgb2hsv(clr.rgb[1], clr.rgb[2], clr.rgb[3], 255))
  clrs <- 0:(n-1)
  clrs <- sapply(clrs, function(x){
    offset <- x/(n)
    h <- (clr.hsv[1] + offset) %% 1
    s <- clr.hsv[2]
    v <- clr.hsv[3]
    clr <- hsv(h, s, v)
    return(clr)
  })
  clrs <- interleave_colors(clrs)
  return(clrs)
}

#' Reshuffle a list of colors.
#'
#' This function reshuffles a vector of colours so that in a list of \code{n}
#' colors, every color \code{i} in \code{\{1, n / 2\}} will be placed next
#' to color \code{i + n / 2}. This is useful for visualization of
#' automatically generated color palettes based upon for example gradients
#' or HSL color circles.
#'
#' @param clrs A vector of colors to reshuffle.
#' @return A reshuffled vector of \code{clrs}.
interleave_colors <- function(clrs){
  n.col <- length(clrs)
  ordr <- rep(1:ceiling(n.col/2), each = 2, length.out = n.col)
  indx <- seq(2, length(ordr), 2)
  ordr[indx] <- ordr[indx] + ceiling(n.col/2)
  clrs <- clrs[ordr]
  return(clrs)
}

