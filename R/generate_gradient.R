#' @import tidyr plotwidgets
generate_gradient <- function(i, clr = "#008CF0", min_l = 0.05, max_l = 0.95){
  
  # Generate a gradient based on a colour
  # 
  # @description
  # This function generates \code{i} equidistant shades and tints
  # of a colour.
  # 
  # @details 
  # Converts the colour to HSL, and then varies the L value between \code{min_l} 
  # and \code{max_l} to generate a gradient. \code{min_l} and \code{max_l} are never 
  # included in the gradient to avoid large contrasts. The L value 
  # of the original colour is dropped and may also not be included.
  #
  # @param i The number of colors to generate.
  # @param clr The base color from which to generate the gradient
  # @param min_l The minimum lightness of the gradient
  # @param max_l the maximum lightness of the gradient
  # @return A vector of \code{i} colors.
  
  if (i == 0){
    return(c())
  }
  if (i == 1){
    return(clr)
  }
  
  # Convert colour to HSL
  clr_hsl <- col2hsl(clr)
  
  # Get the lightness points between 0 and 1 at which to generate shades and/or tints
  ps <- divide_range(min_l, max_l, i)
  
  # Generate shades and tints of colour
  clrs <- sapply(ps, function(p){
    clr_var <- clr_hsl
    clr_var["L",] <- p
    return(hsl2col(clr_var))
  })
  return(clrs)
}

# Generates n equidistant points for a range. Does not include min and max.
divide_range <- function(min, max, n){
  size <- (max - min) / (n + 1)
  return(min + 1:n * size)
}






