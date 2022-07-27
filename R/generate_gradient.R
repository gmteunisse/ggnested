#' @impor tidyr plotwidgets
generate_gradient <- function(i, clr = "#008CF0", min_p = 0.05, max_p = 0.95){
  if (i == 0){
    return(c())
  }
  if (i == 1){
    return(clr)
  }
  
  # Convert colour to HSL
  clr_hsl <- col2hsl(clr)
  
  # Get the lightness points between 0 and 1 at which to generate shades and/or tints
  ps <- divide_range(min_p, max_p, i)
  
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






