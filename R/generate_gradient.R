#' @import tidyr plotwidgets grDevices
generate_gradient <- function(i, clr = "#008CF0", type = c("both", "shades", "tints", "fancy"), min_l = 0.05, max_l = 0.98, min_c = 50, max_c = 90, power = 1){
  
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
  # @param type whether to generate shades, tints, both, or a fancy gradient
  # @param min_l The minimum lightness of the gradient
  # @param max_l the maximum lightness of the gradient
  # @param min_c The minimum chroma value of the gradient
  # @param max_c the maximum chroma value of the gradient
  # @param power control parameter determining how chroma and luminance need to be increased (1=linear, 2= quadratic, etc.)
  # @return A vector of \code{i} colors.
  
  if (i == 0){
    return(c())
  }
  if (i == 1){
    return(clr)
  }
  
  # Convert colour to HSL
  clr_hsl <- col2hsl(clr)
  
  # Evaluate gradient type
  type <- type[1]
  if (type == "fancy"){
    clrs <- sequential_hcl(n = i, 
                           h = clr_hsl['H',1],
                           c = c(min_c, max_c),
                           l = 100 * c(min_l, max_l),
                           power = power)
    
  } else {
    # Get the lightness points between 0 and 1 at which to generate shades and/or tints
    if(type == "both"){
      ps <- divide_range(min_l, max_l, i)
    } else if (type == "shades"){
      ps <- divide_range(min_l, clr_hsl["L",], i)
    } else if (type == "tints"){
      ps <- divide_range(clr_hsl["L",], max_l, i)
    } else {
      msg <- sprintf("Did not recognize type = %s. Choose one of 'both', 'shades' or 'tints")
      stop(msg)
    }
    
    # Generate gradient
    low <- clr_hsl
    low["L",] <- ps[1]
    high <- clr_hsl
    high["L",] <- ps[i]
    clrs <- colorRampPalette(colors = c(hsl2col(low), 
                                        hsl2col(high)),
                             bias = 2,
                             interpolate = "spline")(i)
  }
  
  return(clrs)
}

#' @import tidyr plotwidgets grDevices colorspace
generate_gradient_2 <- function(i, clr = "#008CF0", min_l = 10, max_l = 98, min_c = 20, max_c = 200, pwr = 1){
  
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
  # @param type whether to generate shades, tints, or both
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
  
  clrs <- sequential_hcl(n = i, 
                 h = clr_hsl['H',1],
                 c = c(min_c, max_c),
                 l = c(min_l, max_l),
                 power = pwr)
  return(clrs)
}

# Generates n equidistant points for a range. Does not include min and max.
divide_range <- function(min, max, n){
  size <- (max - min) / (n + 1)
  return(min + 1:n * size)
}






