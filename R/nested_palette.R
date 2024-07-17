#' Generate a nested palette
#' 
#' @description
#' This function generates colours based on a grouping, and shades and tints 
#' of each colour based on a subgrouping.
#' 
#'  @details
#'  The 'fancy' gradient is generated using the \code{colorspace} package and needs
#'  fine tuningas, some parameter combinations do not yield a monochromal gradient.
#'
#' @param data A dataframe with at least two columns
#' @param group Name of the main grouping variable in the dataframe
#' @param subgroup Name of the subgrouping variable in the dataframe
#' @param gradient_type whether to generate shades, tints, both or a fancy gradient for each main
#' colour. Fancy adjusts both luminance and chroma using the colorspace
#' package.
#' @param min_l the lowest lightness value between 0 and 1 of each gradient if 
#' \code{type \%in\% c('both', 'shades')}, where 0 equals black and 1 equals white.
#' @param max_l the highest lightness value between 0 and 1 of each gradient if 
#' \code{type \%in\% c('both', 'tints')}, where 0 equals black and 1 equals white.
#' @param min_c the lowest chroma value between 0 and 360 of each gradient if 
#' \code{type == 'fancy'}.
#' @param max_c the highest chroma value between 0 and 360 of each gradient if 
#' \code{type == 'fancy'}.
#' @param power control parameter determining how chroma and luminance need to be 
#' increased (1=linear, 2= quadratic, etc.) if \code{type == 'fancy'}.
#' @param palette An optional palette of at least as many colours as there are 
#' unique values in the grouping variable. If named, names are assumed to corresponds
#' to \code{main_group} levels.
#' @param base_clr A colour from which to generate other colours if no palette 
#' is provided
#' @param join_str The string used to join the group and subgroup variables
#' @return A dataframe with each group and its colour, each subgroup with its 
#' colour and the joined group_subgroup.
#' @import dplyr tidyr purrr
#' @importFrom magrittr %>%
#' @export
nested_palette <- function(data, group, subgroup, gradient_type = c("both", "shades", "tints", "fancy"),
                           min_l = 0.05, max_l = 0.98, palette = NULL, base_clr = "#008CF0", 
                           join_str = "_",  min_c = 50, max_c = 90, power = 1){
  
  # Check arguments
  if (!group %in% colnames(data)){
    msg <- sprintf("Error: %s not in colnames(data)", group)
    stop(msg)
  }
  if (!subgroup %in% colnames(data)){
    msg <- sprintf("Error: %s not in colnames(data)", subgroup)
    stop(msg)
  }
  if (!is.data.frame(data)){
    msg <- sprintf("Error: df needs to be class 'data.frame', instead got '%s'", paste(class(data), collapse = ", "))
    stop(msg)
  }
  
  # Order by group and subgroup
  df <- data %>%
    arrange(!!sym(group), !!sym(subgroup)) %>%
    mutate(group_subgroup = sprintf("%s%s", !!sym(group), !!sym(subgroup)) %>%
             factor(ordered = T))

  # Define the number of variants required per color
  groups <- df %>%
    pull(!!group) %>%
    unique()
  n_clrs <- groups %>%
    length()

  # Generate colors if required
  if (is.null(palette)){
    clr_pal <- generate_colors(n_clrs, base_clr)
  } else {
    if (length(palette) < n_clrs){
      stop(sprintf("Error: %d values required in clr.pal, %d provided.", n_clrs, length(palette)))
    }
    clr_pal <- palette
  }

  # Assign group colours
  if (!is.null(names(clr_pal))){
    if (all(groups %in% names(clr_pal))){
      group_colours <- data.frame(group = groups,
                                  group_colour = unname(clr_pal[as.character(groups)]))
    } else {
      stop("Error: palette names don't correspond to group levels.")
    }
  } else {
    group_colours <- data.frame(group = groups,
                                group_colour = clr_pal[1:n_clrs])
  }

  # Rename to group name
  group_colours <- group_colours %>%
    rename(!!group := "group")

  # Assign subgroup colours
  clr_tbl <- df %>%
    select(!!group, !!subgroup) %>%
    group_by(across(all_of(!!group))) %>%
    unique() %>%
    mutate(n = n()) %>%
    left_join(group_colours, by = group) %>%
    nest() %>%
    mutate(subgroup_colour = map(data, ~generate_gradient(i = .x[["n"]][1], 
                                                          clr = .x[["group_colour"]][1],
                                                          type = gradient_type,
                                                          min_l = min_l,
                                                          max_l = max_l,
                                                          min_c = min_c,
                                                          max_c = max_c,
                                                          power = power))) %>%
    unnest(c(data, subgroup_colour)) %>%
    ungroup() %>%
    mutate(group_subgroup = sprintf("%s%s%s", !!sym(group), join_str, !!sym(subgroup))) %>%
    select(!n)

  return(clr_tbl)

}

#' Extract palette from a ggnested object
#' 
#' @description
#' This function extracts the colour palette used in a ggnested object.
#' 
#' @param ggnested_obj A ggplot2 object generated by ggnested
#' @return A dataframe with the group_subgroup label, the main group colours
#'  and the subgroup colours.
#' @import dplyr tidyr
#' @importFrom magrittr %>%
#' @export
extract_palette <- function(ggnested_obj){
  ggnested_obj$data %>%
    select(group_subgroup, group_colour, subgroup_colour) %>%
    unique()
}
