#' Generate a ggplot2 object with a nested colour/fill structure
#' 
#' @description
#' This function generates a ggplot2 object for a dataframe with a nested or 
#' clustered structure. The main group or cluster is assigned a colour,
#'  and each subgroup within each main group is assigned a shade or tint of that 
#'  colour.
#'
#' @param data A dataframe
#' @param mapping Aesthetic mapping for the ggplot2 object. Specify both group 
#' and subgroup within the aesthetic mapping.
#' @param ... Arguments to be passed to the ggplot function
#' @param legend_labeling Labeling for legend keys. \code{main} displays the 
#' label of the main group; \code{sub} displays the labels of the subgroups; 
#' \code{join} displays the joined labels of the main and subgroups, separated 
#' by \code{join_str}.
#' @param join_str String used to join main and subgroup labels
#' @param legend_title Legend title
#' @param main_keys Logical. Display main groups as titles within the legend?
#' @param nested_aes The aethetics in c("fill", "colour") to which to apply the nested
#'  palette.
#' @param gradient_type whether to generate shades, tints, or both for each main
#' colour
#' @param min_l the lowest lightness value between 0 and 1 of each gradient if 
#' \code{type \%in\% c('both', 'shades')}, where 0 equals black and 1 equals white.
#' @param max_l the highest lightness value between 0 and 1 of each gradient if 
#' \code{type \%in\% c('both', 'tints')}, where 0 equals black and 1 equals white.
#' @param palette An optional palette of at least as many colours as there are 
#' @param main_palette An optional palette of at least as many colours as there  
#' are unique values in the main grouping variable. Names will be used to assign
#' colours if provided.
#' @param base_clr A colour from which to generate other colours if no palette 
#' is provided
#' @return A ggplot2 object
#' @import dplyr tidyr ggplot2
#' @importFrom magrittr %>%
#' @export
ggnested <- function(data, 
                     mapping = aes(), 
                     ...,
                     legend_labeling = c("sub", "join", "main"),
                     join_str = " - ",
                     legend_title = NULL,
                     main_keys = TRUE,
                     nested_aes = c("fill", "color"),
                     gradient_type = c("both", "shades", "tints"),
                     min_l = 0.05,
                     max_l = 0.95,
                     main_palette = NULL, 
                     base_clr = "#008CF0"
){
  
  # Check if mapping has required args
  aes_args <- names(mapping)
  if (!"main_group" %in% aes_args){
    stop("Error: provide the main_group in the aesthetic mapping argument. For non-nested data, use the regular ggplot2 function.")
  }
  if (!"sub_group" %in% aes_args){
    stop("Error: provide a subgroup in the aesthetic mapping argument. For non-nested data, use the regular ggplot2 function.")
  }
  
  # Show warnings when fill or colour are specified in aes
  if ("fill" %in% aes_args & "fill" %in% nested_aes){
    warning("Warning: fill aesthetics will be ignored in the main ggnested function. Please specify non-nested fill in the geom_* layer. Alternatively,
            remove 'fill' from mapping_aes.")
    mapping$fill <- NULL
  }
  if (("colour" %in% aes_args | "color" %in% aes_args) & ("colour" %in% nested_aes | "color" %in% nested_aes)){
    warning("Warning: colour aesthetics will be ignored in the main ggnested function. Please specify non-nested colour in the geom_* layer. Alternatively,
            remove 'colour' from mapping_aes.")
    mapping$colour <- NULL
    mapping$color <- NULL
  }
  
  # Define group and subgroup
  group <- quo_name(mapping$main_group)
  subgroup <- quo_name(mapping$sub_group)
  
  # Generate the nested palette
  pal <- nested_palette(data, group, subgroup, gradient_type, min_l, max_l, 
                        main_palette, base_clr, join_str)
  
  # Extract colours
  colours <- pal %>%
    rename(sublabel = !!subgroup,
           label = !!group) %>%
    as.data.frame()
  
  # Add main_group labels to the legend as extra keys that appear as titles
  if (main_keys){
    colours <- colours %>%
      group_by(label) %>% 
      group_modify(~add_row(.x, .before = 0)) %>%
      ungroup() %>%
      mutate(subgroup_colour = ifelse(is.na(subgroup_colour), "#FFFFFF", subgroup_colour),
             sublabel = ifelse(is.na(sublabel), sprintf("**%s**", as.character(label)), as.character(sublabel)),
             group_subgroup = ifelse(is.na(group_subgroup), sprintf("**%s**", as.character(label)), group_subgroup)) %>%
      as.data.frame() 
  }

  # Get the final colours
  vals <- colours$subgroup_colour
  names(vals) <- colours$group_subgroup
  
  # Reorder the data
  df <- left_join(data, pal, by = c(group, subgroup)) %>%
    arrange(group, subgroup) %>%
    mutate(group_subgroup = factor(group_subgroup, ordered = T, levels = colours$group_subgroup),
           !!subgroup := factor(!!sym(subgroup), ordered = T),
           !!group := factor(!!sym(group), ordered = T)) %>%
    ungroup() %>%
    arrange(group_subgroup)
  
  # Add legend labels and title
  if (legend_labeling[1] == "join"){
    labels <- colours$group_subgroup
    leg_title <- sprintf("%s%s%s", group, join_str, subgroup)
  } else if (legend_labeling[1] == "main"){
    labels <- colours$label
    leg_title <- group
  } else if (legend_labeling[1] == "sub"){
    labels <- colours$sublabel
    leg_title <- subgroup
  } else {
    stop("Invalid option for legend_labeling. Pick one of c('join', 'main', 'sub')")
  }
  
  if (!is.null(legend_title)){
    leg_title <- legend_title
  }
  
  # Generate a scale
  nested_scale <- scale_discrete_manual(..., 
                                        aesthetics = nested_aes, 
                                        name = leg_title, 
                                        values = vals, 
                                        labels = labels, 
                                        drop = F)
  
  # Update mapping
  if ("fill" %in% nested_aes){
    mapping$fill <- quo(group_subgroup)
  }
  if ("colour" %in% nested_aes | "color" %in% nested_aes){
    mapping$colour <- quo(group_subgroup)
  }
  
  # Generate the plot
  p <- ggplot(df, mapping, ...) +
    nested_scale
  if (main_keys){
    p <- p +
      theme_nested(theme)
  }
  return(p)
}

#' Add a ggplot2 theme to a ggnested plot
#' 
#' @description
#' This is a wrapper function around any  ggplot2 theme* function to modify the 
#' theme of a ggnested plot.
#'
#' @param theme_fun The theme* function to apply
#' @param ... Arguments to be passed to the theme* function
#' @return A ggplot2 theme
#' @import ggplot2 ggtext
#' @export
theme_nested <- function(theme_fun = theme, ...){
  new_theme <- theme_fun(...) +
    theme(legend.text = element_markdown(),
          legend.key = element_rect(colour = "#FFFFFF",
                                    fill = "#FFFFFF"))
  return(new_theme)
}

