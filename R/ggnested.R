ggnested <- function(data, 
                     mapping = aes(), 
                     ...,
                     legend_labeling = c("sub", "join", "main"),
                     join_str = " - ",
                     legend_title = NULL,
                     main_keys = TRUE,
                     nested_aes = c("fill", "color"), 
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
  pal <- nested_palette(data, group, subgroup, main_palette, base_clr, join_str)
  
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

#' @import ggplot2
#' @export
theme_nested <- function(theme_fun = theme_minimal, ...){
  new_theme <- theme_fun(...) +
    theme(legend.text = element_markdown(),
          legend.key = element_rect(colour = "#FFFFFF",
                                    fill = "#FFFFFF"))
  return(new_theme)
}

