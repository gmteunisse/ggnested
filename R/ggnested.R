nested_palette <- function(data, group, subgroup, palette = NULL, base_clr = "#6495ed"){
  
  # Order by group and the count of each subgroup
  df <- data %>%
    arrange(!!sym(group), !!sym(subgroup)) %>%
    mutate(group_subgroup = sprintf("%s%s", !!sym(group), !!sym(subgroup)) %>%
             factor(ordered = T))
  
  # Generate palette
  pal <- nested_colours(df, group, subgroup, palette = palette, base_clr = base_clr)
  return(pal)
}

scale_nested <- function(data, group, subgroup, aesthetics = c("fill", "colour"), palette = NULL, base_clr = "#6495ed", ...){
  
  # Generate the nested palette
  pal <- nested_palette(data, group, subgroup, palette, base_clr)
  
  # Relevel
  df <- left_join(data, pal, by = c(group, subgroup)) %>%
    mutate(group_subgroup = factor(group_subgroup, ordered = T),
           !!subgroup := factor(!!sym(subgroup), ordered = T),
           !!group := factor(!!sym(group), ordered = T))
  
  # Extract colours
  colours <- pal %>%
    rename(sublabel = !!subgroup,
           label = !!group) %>%
    as.data.frame()
  
  # Return a scale
  scale_nested <- scale_discrete_manual(..., aesthetics = aesthetics, name = subgroup, values = colours$subgroup_colour, labels = colours$sublabel)
  return(scale_nested)
}

ggnested <- function(data, mapping = aes(), ..., palette = NULL, base_clr = "#6495ED"){
  
  # Check if mapping has required args
  aes_args <- names(mapping)
  if (!"main_group" %in% aes_args){
    stop("Error: provide the main_group in the aesthetic mapping argument. For non-nested data, use the regular ggplot2 function.")
  }
  if (!"sub_group" %in% aes_args){
    stop("Error: provide a subgroup in the aesthetic mapping argument. For non-nested data, use the regular ggplot2 function.")
  }
  
  # Show warning
  if ("fill" %in% aes_args | "colour" %in% aes_args | "color" %in% aes_args){
    warning("Warning: fill and colour aesthetics will be ignored in the main ggnested function. Please specify fill or colour in the geom_* layer.")
  }
  
  # Define group and subgroup
  group <- quo_name(mapping$main_group)
  subgroup <- quo_name(mapping$sub_group)
  
  # Generate the nested palette
  pal <- nested_palette(data, group, subgroup, palette, base_clr)
  
  # Relevel
  df <- left_join(data, pal, by = c(group, subgroup)) %>%
    mutate(group_subgroup = factor(group_subgroup, ordered = T),
           !!subgroup := factor(!!sym(subgroup), ordered = T),
           !!group := factor(!!sym(group), ordered = T))
  
  # Extract colours
  colours <- pal %>%
    rename(sublabel = !!subgroup,
           label = !!group) %>%
    as.data.frame()
  
  # Generate a scale
  nested_scale <- scale_discrete_manual(..., 
                                        aesthetics = c("fill", "colour"), 
                                        name = subgroup, 
                                        values = colours$subgroup_colour, 
                                        labels = colours$sublabel)
  # Update mapping
  mapping$fill <- quo(group_subgroup)
  mapping$colour <- quo(group_subgroup)
  
  # Generate the plot
  p <- ggplot(df, mapping, ...) +
    nested_scale
  return(p)
}


ggnested_2 <- function(data, mapping = aes(), ..., palette = NULL, base_clr = "#6495ED"){
  
  # Check if mapping has required args
  aes_args <- names(mapping)
  if (!"main_group" %in% aes_args){
    stop("Error: provide the main_group in the aesthetic mapping argument. For non-nested data, use the regular ggplot2 function.")
  }
  if (!"sub_group" %in% aes_args){
    stop("Error: provide a subgroup in the aesthetic mapping argument. For non-nested data, use the regular ggplot2 function.")
  }
  
  # Show warning
  if ("fill" %in% aes_args | "colour" %in% aes_args | "color" %in% aes_args){
    warning("Warning: fill and colour aesthetics will be ignored in the main ggnested function. Please specify fill or colour in the geom_* layer.")
  }
  
  # Define group and subgroup
  group <- quo_name(mapping$main_group)
  subgroup <- quo_name(mapping$sub_group)
  
  # Generate the nested palette
  pal <- nested_palette(data, group, subgroup, palette, base_clr)
  
  # Relevel
  df <- left_join(data, pal, by = c(group, subgroup)) %>%
    mutate(group_subgroup = factor(group_subgroup, ordered = T),
           !!subgroup := factor(!!sym(subgroup), ordered = T),
           !!group := factor(!!sym(group), ordered = T))
  
  # Extract colours
  colours <- pal %>%
    rename(sublabel = !!subgroup,
           label = !!group) %>%
    as.data.frame()
  
  # Generate a scale
  nested_scale <- scale_discrete_manual(..., 
                                        aesthetics = c("fill", "colour"), 
                                        name = subgroup, 
                                        values = colours$subgroup_colour, 
                                        labels = colours$sublabel)
  # Update mapping
  mapping$fill <- quo(group_subgroup)
  mapping$colour <- quo(group_subgroup)
  
  # Generate the plot
  p <- ggplot(df, mapping, ...) +
    nested_scale
  return(p)
}
