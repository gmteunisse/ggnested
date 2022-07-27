#' @import dplyr tidyr purrr
#' @export
nested_palette <- function(data, group, subgroup, palette = NULL, base_clr = "#008CF0", join_str = "_"){
  
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
  
  # Order by group and the count of each subgroup
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
    clr_pal <- palette[1:n_clrs]
  }

  # Assign group colours
  if (!is.null(names(clr_pal))){
    if (all(groups %in% names(clr_pal))){
      group_colours <- data.frame(group = names(clr_pal),
                                  group_colour = clr_pal)
    } else {
      stop("Error: palette names don't correspond to group levels.")
    }
  } else {
    group_colours <- data.frame(group = groups,
                                group_colour = clr_pal)
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
    mutate(subgroup_colour = map(data, ~generate_gradient(.x[["n"]][1], .x[["group_colour"]][1]))) %>%
    unnest(c(data, subgroup_colour)) %>%
    ungroup() %>%
    mutate(group_subgroup = sprintf("%s%s%s", !!sym(group), join_str, !!sym(subgroup))) %>%
    select(!n)

  return(clr_tbl)

}
