
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggnested

`ggnested` is a wrapper function around `ggplot2` that enables users to
display data with a two-level nested or clustered structure. It uses
colours or fills to label the main group or cluster, and a gradient of
shades and tints of each colour to label subgroups within each main
group. It is particularly useful for data with a hierarchy, such as
taxonomic annotations or geographic locations.

This is one of the core functions of the `fantaxtic` package, which
visualizes taxonomic ranks of amplicon sequencing / microbiome data as
stored in `phyloseq` objects. However, as it has utility beyond
displaying taxonomic annotations, it has been implemented as a separate
package.

**Keywords: R, ggplot2, nested data, clustered data, multiple levels,
shades, tints, gradient**

## Installation

``` r
if(!"devtools" %in% installed.packages()){
  install.packages("devtools")
}
devtools::install_github("gmteunisse/ggnested")
```

## Usage

### Basic

`ggnested` can be used exactly like the regular `ggplot` function, but
with some extra arguments. Instead of specifying a color or a fill, a
`main_group` and a `sub_group` need to be specified in the aesthetic
mapping through `aes(...)`. The ggplot object can then be used to create
any regular ggplot plot, such as barplots, scatterplots or boxplots, as
well as any other layers such as faceting.

``` r
library(ggnested)
#> Loading required package: tidyr
#> Loading required package: dplyr
#> Warning: package 'dplyr' was built under R version 4.1.3
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#> Loading required package: purrr
#> Loading required package: magrittr
#> 
#> Attaching package: 'magrittr'
#> The following object is masked from 'package:purrr':
#> 
#>     set_names
#> The following object is masked from 'package:tidyr':
#> 
#>     extract
#> Loading required package: plotwidgets
#> Warning: package 'plotwidgets' was built under R version 4.1.3
#> Loading required package: ggplot2
#> Warning: package 'ggplot2' was built under R version 4.1.3
#> Loading required package: ggtext
#> Warning: package 'ggtext' was built under R version 4.1.3
data(diamonds)
```

``` r
ggnested(diamonds, 
         aes(clarity, 
             main_group = color, 
             sub_group = cut)) + 
  geom_bar()
```

![](man/figures/README-barplot-1.png)<!-- -->

``` r
ggnested(diamonds, 
         aes(x = carat, 
             y = price, 
             main_group = color, 
             sub_group = cut)) + 
  geom_point(alpha = 0.5) +
  facet_wrap(~color)
```

![](man/figures/README-scatterplot-1.png)<!-- -->

``` r
ggnested(diamonds, 
         aes(x = color,
             y = price, 
             main_group = color, 
             sub_group = cut)) + 
  geom_boxplot(alpha = .5)
```

![](man/figures/README-boxplot-1.png)<!-- -->

### Options

A few options are available, most of which are related to the appearance
of the plot.

#### Legend

A few options are available to alter the way the legend is constructed.

-   The `legend_labeling` option alters the labels that are used in the
    legend. For example, it is possible to display both the `main_group`
    label and the `sub_group` label, or just the `main_group` label.
-   The `join_str` option is used if `legend_labeling = "join"`,and
    specifies a string to join the two labels.
-   The `legend_title` option alters the legend title.
-   The `main_keys` determines whether `main_group` labels are displayed
    as keys within the legend.

Bringing it all together:

``` r
ggnested(diamonds, 
         aes(clarity, 
             main_group = color, 
             sub_group = cut),
         legend_labeling = "join",
         join_str = ": ",
         legend_title = "Joined labels without titles",
         main_keys = FALSE) + 
  geom_bar()
```

![](man/figures/README-legend_labeling-1.png)<!-- -->

#### Colour palette

`ggnested` creates its own colour palettes. Like ggplot2, it samples
maximally separated colours, but unlike ggplot2, it shuffles the palette
so that dissimlar colours are placed next to one another. This is to
avoid confusion in colours and shades between different groups. Also
unlike ggplot2, the user can define a base colour
(e.g. `base_clr = #008CF0`) that serves as the first colour of the
palette, based on which all other colours are sampled. Finally, A custom
palette of colours for the `main_group` can be provided
(`main_palette = c()`), which will then be used to generate gradients
for the subgroups.

``` r
ggnested(diamonds, 
         aes(clarity, 
             main_group = color, 
             sub_group = cut),
         base_clr = "red") + 
  geom_bar()
```

![](man/figures/README-base_clr-1.png)<!-- -->

Note that if a custom palette is named, the colours will be associated
with the appropriate `main_group` level.

``` r
pal <- c("red", "blue", "green", "yellow", "purple", "orange", "grey50")
names(pal) <- unique(diamonds$color)
ggnested(diamonds, 
         aes(clarity, 
             main_group = color, 
             sub_group = cut), 
         main_palette = pal) + 
  geom_bar()
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

#### Affected aethetics

By default, both the `color` and `fill` aesthetics of the plot are
altered by `ggnested`. However, it is possible to specify which
aesthetics should be altered using `nested_aes = c("fill", "colour")`.
Note that it leads to issues with the legends of geoms that
automatically create both `colour` and `fill` aethetics, as shown with
the boxplots below - the `main_group` titles now also show a box
outline! This can of course be resolved by setting `main_keys = FALSE`
as shown above.

``` r
ggnested(diamonds, 
         aes(x = color,
             y = price, 
             main_group = color, 
             sub_group = cut),
         nested_aes = "fill") + 
  geom_boxplot()
```

![](man/figures/README-nested_aes-1.png)<!-- -->

### Themes

To ensure that a `ggnested` plot does not alter when the theme is
altered, it is recommended to alter the theme using the `theme_nested`
function. This function is simply a wrapper around any ggplot `theme_*`
function.

``` r
ggnested(diamonds, 
         aes(clarity, 
             main_group = color, 
             sub_group = cut)) + 
  geom_bar() +
  theme_nested(theme_minimal)
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->
