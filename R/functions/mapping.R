# R/functions/mapping.R
# Choropleth mapping wrapper for sf objects using ggplot2 + viridis.

mapping <- function(data = NULL, variable, ltitle, ptitle, stitle) {
  # Allow calling with or without explicit data= arg (original usage passed no data arg)
  if (is.null(data)) stop("mapping() requires a data argument (an sf object).")

  ggplot(data = data) +
    geom_sf(aes(fill = {{ variable }}), color = NA) +
    scale_fill_viridis_c(option = "magma", na.value = "grey80") +
    theme_void() +
    labs(
      fill     = ltitle,
      title    = ptitle,
      subtitle = stitle
    ) +
    theme(
      legend.position       = "right",
      legend.title          = element_text(size = 8, face = "bold"),
      legend.text           = element_text(size = 8),
      plot.title            = element_text(face = "bold", color = "black"),
      plot.subtitle         = element_text(face = "italic", color = "black")
    )
}
