# VISUALISATION FUNCTIONS
# ~~~~~~~~~~~~~~~~~~~~~~~~~

# Basic style guides to follow;
# 1. Keep decimals to one point in every graph
# 2. Ensure that the bars are always in decreasing order
# 3. Add caption to every graph, with the source (core/ CMIE survey), along with the number of observations (N)
# 4. Keep a single colour scale across graphs

# SOA THEME ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~
theme_soa <- function() {
  theme_minimal() %+replace%
    theme( 
      
      # Base family and size
      text = element_text(
        size = 7,
        family = "Proxima Nova A",
        # face = "plain", 
        color = "#707274", 
        hjust = 0.5, 
        vjust = 0.5, 
        angle = 0, 
        lineheight = 0.9,
        debug = FALSE,
        margin = margin(2, 2, 2, 2)
      ),
      
      # Removing titles, axis lines, and axis titles
      panel.grid = element_blank(),
      strip.background = element_blank(),
      axis.title = element_blank(),
      strip.text = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "pt"),
      
      # Moving legend
      legend.spacing.x = unit(0.05, "lines"),
      legend.title = element_text(
        size = 7,
        face = "bold",
        lineheight = 0.3,
        hjust = 0,
        vjust = 1,
      ),
      legend.text = element_text(
        size = 6,
        # face = "bold",
        lineheight = 0.3,
        hjust = 0
      ),
      legend.key.height = unit(0.6, "lines"),
      legend.key.width = unit(0.3, "lines"),
      
      # Default caption, title and subtitle
      # plot.title = element_text(
      #   size = 16,
      #   hjust = 0,
      #   face = "bold"
      # ),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      # plot.subtitle = element_text(
      #   size = 14,
      #   hjust = 0,
      #   face = "italic"
      # ),
      
      plot.caption = element_text(
        size = 4.5,
        lineheight = 1.01,
        hjust = 0,
        vjust = 0
      )

    )
}

# SOA COLOUR PALLETE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~
soa_colours <- list(
  "main" = "#33a3dc",
  "green" = "#55be8c",
  "orange" = "#f15b4e",
  "purple" = "#663695",
  "magenta" = "#cc3366",
  "yellow" = "#f68c3d"
)

soa_light <- list(
  "main" = "#add1ef",
  "green" = "#a7d8bd",
  "orange" = "#f9b8aa",
  "purple" = "#b7a0cc",
  "magenta" = "#e8a5b4",
  "yellow" = "#fecc8e"
)

soa_palette <- list(
  "main" = c("#e1eef9", "#add1ef", "#78b8e5", "#33a3dc"),
  "green" = c("#cbe8d8", "#a7d8bd", "#81caa3", "#55be8c"),
  "orange" = c("#fde8e6", "#f9b8aa", "#f58a77", "#f15b4e"),
  "purple" = c("#f0e5f1", "#b7a0cc", "#8b69ae", "#663695"),
  "magenta" = c("#fce0ec", "#e8a5b4", "#d97189", "#cc3366"),
  "yellow" = c("#ffeebd", "#fecc8e", "#faab66", "#f68c3d"),
  "blue_green" = c("#add1ef", "#a7d8bd"),
  "blue_yellow" = c("#add1ef", "#fecc8e"),
  "blue_purple" = c("#add1ef", "#b7a0cc"),
  "main6" = c("#ebf7fd", "#aedaf0", "#70bfe7", "#33a3dc", "#24729b", "#144158", "#0c212c")
)

soa_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- soa_palette[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

# SOA FILL AND COLOUR SCHEMES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~
scale_fill_soa <- function(palette = "main", 
                           discrete = TRUE,
                           reverse = FALSE,
                           ...) {
  pal <- soa_pal(palette = palette, reverse = reverse)
  
  if (discrete)
    discrete_scale("fill", paste0("soa_", palette), palette = pal, ...)
  else
    scale_fill_gradientn(colours = pal(256), ...)
}

scale_colour_soa <- function(palette = "main", 
                             discrete = TRUE,
                             reverse = FALSE,
                             ...) {
  pal <- soa_pal(palette = palette, reverse = reverse)
  
  if (discrete)
    discrete_scale("colour", paste0("soa_", palette), palette = pal, ...)
  else
    scale_colour_gradientn(colours = pal(256), ...)
}
