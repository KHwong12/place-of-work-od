library(ggplot2)
library(scales)

theme_set(
  # Pick a starting theme
  theme_minimal(base_family = "Work Sans") +
    # Add your favourite elements
    theme(
      text = element_text(family = "Work Sans"),
      
      plot.title.position = 'plot',
      plot.margin = margin(25, 25, 10, 25),
      
      panel.grid.major.y = element_line(linewidth = .5),
      
      axis.line.y = element_blank(),
      
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      
      axis.ticks.y = element_blank(),
      
      axis.text.y = element_text(vjust = 0),
      
      legend.position = "top",
      legend.text = element_text(size = 12),
      
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      
      plot.caption = element_text(hjust = 0),
      plot.caption.position = 'plot'
    )
)
