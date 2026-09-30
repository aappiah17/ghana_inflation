library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggtext)
library(ggrepel)
library(scales)

theme_alfred <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +   # device sans, not Lato
    theme(
      plot.background   = element_rect(fill = "floralwhite", colour = NA),
      panel.background  = element_rect(fill = "floralwhite", colour = NA),
      panel.grid        = element_blank(),
      axis.ticks        = element_blank(),
      legend.position   = "none",
      plot.title         = element_markdown(hjust = 0.5, colour = "grey30", size = 16,
                                            face = "bold", margin = margin(b = 6)),
      plot.subtitle      = element_markdown(hjust = 0.5, colour = "grey55", size = 11,
                                            face = "bold", lineheight = 1.3,
                                            margin = margin(b = 14)),
      plot.caption       = element_text(hjust = 1, colour = "grey55", size = 10,
                                        face = "bold", margin = margin(t = 12)),
      axis.text          = element_text(colour = "grey35", size = 12, face = "bold"),
      axis.title         = element_text(colour = "grey35", size = 12, face = "bold"),
      axis.title.x       = element_text(margin = margin(t = 10)),
      axis.title.y       = element_text(margin = margin(r = 10)),
      plot.title.position = "plot",
      plot.margin        = margin(16, 20, 12, 16)
    )
}