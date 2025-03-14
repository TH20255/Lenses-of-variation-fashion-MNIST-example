library(ggplot2)
library(reshape2)

print_image_matrix <- function(x, idx_mat, name = "image") {
  x1 <- nrow(idx_mat)  
  x2 <- ncol(idx_mat)  
  n  <- x1 * x2       
  p  <- ncol(x)

  flatten_idx <- as.vector(t(idx_mat))

  img_side <- sqrt(p)

  plotdata <- array(NA, dim = c(n, img_side, img_side))

  for (k in seq_len(n)) {
    row_idx <- flatten_idx[k]
    if (is.na(row_idx)) {
      plotdata[k, , ] <- 0
    } else {
      plotdata[k, , ] <- x[row_idx, ]
    }
  }

  plot_melted <- melt(plotdata, varnames = c("image", "x", "y"), value.name = "intensity")

  digit_plot <- ggplot(plot_melted, aes(x = x, y = y, fill = intensity / 255)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "black", na.value = "white") +
    scale_y_reverse() +
    facet_wrap(~ image, nrow = x1, ncol = x2) +
    theme(
      panel.background = element_blank(),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      panel.spacing = unit(0, "lines"),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(x = NULL, y = NULL)

  ggsave(
    filename = paste0(name, ".png"),
    plot = digit_plot,
    width = 2.5 * x2 + 1.2,
    height = 3 * x1 + 0.3,
    dpi = 100,
    limitsize = FALSE
  )
}

