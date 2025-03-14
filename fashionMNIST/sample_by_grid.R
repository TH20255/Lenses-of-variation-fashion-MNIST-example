sample_by_grid <- function(data2d, x_grid_size = 10, y_grid_size = 10) {
  # data2d: n x 2 matrix or data frame with columns [x, y]
  # x_grid_size: number of bins along the x-axis
  # y_grid_size: number of bins along the y-axis

  # 1) Extract x, y
  x <- data2d[, 1]
  y <- data2d[, 2]

  # 2) Create breakpoints for x and y
  x_breaks <- seq(min(x), max(x), length.out = x_grid_size + 1)
  y_breaks <- seq(min(y), max(y), length.out = y_grid_size + 1)

  # 3) Bin each point in x and y
  x_bin <- cut(x, breaks = x_breaks, include.lowest = TRUE, labels = FALSE)
  y_bin <- cut(y, breaks = y_breaks, include.lowest = TRUE, labels = FALSE)

  # 4) Create a factor that represents (x_bin, y_bin)
  #    E.g. "3.5" means x_bin=3, y_bin=5
  bin_group <- interaction(x_bin, y_bin, drop = TRUE)

  # 5) Split row indices by these bins
  grouped_indices <- split(seq_len(nrow(data2d)), bin_group)

  # 6) For each bin, pick exactly one point index (the first in that bin).
  #    If you want random, replace idx[1] with sample(idx, 1).
  chosen_per_bin <- lapply(grouped_indices, function(idx) idx[1])

  # 7) Initialize an m x n matrix of NAs
  #    We'll interpret i = x_bin (row dimension), j = y_bin (column dimension).
  M <- matrix(NA_integer_, nrow = x_grid_size, ncol = y_grid_size)

  # 8) Fill M[i,j] with the chosen index for bin i,j
  bin_names <- names(chosen_per_bin)  # something like "1.1", "1.2", ..., "m.n"
  for (bn in bin_names) {
    # Parse "i.j" to get integer i, j
    parts <- strsplit(bn, "\\.")[[1]]
    i <- as.integer(parts[1])
    j <- as.integer(parts[2])

    M[i, j] <- chosen_per_bin[[bn]]
  }

  return(M)
}

trace_back_index <- function(grid_idx, selected_index) {
  # grid_idx: a matrix of indices relative to the subset (e.g. output of sample_by_grid)
  # selected_index: a vector of indices that were used to subset the original dataset
  #
  # Returns: a matrix of original indices with the same dimensions as grid_idx.

  original_grid_idx <- matrix(selected_index[grid_idx],
                              nrow = nrow(grid_idx),
                              ncol = ncol(grid_idx))
  return(original_grid_idx)
}
