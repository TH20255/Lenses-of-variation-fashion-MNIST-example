find_spectral_gap <- function(pca_result) {
  eigenvalues <- pca_result$sdev^2
  
  diff_eigs <- diff(eigenvalues)
  
  largest_gap_index <- which.max(diff_eigs)
  
  return(list(
    num_components = largest_gap_index,
    eigenvalues    = eigenvalues
  ))
}