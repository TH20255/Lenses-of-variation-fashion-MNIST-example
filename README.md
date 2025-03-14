# Lenses of Variation: Fashion MNIST Example
This repository contains an integrated R pipeline for analyzing the Fashion MNIST dataset using a PCA–fastPRIM framework. The goal is to extract desired subsets from the data by applying fastPRIM peeling in both high-variance (principal) and low-variance (pettiest) subspaces. These subsets are then visualized using t-SNE and grid-based sample selection, providing insights into fashion trends and design variety.

## Directory Structure

- **data/**  
  Contains the input CSV file `fashion-mnist_test.csv` with the Fashion MNIST test dataset.

- **PRIM/**  
  Contains R scripts for the PRIM-based peeling functions:
  - `spectral_gap.R`: Computes the spectral gap from the PCA result.
  - `fastPRIM.R`: Implements the fastPRIM peeling algorithm.

- **fashionMNIST/**  
  Contains helper scripts:
  - `sample_by_grid.R`: Selects data points by grid from a t-SNE embedding.
  - `print_image.R`: Reconstructs and saves image matrices for selected indices.

- **outputs/**  
  Output directory for grid plots and reconstructed images.

## Dependencies

Ensure that you have the following R packages installed:
- **tidyverse**
- **keras**
- **Rtsne**

Install them via:
```r
install.packages(c("tidyverse", "Rtsne"))
# For keras, follow instructions at: https://keras.rstudio.com/

## Contact
For any questions or issues, please refer to the source files in the repository or contact the project maintainers by txl1001@miami.edu.
