library(tidyverse)

data0 <- read.csv("./data/fashion-mnist_test.csv", header=TRUE,
                  stringsAsFactors = FALSE)

# input the classification results from classification algorithm like k-means
data0$label <- classification_results

for(cloth_type in 0:9){
  path <- "./outputs/"
  data <- data0 %>% filter(label==cloth_type)
  data <- data[,-1]
  data <- data[!duplicated(data), ]
  
  ###### PCA ######
  pca <- prcomp(data)
  names(pca)[2] <- "loadings"
  x_pca <- pca$x
  reg_x <- as.data.frame(x_pca)
  p <- ncol(reg_x)
  
  source(".\\PRIM\\spectral_gap.R")
  spectral_gap <- find_spectral_gap(pca)
  p_num <- 10
  
  principal_index <- 1:p_num
  pettiest_index <- (spectral_gap$num_components-50+1):spectral_gap$num_components
  
  high_var <- reg_x[principal_index] %>% data.matrix()
  low_var <- reg_x[pettiest_index] %>% data.matrix()
  
  ###### fastPRIM peeling ######
  source(".\\PRIM\\fastPRIM.R")
  m.fastprim.h <- fastPRIM(x=high_var,
                           beta = 0.1,
                           t = 10)
  
  m.fastprim.l <- fastPRIM(x=low_var,
                           beta = 0.1,
                           t = 10)
  
  ###### visualization by t-SNE ######
  selected_index_h <- m.fastprim.h$box[[1]][["points_index"]]
  selected_index_l <- m.fastprim.l$box[[1]][["points_index"]]
  
  library(keras)
  library(Rtsne)
  set.seed(123)
  tsne_result <- Rtsne(data, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 1000)
  tsne_df <- data.frame(TSNE1 = tsne_result$Y[, 1], TSNE2 = tsne_result$Y[, 2])
  
  # select data points by grid
  source(".\\fashionMNIST\\sample_by_grid.R")
  selected_grid_matrix_h <- sample_by_grid(tsne_df[selected_index_h,],10,10)
  selected_grid_matrix_h <- trace_back_index(selected_grid_matrix_h,selected_index_h)
  
  selected_grid_matrix_l <- sample_by_grid(tsne_df[selected_index_l,],10,10)
  selected_grid_matrix_l <- trace_back_index(selected_grid_matrix_l,selected_index_l)
  
  grid_plot_h <- ggplot(tsne_df, aes(x = TSNE1, y = TSNE2)) +
    geom_point(alpha=0.3) +
    geom_point(data=tsne_df[selected_index_h,],
               aes(x = TSNE1, y = TSNE2), color="#F8766D", size = 2) +
    geom_point(data = tsne_df[na.omit(as.vector(t(selected_grid_matrix_h))), ],
               aes(x = TSNE1, y = TSNE2), color = "red", shape = 1, size = 4) +
    labs(x = "t-SNE Dimension 1",
         y = "t-SNE Dimension 2") +
    theme_bw()
  
  grid_plot_l <- ggplot(tsne_df, aes(x = TSNE1, y = TSNE2)) +
    geom_point(alpha=0.3) +
    geom_point(data=tsne_df[selected_index_l,],
               aes(x = TSNE1, y = TSNE2), color="#00BFC4", size = 2) +
    geom_point(data = tsne_df[na.omit(as.vector(t(selected_grid_matrix_l))), ],
               aes(x = TSNE1, y = TSNE2), color = "red", shape = 1, size = 4) +
    labs(x = "t-SNE Dimension 1",
         y = "t-SNE Dimension 2") +
    theme_bw()
  
  ggsave(filename = paste0(path,"grid_",cloth_type,"_h",".png"),
         plot = grid_plot_h,
         width = 5.5, height = 4.5, dpi = 100)
  ggsave(filename = paste0(path,"grid_",cloth_type,"_l",".png"),
         plot = grid_plot_l,
         width = 5.5, height = 4.5, dpi = 100)
  
  source(".\\fashionMNIST\\print_image.R")
  print_image_matrix(as.matrix(data), selected_grid_matrix_h, name=paste0(path,"fashion_",cloth_type,"_h"))
  print_image_matrix(as.matrix(data), selected_grid_matrix_l, name=paste0(path,"fashion_",cloth_type,"_l"))
}