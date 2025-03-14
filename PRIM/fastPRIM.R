fast_peel_exact_t <- function(x, beta, t, tol = 1, max_iter = 100) {
  n <- nrow(x)
  p <- ncol(x)
  beta_T <- 1 - (1 - beta)^t
  target <- round(n * beta_T)
  
  lo <- 0
  hi <- 0.5
  lambda <- beta_T / 2
  
  iter <- 0
  while (iter < max_iter) {
    iter <- iter + 1
    lower <- sapply(1:p, function(i) quantile(x[, i], probs = lambda))
    upper <- sapply(1:p, function(i) quantile(x[, i], probs = 1 - lambda))
    
    inside <- rep(TRUE, n)
    for(i in 1:p) {
      inside <- inside & (x[, i] >= lower[i]) & (x[, i] <= upper[i])
    }
    count <- sum(inside)
    
    if (abs(count - target) <= tol) break
    
    if (count > target) {
      lo <- lambda
      lambda <- (lambda + hi) / 2
    } else {
      hi <- lambda
      lambda <- (lo + lambda) / 2
    }
  }
  
  return(list(box_bb = lower,
              box_pp = upper,
              count = count,
              lambda = lambda,
              iterations = iter))
}

fastPRIM <- function(x, beta = 0.10, t = 5, tol = 5, max_iter = 100) {
  n <- nrow(x)
  boxes <- list()
  count_data <- as.data.frame(x)
  
  for(i in 1:t) {
    box <- fast_peel_exact_t(x, beta, i, tol, max_iter)
    
    inside <- rep(TRUE, n)
    for(j in 1:ncol(x)) {
      inside <- inside & (x[, j] >= box$box_bb[j]) & (x[, j] <= box$box_pp[j])
    }
    points_in <- list(point = x[inside, , drop = FALSE],
                      index = which(inside))
    points_num <- nrow(points_in$point)
    
    boxes[[i]] <- list(box_bb = box$box_bb,
                       box_pp = box$box_pp,
                       points_num = points_num,
                       points_index = points_in$index,
                       points = points_in$point)
  }
  
  results <- list(box = boxes, size = n)
  return(results)
}