fit_logistic_lasso <- function(x, y, lambda, beta0 = NULL, eps = 0.0001, iter_max = 100){
  
  ## fit_logistic_lasso(x, y, lambda, beta0, eps, iter_max) computes
  ## the logistic Lasso using coordinate descent on the penalized
  ## interatively reweighted least squares algorithm.
  ##
  ## Input:
  ## - x: matrix of predictors (not including the intercept)
  ## - y: vector of data
  ## - lambda: penalty
  ## - beta0: initial guess
  ## - eps: parameter for stopping critereon
  ## - iter_max: maximum number of iteration
  ##
  ## Output:
  ## - A list containing the members of intercept, beta, and lambda
  ##
  ## Example:
  ##   library(tidymodels)
  ##   library(tidyverse)
  ##   ret <- fit_logistic_lasso(x = x, y = y, lambda = 0.3)
  ##   ret
  
  n <-dim(x)[1]
  p <-dim(x)[2]
  
  # set beta0, beta, and intercept0
  if(is.null(beta0)){
    beta0 <- rep(0, p)
    beta <- beta0
    ic0 <- 0 # ic represents intercept0
  } else{
    beta <- beta0[-1] # access all but 1st element
    ic0 <- beta0[1] # access 1st element
  }
  
  # process the factor to be 0/1
  fct_levels <- levels(y)
  y <- as.numeric(y) - 1
  
  x_beta0 <- (x %*% beta) %>% as.numeric
  p_val <- 1/(1 + exp(-x_beta0))
  
  # initial gradient
  grad <- matrix(0, n, p)
  
  for(i in 1:iter_max){
    w <- p_val * (1 - p_val)
    z <- x_beta0 + (y - p_val)/w
    r_j <- z - x %*% beta
    
    ic <- sum(w*r_j)/sum(w) # ic represents intercept
    
    for (j in 1:p){
      r_j <- z - x[,-j] %*% beta[-j]
      
      if((t(x[,j]) %*% (r_j*w)) > 0){
        beta[j] <- max((abs(t(x[,j]) %*% (r_j*w)) - n*lambda),0)/(sum(w*(x[,j])^2))
        grad[j] <- (((t(x[,j]) %*% (r_j*w))/n) - (sum(w*(x[,j])^2*beta[j])/n)) + lambda
      } else{
        beta[j] <- -max((abs(t(x[,j]) %*% (r_j*w)) - n*lambda),0)/(sum(w*(x[,j])^2))
        grad[j] <- (((t(x[,j]) %*% (r_j*w))/n) - (sum(w*(x[,j])^2*beta[j])/n)) - lambda
      } 
    }
    
    x_beta0 <- (x %*% beta0) %>% as.numeric()
    p_val <- 1/(1 + exp(-x_beta0))
    names(beta) <- colnames(x)
    
    # Convergence check
    if (((max(max(abs(beta - beta0)), abs(ic - ic0))) < eps) == TRUE && 
        (sqrt(sum(grad^2)) < eps) == TRUE){
      return(list(converged = TRUE,
                  iter = i,
                  beta = beta, 
                  icntercept = ic,
                  lambda = lambda, 
                  fct_levels = fct_levels,
                  error = sqrt(sum(grad^2))))
    }
    beta0 <- beta
    ic0 <- ic
  }
  # We have exceeded iter_max    
  warning("Convergence took more than iter_max iter")
  return(list(converged = FALSE,
              iter = i,
              beta = beta, 
              intercept = ic,
              lambda = lambda, 
              fct_levels = fct_levels,
              error = sqrt(sum(grad^2))))
}

predict_logistic_lasso <- function(object, new_x){
  
  ## predict_logistic_lasso(object, new_x) predicts logistic lasso
  ## using the given data called new_x. 
  ##
  ## Input:
  ## - object: output from fit_logistic_lasso
  ## - new_x: data to predict at (may bew more than one point)
  ##
  ## Output:
  ## - A list containing the intercept and beta
  ##
  ## Example:
  ##   library(tidymodels)
  ##   library(tidyverse)
  ##   ret <- predict_logistic_lasso(object$fit, 
  ##            as.matrix(new_data[, names(object$fit$beta)]))
  ##   ret
  
  numeric_pred <- (object$intercept + (new_x %*% object$beta) >= 0) %>% as.numeric
  return(object$fct_levels[numeric_pred + 1] %>% factor)
}