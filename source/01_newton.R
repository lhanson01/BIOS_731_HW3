compute_logit_gradient <- function(X, Y, pi){
  gradient <- numeric(length = p + 1)
  for(j in seq_len(p+1)){
    gradient[j] <- sum(X[,j]*(Y-pi))
  }
  # grad_comp <- matrix(NA, n, 2)
  # for(i in 1:n){
  #   pi <- exp(X[i,]%*%beta)/(1 + exp(X[i,]%*%beta))
  #   for(j in 1:(p+1)){
  #     grad_comp[i,j] <- X[i,j]*(Y[i]-pi)
  #   }
  # }
  # gradient <- colSums(grad_comp)
  return(gradient)
}

#compute_logit_gradient(X,Y, beta_init)


compute_logit_hessian <- function(X, Y, pi){
  hessian <- matrix(NA, nrow = p + 1, ncol = p + 1)
  for(j in seq_len(p+1)){
    for(k in seq_len(p+1)){
      hessian[j,k] <- -sum(X[,j]*X[,k]*pi)
    }
  }
  return(hessian)
}


newton <- function(X, Y, beta_init, tolerance, max_iter){
  
  beta <- beta_init
  beta_history <- gradient_history <- hessian_history <- 
    vector("list", length = max_iter)
  alpha = 0.01
  
  for(iter in seq_len(max_iter)){
    beta_history[[iter]] <- beta
    pi <- exp(X%*%beta)/(1+exp(X%*%beta))
    iter_gradient <- compute_logit_gradient(X, Y, pi)
    iter_hessian <- compute_logit_hessian(X, Y, pi)
    
    gradient_history[[iter]] <- iter_gradient
    hessian_history[[iter]] <- iter_hessian
    
    ##### Step towards MLE ####
    beta <- beta - solve(iter_hessian)%*%iter_gradient
    #beta <- beta - alpha*gradient
    
  }
  
  return(beta_history)
  
  
}


