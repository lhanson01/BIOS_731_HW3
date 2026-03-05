em_survival <- function(lambda_0, t, c, d, convergence_tol, max_iter){
  
  lambda <- lambda_0
  
  n <- length(t)
  
  lambda_history <- numeric(length = max_iter)
  
  lambda_history[1] <- lambda_0
  
  iter = 2
  
  
  
  while(iter < max_iter & convergence > convergence_tol){
    
   lambda <- sum(d*t + (1-d)*(c+lambda))/n
   
   lambda_history[iter] <- lambda
   
  # Using inner product (lambda_{t+1} - lambda{t})^T*(lambda_{t+1} - lambda{t}) 
  # for now
   if(iter > 1){
     convergence <- (lambda_history[iter]-lambda_history[iter-1])^2
   }
   
   # Q function to plot
   # Observed ll to plot?
   
  iter = iter + 1
      
  }
  
  return(list(lambda_history = lambda_history,
              lambda = lambda))
  
}


