n <- 200
X_0 <- rep(1,n)
X_1 <- rnorm(n, mean = 0, sd = 11)
X <- cbind(X_0, X_1)
p <- ncol(X) - 1
beta_0_true <- 1# change to 1 later i guess, makes baseline probability really high
beta_1_true <- 0.3
beta_true <- rbind(beta_0_true, beta_1_true)
true_pi <- exp(X%*%beta_true)/(1+exp(X%*%beta_true))
Y <- rbinom(n, 1, true_pi)

iter <- 1
convergence_tol <- 0.0001
convergence <- 10
max_iter <- 100

minor <- objective <- beta_vec <- rep(NA, length = max_iter)

beta_init <- t(c(0,0))

logit_MM <- function(beta_0, convergence_tol, max_iter){
  
  beta <- beta_init
  
  beta_history <- rep(NA, max_iter)

  while(iter < max_iter & convergence > convergence_tol){
    
 
    minor[iter] <- sum(Y*X%*%beta)-sum((exp(X%*%beta)/(1 + exp(X%*%beta))))
    objective[iter] <- sum(Y*X%*%beta - log(1+exp(X%*%beta)))
    
    
  }
    
}
