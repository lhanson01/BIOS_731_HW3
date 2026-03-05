library(dplyr)

set.seed(0725)
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

beta_0_init <- 0
beta_1_init <- 0
beta_init <- c(beta_0_init, beta_1_init)
max_iter <- 200
convergence_tol <- 0.0001
convergence <- 10


##### Newton #####
tictoc::tic()
newton_results <- newton(X, Y, beta_init, max_iter = max_iter, 
                         convergence_tol = convergence_tol)
t <- tictoc::toc()
t_newton<- t$toc - t$tic

newton_maxit <- newton_results$iterations[1]
newton_ll_history <- as.numeric(lapply(newton_results$ll_hist, function(x){
  as.numeric(x)
})
)
newton_beta <- newton_results$beta_hist[[newton_maxit]]
I_newton <- solve(-newton_results$hessian_hist[[newton_maxit]])
se_beta_newton <- sqrt(diag(I_newton))

# not correct, think its the t distribution
newton_confint <- list(
  beta_0_confint = c(newton_beta[1] - pt(0.975,n-p-1) * se_beta_newton[1],
                     newton_beta[1] + pt(0.975,n-p-1) * se_beta_newton[1]
                    ),
  beta_1_confint = c(newton_beta[2] - pt(0.975,n-p-1) * se_beta_newton[2],
                     newton_beta[2] + pt(0.975,n-p-1) * se_beta_newton[2]
  )
  )


##### GLM #####
tictoc::tic()
deviance <- capture.output(
  glm_results <- glm(Y ~ X[,2], 
                   family = "binomial",
                   start = beta_init, 
                   control = list(maxit = 100,
                                  epsilon = convergence_tol,
                                  trace = TRUE))
)

t <- tictoc::toc()
t_glm <- t$toc - t$tic

glm_ll_hist <- as.numeric(lapply(deviance, function(x){
  as.numeric(gsub("[^0-9.]", "", x))}
)) / -2

glm_maxit <- glm_results$iter
glm_beta <- glm_results$coefficients
glm_confint <- confint(glm_results)


##### Optim ######
obj_history <- c()

tictoc::tic()
optim_results <- optim(par = beta_init,
                      fn = log_lh,
                      method = "BFGS",
                      control = list(maxit = 100),
                      X = X,
                      y = y, 
                      hessian = TRUE)
t <- tictoc::toc()
t_optim <- t$toc - t$tic

optim_maxit <- optim_results$counts[1]
optim_beta <- optim_results$par
I_optim <- solve(-optim_results$hessian)
se_beta_optim <- sqrt(diag(I_newton))

### Also probably not correct, too narrow
optim_confint <- list(
  beta_0_confint = c(optim_beta[1] - pt(0.975,n-p-1) * se_beta_optim[1],
                     optim_beta[1] + pt(0.975,n-p-1) * se_beta_optim[1]
  ),
  beta_1_confint = c(optim_beta[2] - pt(0.975,n-p-1) * se_beta_optim[2],
                     optim_beta[2] + pt(0.975,n-p-1) * se_beta_optim[2]
  )
)

beta_0_all <- c(newton_beta[1], glm_beta[1], optim_beta[1])
beta_1_all <- c(newton_beta[2], glm_beta[2], optim_beta[2])
beta_0_ll_all <- c(newton_confint[[1]][1], glm_confint[1,1], optim_confint[[1]][1])
beta_0_ul_all <- c(newton_confint[[1]][2], glm_confint[1,2], optim_confint[[1]][2])
beta_1_ll_all <- c(newton_confint[[2]][1], glm_confint[2,1], optim_confint[[2]][1])
beta_1_ul_all <- c(newton_confint[[2]][2], glm_confint[2,2], optim_confint[[2]][2])
computation_time_all <- c(t_newton, t_glm, t_optim)
max_it_all <- c(newton_maxit, glm_maxit, optim_maxit)

results_frame <- data.frame("beta_0" = beta_0_all, 
                            "beta_1" = beta_1_all,
                            "beta_0_ll" =beta_0_ll_all,
                            "beta_0_ul" = beta_0_ul_all,
                            "beta_1_ll" = beta_1_ll_all,
                            "beta_1_ul" = beta_1_ul_all,
                            "Computation Time" = computation_time_all, 
                            "Max Iterations" = max_it_all) 

row.names(results_frame) <- c("Newton", "GLM", "Optim")






