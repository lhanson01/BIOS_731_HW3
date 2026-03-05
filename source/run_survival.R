time <- censor_time <- veteran$time
censoring_status <- veteran$status
n <- length(time)
max_iter <- 200
convergence_tol <- 0.00001
convergence <- 10
lambda_init <- mean(time)

lambda_result <- em_survival(
  lambda_0 = lambda_init, 
  t = time, 
  c = censor_time,
  d = censoring_status,
  convergence_tol = convergence_tol,
  max_iter = max_iter
)

lambda_hist <- lambda_result$lambda_history

lambda_hat <- lambda_result$lambda

set.seed(0725)
nboot <- 10000
samples <- replicate(nboot, sample(seq_len(n), size = n, replace = TRUE))
lambda_bs <- numeric(length = nboot)

for(i in seq_len(nboot)){
  t <- c_t <- veteran$time[samples[,i]]
  cens_status <- veteran$status[samples[,i]]
  lambda_0 <- mean(t)
  lambda_bs[i] <- em_survival(
    lambda_0 = lambda_init, 
    t = t, 
    c = c_t,
    d = cens_status,
    convergence_tol = convergence_tol,
    max_iter = max_iter
  )$lambda
}

perc_boot_CI <- quantile(lambda_bs, c(0.025, 0.975))

AFT_model <- survreg(Surv(time, censoring_status) ~ 1, dist = "weibull", scale = 1)

AFT_est <- as.numeric(exp(AFT_model$coefficients))

AFT_CI <- exp(confint(AFT_model))