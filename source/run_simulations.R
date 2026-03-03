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

newton(X, Y, beta_init, max_iter = 100)

glm(Y ~ X, family = "binomial")