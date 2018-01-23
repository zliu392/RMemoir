set.seed(123)
# generate data
x <- runif(100, 0, 2)
x <- sort(x)
y <- 0.1 * x^3 + x + sin(x) + 10
y <- y  + rnorm(100)
plot(x,y)
# set bandwidth
h <- 0.1
# kernel function
EpaK <- function(x_dat, x) {
  u <- (x_dat - x) / h
  u[which(abs(u) > 1)] <- 1
  K <- 3/4 * (1 - u^2)
  return(K)
}
K <- EpaK(x, 1)

# 1. Kernel estimator
(m_kernel <- sum(y*K) / sum(K))
# or using lm command
lm(y~1, weights = K)
# which is equivalent to
W <- diag(K)
lhs <- sqrt(W) %*% Y
rhs <- sqrt(W) %*% rep(1,100)
lm(lhs ~ 0 + rhs)

# 2. Local linear estimator
W <- diag(K)
X <- cbind(rep(1,100),x-1)
Y <- as.matrix(y)
(m_locallinear <- solve((t(X) %*% W %*% X)) %*% t(X) %*% W %*% Y)
# or using lm command
lm(Y ~ 0 + X, weights = K)
# which is equivalent to
lhs <- sqrt(W) %*% Y
rhs <- sqrt(W) %*% X
lm(lhs ~ 0 + rhs)