set.seed(2137)
our_lambda <- 3/2
use_analitical_solution <- TRUE

log_like_fn <- function(lambda, X){
  X <- as.matrix(X)
  n <- length(X)
  fn <- n*log(lambda) - lambda*sum(X)
  
  return (-fn)
}

simulate_for_sample_size <- function(lambda, sample_size){
  random_sample <- rexp(sample_size, lambda)
  
  # 1. calculate maximum likelihood estimation
  if (use_analitical_solution){
    # https://www.statlect.com/fundamentals-of-statistics/exponential-distribution-maximum-likelihood
    est = length(random_sample)/sum(random_sample)
  }else{
    # https://www.youtube.com/watch?v=w3drLH-DFpE
    mle_est <- optim(par=3, fn=log_like_fn, lower=0, upper=Inf, method="L-BFGS-B", X = random_sample)
    est <- mle_est$par
  }
  
  # 2. calculate its bias
  bias <- est - lambda # abs?
  
  # 3. calculate cr bound
  numerator <- 1 + 1/(sample_size-1)
  fisher_inf <- 1/(lambda^2) #https://www-users.mat.umk.pl/~alzaig/inf_Fi.pdf
  # https://www-users.mat.umk.pl/~alzaig/nie_C_R.pdf
  cr_bound <- numerator / (sample_size * fisher_inf)
  
  # 4. calculate variance
  variance <- mean((random_sample - lambda)^2) #???????
  
  return (c(sample_size, est, bias, variance, cr_bound))
}


################################################################################

column_names <- c('sample_size', 'estimated_lambda', 'bias', 'variance', 'cr_bound')
results <- data.frame(matrix(ncol = length(column_names), nrow = 0))
colnames(results) <- column_names

for (i in 1:100){
  sample_size <- 1000 * i
  result <- simulate_for_sample_size(our_lambda, sample_size)
  
  result <- as.data.frame(t(result))
  colnames(result) <- column_names
  results <- rbind(results, result)
}
print(results)


# plot results somehow
plot(results$bias ~ results$sample_size, 
     main = 'Obciążenie od liczności próby', xlab = 'liczność próby', ylab = 'obciążenie')

plot(results$variance ~ results$sample_size, 
     main = 'Wariancja od liczności próby', xlab = 'liczność próby', ylab = 'wariancja')

plot(results$cr_bound ~ results$sample_size, 
     main = 'Kres CR od liczności próby', xlab = 'liczność próby', ylab = 'CR')
