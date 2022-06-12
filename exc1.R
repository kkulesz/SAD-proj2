# set params of experiment
set.seed(2137)
our_lambda <- 3/2
use_analitical_solution <- TRUE
start <- 10
step <- 1
no_of_steps <- 1000
no_of_simulations_in_single_step <- 100


# do experiment
log_like_fn <- function(lambda, X){
  X <- as.matrix(X)
  n <- length(X)
  fn <- n*log(lambda) - lambda*sum(X)
  
  return (-fn)
}

simulate_for_sample_size <- function(lambda, sample_size){
  
  estimators = c()
  for (i in 1:no_of_simulations_in_single_step){
    random_sample <- rexp(sample_size, lambda)
    
    # 1. calculate maximum likelihood estimations
    if (use_analitical_solution){
      # https://www.statlect.com/fundamentals-of-statistics/exponential-distribution-maximum-likelihood
      est = length(random_sample)/sum(random_sample)
    }else{
      # https://www.youtube.com/watch?v=w3drLH-DFpE
      mle_est <- optim(par=3, fn=log_like_fn, lower=0, upper=Inf, method="L-BFGS-B", X = random_sample)
      est <- mle_est$par
    }
    
    estimators <- c(estimators, est)
  }
  final_est <- mean(estimators)
  
  # 2. calculate its bias
  bias <- final_est - lambda # abs?
  
  # 3. calculate cr bound
  fisher_inf <- 1/(lambda^2) 
  m_prim <- 1
  cr_bound <- m_prim / (sample_size * fisher_inf)
  
  # 4. calculate variance
  variance <- mean((estimators - lambda)^2)
  
  # TODO

  if(sample_size %% 300 == 0){
    print('jestem ')
    normalized <- sqrt(sample_size) * (estimators - lambda)
    hist(normalized, prob=TRUE, xlab="max. temp. diff.", main=as.character(sample_size), xlim=c(-10,10), breaks = 20)
  }
  
  return (c(sample_size, est, bias, variance, cr_bound))
}


################################################################################

column_names <- c('sample_size', 'estimated_lambda', 'bias', 'variance', 'cr_bound')
results <- data.frame(matrix(ncol = length(column_names), nrow = 0))
colnames(results) <- column_names

for (i in 1:no_of_steps){
  sample_size <- start + step * (i-1)
  print(sample_size)
  result <- simulate_for_sample_size(our_lambda, sample_size)
  
  result <- as.data.frame(t(result))
  colnames(result) <- column_names
  results <- rbind(results, result)
}
print(results)


# plot bias
plot(results$bias ~ results$sample_size, 
     main = 'Obciążenie od liczności próby', xlab = 'liczność próby', ylab = 'obciążenie')
abline(h = 0, col = 'red', lty=2)

# plot cr-bound and variance
plot(results$cr_bound ~ results$sample_size, 
     main = 'siema', xlab = 'liczność próby', ylab = 'wartość', type = 'l', col = 'red', lwd = 2, ylim=c(0,1), log = 'x')
lines(results$sample_size, results$variance, col='blue', lwd = 1)
legend(x = 'topright', c('variance', 'cr-bound'), col = c('blue', 'red'), lty=c(1, 1), lwd = 2)

# links to not forget them
# https://www-users.mat.umk.pl/~alzaig/inf_Fi.pdf
# https://www-users.mat.umk.pl/~alzaig/nie_C_R.pdf