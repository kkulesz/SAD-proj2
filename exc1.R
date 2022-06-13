#### set params of experiment
set.seed(2137)
our_lambda <- 3/2
start <- 10
step <- 10
no_of_steps <- 100
no_of_simulations_in_single_step <- 1000

#### help functions
simulate_for_sample_size <- function(lambda, sample_size){
  
  # 1. calculate maximum likelihood estimations
  estimators = c()
  for (i in 1:no_of_simulations_in_single_step){
    random_sample <- rexp(sample_size, lambda)
    est = length(random_sample)/sum(random_sample)
    estimators <- c(estimators, est)
  }
  final_est <- mean(estimators)
  
  # 2. calculate its bias
  bias <- final_est - lambda
  
  # 3. calculate cr bound
  fisher_inf <- 1/(lambda^2) 
  m_prim <- 1
  cr_bound <- m_prim / (sample_size * fisher_inf)
  
  # 4. calculate variance
  variance <- mean((estimators - lambda)^2)
  
  # 5. plot normalized for chosen sample sizes
  if(sample_size == 10 || sample_size == 100 || sample_size == 1000 || sample_size == 10000){
    normalized <- sqrt(sample_size) * (estimators - lambda)
    hist(normalized, prob=TRUE, xlab="Value", main=paste('Normalized for sample size = ', as.character(sample_size)), xlim=c(-6,6), ylim=c(0, 0.5), breaks = 100)
    norm_sd = sqrt(sample_size) * sqrt(variance)
    curve(dnorm(x, mean=0, sd=norm_sd), add=TRUE, col = 'red', lwd = 2)
  }
  
  return (c(sample_size, est, bias, variance, cr_bound))
}


#### do experiment
column_names <- c('sample_size', 'estimated_lambda', 'bias', 'variance', 'cr_bound')
results <- data.frame(matrix(ncol = length(column_names), nrow = 0))
colnames(results) <- column_names

for (i in 1:no_of_steps){
  sample_size <- start + step * (i-1)
  result <- simulate_for_sample_size(our_lambda, sample_size)
  
  result <- as.data.frame(t(result))
  colnames(result) <- column_names
  results <- rbind(results, result)
}


# plot bias
plot(results$bias ~ results$sample_size, 
     main = 'Obciążenie od liczności próby', xlab = 'liczność próby', ylab = 'obciążenie')
abline(h = 0, col = 'red', lty=2)

# plot cr-bound and variance
plot(results$cr_bound ~ results$sample_size, 
     main = 'siema', xlab = 'liczność próby', ylab = 'wartość', type = 'l', col = 'red', lwd = 2, ylim=c(0, 0.5), log = 'x')
lines(results$sample_size, results$variance, col='blue', lwd = 1)
legend(x = 'topright', c('variance', 'cr-bound'), col = c('blue', 'red'), lty=c(1, 1), lwd = 2)
