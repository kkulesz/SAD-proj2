set.seed(2137)
lambda <- as.integer(3/2)

simulate_for_sample_size <- function(sample_size){
  random_sample <- rexp(sample_size, lambda)
  
  # count maximum likelihood estimation
  # count its variance
  # count its bias
  # count Cramer-Rao bound? or it should be identitcal everywhere?
  # compare and return everything so it can be plotted later?
  return (c('', '', '', '', ''))
}

column_names <- c('sample_size', 'estimated_lambda', 'bias', 'variance', 'CR-bound')
results <- data.frame(matrix(ncol = length(column_names), nrow = 0))
colnames(results) <- column_names

base <- 2
for (exponent in 1:16){
  sample_size <- base ^ exponent # exponentail or linear scale?
  result <- simulate_for_sample_size(sample_size)
  
  result <- as.data.frame(t(result))
  colnames(result) <- column_names
  results <- rbind(results, result)
}

# plot results somehow


