sigLvl <- function(n, sigma, iters, alfa= 0.01)
{
  nullHypothesis <- 0
  altHypothesis <- 0
  for (i in 1:iters)
  {
    sampleData <- rnorm(n, 0, sigma)
    ttest <- t.test(sampleData, alternative = "two.sided", mu=0, conf.level=1-alfa)
    if(ttest$p.value <= alfa) {
      altHypothesis <- altHypothesis + 1
    }
    else {
      nullHypothesis <- nullHypothesis + 1
    }
  }
  out <- paste0("istotność dla n=",n," sigma=",sigma," iteracje=",iters," wynosi: ", ... =   round(altHypothesis/(altHypothesis+nullHypothesis),2))
  return(list(out, round(altHypothesis/(altHypothesis+nullHypothesis),2)))
}

n_vec <- c(50, 100, 500, 2000)
sigma_vec <- c(0.5, 1, 2, 5)
iters_vec <- c(50, 100, 500, 2000)

all <- length(sigma_vec)*length(n_vec)

for (iter in iters_vec)
{
  correct_num <- 0
  print(paste0("### ITERS = ", iter," " ))
  for (sigma in sigma_vec)
  {
    for (n in n_vec)
    {
      if(sigLvl(n,sigma,iter)[[2]] == 0.01) correct_num <- correct_num + 1
    }
  }
  print(paste0("correctly: ", correct_num/all*100,"%"))
}

betaValue <- function(n, mu, sigma, iters, alfa= 0.01)
{
  nullHypothesis <- 0
  altHypothesis <- 0
  for (i in 1:iters)
  {
    sampleData <- rnorm(n, mu, sigma)
    ttest <- t.test(sampleData, alternative = "two.sided", mu=0, conf.level=1-alfa)
    if(ttest$p.value <= alfa) {
      altHypothesis <- altHypothesis + 1
    }
    else {
      nullHypothesis <- nullHypothesis + 1
    }
  }
  beta <- nullHypothesis/(altHypothesis+nullHypothesis)
  out <- paste0("beta n=",n," sigma=",sigma," liczby iteracji=",iters," wynosi: ",round(beta,2))
  return(list(out, round(beta,2)))
}

library(tidyverse)

generateHeatMap <- function(nsamples)
{
  sigma_vec <- seq(0.01, 1, length.out = 50)
  mu_vec <- seq(0.01, 0.5, length.out = 50)


  df <- tibble(n = numeric(), mu = numeric(), sigma = numeric(), beta = numeric(), power = numeric())
  for (sigma_it in sigma_vec)
  {
    for (mu_it in mu_vec)
    {
      beta_it = betaValue(n = nsamples, mu = mu_it, sigma = sigma_it, iters=1000)[[2]]
      df <- df %>% add_row(n = nsamples, mu = mu_it, sigma = sigma_it, beta = beta_it, power = 1-beta_it)
    }
  }
  
  return(ggplot(df, aes(mu, sigma, fill = power)) + geom_tile() + ggtitle(paste0("n=",nsamples))+theme(plot.title = element_text(hjust = 0.5)))
}

generateHeatMap(50)

generateHeatMap(100)

generateHeatMap(500)
