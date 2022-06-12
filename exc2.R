sig_level <- function(n, sigma, iters, alfa= 0.01)
{
  null_hyp <- 0
  alt_hyp <- 0
  for (i in 1:iters)
  {
    sample <- rnorm(n, 0, sigma)
    ttest <- t.test(sample, alternative = "two.sided", mu=0, conf.level=1-alfa)
    if(ttest$p.value <= alfa) {
      alt_hyp <- alt_hyp + 1
    }
    else {
      null_hyp <- null_hyp + 1
    }
  }
  out <- paste0("poziom istotności dla n=",n," sigma=",sigma," liczby iteracji=",iters," wynosi: ", ... =   round(alt_hyp/(alt_hyp+null_hyp),2))
  return(list(out, round(alt_hyp/(alt_hyp+null_hyp),2)))
}


n_vec <- c(50, 100, 500, 1000)
sigma_vec <- c(0.5, 1, 2, 5, 10, 50)
iters_vec <- c(500, 1000, 5000, 10000, 20000)

all <- length(sigma_vec)*length(n_vec)

for (iter in iters_vec)
{
  correct_num <- 0
  print(paste0("############## ITERS = ", iter," #############################" ))
  for (sigma in sigma_vec)
  {
    for (n in n_vec)
    {
      #print(sig_level(n,sigma,iter)[[1]])
      if(sig_level(n,sigma,iter)[[2]] == 0.01) correct_num <- correct_num + 1
    }
  }
  print(paste0("correctly: ", correct_num/all*100,"%"))
}

beta_val <- function(n, mu, sigma, iters, alfa= 0.01)
{
  null_hyp <- 0
  alt_hyp <- 0
  for (i in 1:iters)
  {
    sample <- rnorm(n, mu, sigma)
    ttest <- t.test(sample, alternative = "two.sided", mu=0, conf.level=1-alfa)
    if(ttest$p.value <= alfa) {
      alt_hyp <- alt_hyp + 1
    }
    else {
      null_hyp <- null_hyp + 1
    }
  }
  beta <- null_hyp/(alt_hyp+null_hyp)
  out <- paste0("beta n=",n," sigma=",sigma," liczby iteracji=",iters," wynosi: ",round(beta,2))
  return(list(out, round(beta,2)))
}

library(tidyverse)

get_hetmap <- function(nsamples)
{
  sigma_vec <- seq(0.01, 1, length.out = 50)
  mu_vec <- seq(0.01, 0.5, length.out = 50)


  df <- tibble(n = numeric(), mu = numeric(), sigma = numeric(), beta = numeric(), power = numeric())
  for (sigma_it in sigma_vec)
  {
    for (mu_it in mu_vec)
    {
      beta_it = beta_val(n = nsamples, mu = mu_it, sigma = sigma_it, iters=1000)[[2]]
      df <- df %>% add_row(n = nsamples, mu = mu_it, sigma = sigma_it, beta = beta_it, power = 1-beta_it)
    }
  }
  
  return(ggplot(df, aes(mu, sigma, fill = power)) + geom_tile() + ggtitle(paste0("n=",nsamples))+theme(plot.title = element_text(hjust = 0.5)))
}

get_hetmap(50)

get_hetmap(100)

get_hetmap(500)
