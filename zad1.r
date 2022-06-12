require(magrittr)
require(tidyverse)
require(zoo)
require(tibble)
library(dplyr)
library (reshape2)

set.seed(2137)
lambda <- as.integer(3/2)
# z tw. o kresie Cramera Rao:
# prawdziwa_wariancja >= c1/n/I(lambda) gdzie:
# c1 = (m`(lambda))^2 / I(lambda)
# Wariancja powinna zibegać do Estymator = c1/n/I(lambda) => zbadamy iloraz Wariancja/Estymator
# Z wikipedii I(lambda) = 1/lambda^2
# Z wikipedii obciążenie =  lambda / (n - 1)
#
# Zatem:
#
# wartość oczekiwana: ex(lambda) = lambda + lambda / (n - 1)
# wartość  pochodnej: ex`(lambda)= 1 + 1 / (n-1)              #po lambdzie

informacjaFishera <- function(lambda) {1/(lambda**2)}
wartoscOczekiwana <- function(lambda, n) {lambda + lambda / (n-1)}
wartoscOczekiwanaPrim <- function(n) {1 + 1 / (n-1)}
constValue <- function(lambda, n) { wartoscOczekiwanaPrim(n)**2 / informacjaFishera(lambda) }

rv <- c()
for (exponent in 1:100){
  nn = 1000
  n = 10*exponent
  
  estymatory = replicate(nn, {
      dane = rexp(n, lambda)
      1 / mean(dane)
  })
  
  obciążenie = mean(estymatory - lambda)
  wariancja = var(estymatory)
  estymator = constValue(lambda,n)/n
  
  #print(wariancja/estymator)
  rv <- append(rv, wariancja/estymator)
}

t=1:100
plot(t,rv, type="l", col="green", lwd=5, xlab="time", ylab="concentration", main="estimator fitnes")
