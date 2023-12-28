# geração de números aleatórios

num_ale = function(dist='uniforme') {
  if(dist == 'normal'){
    return(rnorm(1000))
  }
  else if(dist == 'binomial') {
    return(rbinom(n=1000, size = 1000, p=0.5))
  }
  else if(dist == 'poisson') {
    return(rpois(1000))
  }
  else {
    return(runif(1000))
  }
}

data_norm = num_ale('normal')
data_unif = num_ale()
data_binom = num_ale('binomial')
data_pois = num_ale('pois')

hist(data_norm)
hist(data_binom)
hist(data_pois)
hist(data_unif)

# transformada inversa para uma Exp(1)
x = -log(1-data_unif); hist(x)





