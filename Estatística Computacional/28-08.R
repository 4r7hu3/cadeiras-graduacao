# b)
f_fib = function(n){
  fib = c()
  fib[1] = 1; fib[2] = 1
  for (i in 3:n){
    fib[i] = fib[i-1] + fib[i-2]
  }
  return(fib)
}


# a)
f_fib_menor = function(n) {
  fib0 = f_fib(n)
  fib1 = c()
  
  for(i in fib0){
    if(i < n){
      fib1 = c(fib1, i)
    }
    else {
      break
    }
  }
  return(fib1)
}
  
f_fib_menor(6)
f_fib_menor(7)
f_fib_menor(14)
f_fib_menor(22)
f_fib_menor(34)
f_fib_menor(35)


# 2)

f_fib_n = function(n){
  fib = c()
  fib[1] = 1; fib[2] = 1
  for (i in 3:n){
    fib[i] = fib[i-1] + fib[i-2]
  }
  return(fib[n])
}


# 3)

prime_n = function(n) {
  primes = c()
  for(i in 2:n) {
    cont = 0
    for(j in 1:n){
      if(i%%j == 0){
        cont = cont + 1
      }
    }
    if(cont <= 2) {
      primes = c(primes,i)
    }
  }
  return(primes)
}

prime_n(10)
prime_n(32)
