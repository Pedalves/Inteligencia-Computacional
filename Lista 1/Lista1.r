# 1)
menor.detres <- function(val1, val2, val3){
  min(val1, val2, val3)
}

print(menor.detres(1,2,3))
print(menor.detres(1,0,3))
print(menor.detres(7,6,3))


# 2) 
calculadora <- function(val1, val2, oper){
  switch (oper,
    '+' = {result <- val1+val2},
    '-' = {result <- val1-val2},
    '*' = {result <- val1*val2},
    '/' = {result <- val1/val2},
    {
      print('Operado invalido!')
      result <- NULL
    }
  )
  
  return(result)
}

print(calculadora(1, 2, 'd'))
print(calculadora(1, 2, '-'))
print(calculadora(1, 2, '+'))
print(calculadora(3, 2, '*'))
print(calculadora(4, 2, '/'))


# 3)
fibonacci <- function(n){
  f <- c(0, 1)
  
  for(i in 3:n)
  {
    f <- c(f, f[i-1] + f[i-2])
  }
  
  return(f[1:n])
}

print(fibonacci(9))


# 4)
### A)

odd