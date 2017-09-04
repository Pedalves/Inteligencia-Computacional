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

odd.50 <- seq(from=1, to=50, 2)

print(odd.50)

### B)

sum.odd.50 <- sum(odd.50)

print(sum.odd.50)

### C)

mult.3 <- odd.50[odd.50 %% 3 == 0]

print(mult.3)

### D)

div.3 <- mult.3/3

print(div.3)


# 5)
### A)

sq1 <- seq(from=0, to=100, length=20)

print(sq1)
print(length(sq1))

### B)

sq2 <- sq1[sq1 != sq1[5] & sq1 != sq1[10]]

print(seq2)

### C)

sq3 <- sq1[seq(1, 20, 2)]

print(sq3)

### D)

sq4 <- sq1
sq4[seq(2,20,2)] <- seq(2,20,2)

print(sq4)


# 6)
### A)

eletricidade <- data.frame(
  c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"),
  c(8839, 9159, 9476, 9736, 10249, 10664, 11057, 11569, 11969, 12310, 12672, 13002))
colnames(eletricidade) <- c("Meses","Valores")
print(eletricidade)

### B)

eletricidade.consumo <- diff(eletricidade$Valores)
print(eletricidade.consumo)

### C)

eletricidade.range <- c(max(eletricidade.consumo), min(eletricidade.consumo))
print(eletricidade.range)

### D)

eletricidade.media <- mean(eletricidade.consumo)
print(eletricidade.media)

eletricidade.mediana <- median(eletricidade.consumo)
print(eletricidade.mediana)

eletricidade.variancia <- var(eletricidade.consumo)
print(eletricidade.variancia)

eletricidade.desviopadrao <- sd(eletricidade.consumo)
print(eletricidade.desviopadrao)


# 7)

desvio.padrao <- function(amostras){
  sqrt((1/(length(amostras) -1) * sum((amostras - mean(amostras))^2)))
}

desvio.padrao(eletricidade.consumo)


# 8)
### A)

matriz.normal <- matrix(rnorm(3*5, mean = 10, sd = 3.6),nrow = 3, ncol = 5)
print(matriz.normal)

### B)

matriz.normal[2,]
matriz.normal[,3]

### C)

dim(matriz.normal)

### D)

sum(matriz.normal)

### E)

matriz.normal %*% t(matriz.normal)

### F)

sum(matriz.normal[1,])

### G)

dataframe.linha <- data.frame(rowMeans(matriz.normal), apply(matriz.normal, 1, var))
colnames(dataframe.linha) <- c("Media","Variancia")

dataframe.coluna <- data.frame(colMeans(matriz.normal), apply(matriz.normal, 2, var))
colnames(dataframe.linha) <- c("Media","Variancia")


# 9)
### A)

array.tri <- array(seq(0, 59), c(4,5,3))
print(array.tri)

### B)

sum(array.tri[,4,])

### C)

mean(array.tri[1,1,])

### D)

print((array.tri*2) + 5)


# 10)
### A)

dfPopulacaoEconomicamenteAtiva <- read.csv("populacaoeconomicamenteativa.csv", sep = ";")

dim(dfPopulacaoEconomicamenteAtiva)
colnames(dfPopulacaoEconomicamenteAtiva)

### B)

df.sudeste <- dfPopulacaoEconomicamenteAtiva[dfPopulacaoEconomicamenteAtiva$Ano > 2003 & dfPopulacaoEconomicamenteAtiva$Estado %in% c("RJ", "SP", "MG", "ES"), ]

### C)

summario <- aggregate(df.sudeste$Populacao, 
                      list(Ano=df.sudeste$Ano), 
                      sum)
print(summario)

### D)

barplot(summario$x, xlab="Ano", ylab="População Economicamente Ativa", col=c("blue"))


# 11)
### A)

dfLaranja <- read.csv2("laranja.csv")

### B)

define.categoria <- function(df){
  df$Categoria <- ifelse(df$IndiceAcidez<=0.5, "A",
                         ifelse(df$IndiceAcidez>0.5 & df$IndiceAcidez<=0.7, "B",
                                ifelse(df$IndiceAcidez>0.7 & df$IndiceAcidez<=0.9, "C",
                                       ifelse(df$IndiceAcidez>0.9 & df$IndiceAcidez<=1.1, "D",
                                              ifelse(df$IndiceAcidez>1.1 & df$IndiceAcidez<=1.3, "E",
                                                     ifelse(df$IndiceAcidez>1.3 & df$IndiceAcidez<=1.5, "F",
                                                            ifelse(df$IndiceAcidez>1.5 & df$IndiceAcidez<=1.7, "G",
                                                                   ifelse(df$IndiceAcidez>1.7, "H", NA))))))))
  df
}

dfLaranja <- define.categoria(dfLaranja)

### C)

calcula.volume <- function(df) {
  (4*pi*((df$Diametro/2)^3))/3
}

dfLaranja$Volume <- calcula.volume(dfLaranja)

### D)

classifica.destino <- function(categoria, peso, volume) {
  ifelse(categoria == "A", "descarte",
         ifelse(peso/volume > 0.7, "suco", "venda"))
}

dfLaranja$Destino <- classifica.destino(dfLaranja$Categoria, dfLaranja$Peso, dfLaranja$Volume)

### E)

x <- aggregate(dfLaranja$Destino, 
               list(Destino=dfLaranja$Destino), 
               length)

pie(x$x, x$Destino)

### F)

hist(dfLaranja$IndiceAcidez, main="Histograma do Índice de Acidez", xlab="Índice de Acidez", col="green")

### G)

hist(dfLaranja$IndiceAcidez, main="Histograma do Índice de Acidez", xlab="Índice de Acidez", col="green", probability = TRUE, freq = FALSE)

xfit <- seq(min(dfLaranja$IndiceAcidez), max(dfLaranja$IndiceAcidez), length = 40) 
yfit <- dnorm(xfit, mean = mean(dfLaranja$IndiceAcidez), sd = sd(dfLaranja$IndiceAcidez)) 

lines(xfit, yfit)
