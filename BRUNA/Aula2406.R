
#Geração de uniformes discretas
u <- runif(100)
n <-10
ene <-trunc((n+1)*u)+1
table(ene)

diff(pnorm(q=c(1,2)))
x <- u+1

g_x <- function(x) {
  (1 / sqrt(2 * pi)) * exp(-x^2 / 2)
}

# Intervalo de integração
a <- 1
b <- 2

# Número de amostras
n <- 100000

# Gera uniformes em (0,1)
U <- runif(n)

# Transforma para x no intervalo (1,2)
X <- U * (b - a) + a


h_y<- function()

  a<- 1;  b <- 2
ge <- function(x)  (1 / sqrt(2 * pi)) * exp(-x^2 / 2)

aga <- function(y) ge(y*(b-a) + a)*(b - a)
mean(aga(U))

integral2 <-function(n) {
U <- runif(n)
mean(aga(U))
}
qtes <- c(1e2, 5e2, 1e3, 5e3, 1e4, 5e4, 1e5, 1e6)
valores <- function(z) {
  sapply(qtes, integral2)
}
  cem <-replicate(100, valores)
dim(cem)

plot(x=log(qtes, base = 10), y=1:8, ylim = c(0.1, 0.15), type="m")
for( i in 1:8){
  points(x=rep(log(qtes[i], base=10)100), y=cem[i])
}













