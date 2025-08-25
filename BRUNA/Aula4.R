amostra <- rnorm(30)
qqnorm(amostra)
qqline(amostra)


amostra2 <- runif(1000)
acf(amostra2)

U <- runif(12)
U.star <- U-0.5
Fn <- ecdf(U.star)
plot(Fn)
abline(0.5,1)
ene <- 1e3
matriz <-matrix(runif(ene*12)-0.5, ncol = 12)
z.star <- apply(matriz, 1, sum)
f.star <- ecdf(z.star)
plot(f.star)
curve(pnorm,from = 4,to=4, add = T)
ks.test(z.star, "pnorm")
qnorm(p=(1:10/10))
quantis <- qnorm(p=(1:10/10))
cut.star <- cut(x=z.star, breaks = quantis)
conta.star <- table(cut.star)
conta.star 
1- pchisq(2.58, df=10)

pchisq(0.5, df=2)

# Gerar matriz com valores U(−0.5, 0.5)
matriz <- matrix(runif(n * p) - 0.5, ncol = p)

# Separar colunas ímpares e pares
X_impar <- matriz[, seq(1, p, 2)]  # colunas 1,3,5,7,9,11
X_par   <- matriz[, seq(2, p, 2)]  # colunas 2,4,6,8,10,12

# Visualizar as primeiras linhas
head(X_impar)
head(X_par)

p <- 12
matriz <- matrix(runif(n * p) - 0.5, ncol = p)
x <- matriz
# Distâncias x'x de cada vetor
distancias1  <- apply(matriz, 1, function(x) sum(x^2))
distancias <- apply(x**2, MARGIN =1, FUN =  sum)



d_X  <- rowSums(X_impar^2)
d_Xp <- rowSums(X_par^2)
boxplot(d_X, d_Xp, names = c("X (ímpar)", "X' (par)"), main = "Distâncias x'x por grupo")


U1<- runif(10)
U2 <- runif(10)

z1=sqrt(-2*log(U1))*cos(2*pi*U2)
U1[U1 == 0] <- .Machine$double.eps
U1[U1 == 1] <- 1 - .Machine$double.eps
z2=sqrt(-2*log(U1))*sin(2*pi*U2)
hist(c(z1, z2), breaks = 10, main = "Histogram of Z1 and Z2", col = "lightblue")
qqnorm(c(z1, z2)); qqline(c(z1, z2))





repeat {
  U1 <- runif(10)
  U2 <- runif(10)
  if (all(U1 > 0 & U1 < 1 & U2 > 0 & U2 < 1)) break
}



# Gerar 1000 somas de quadrados de 5 normais padrão (convolução)
set.seed(123)
X <- replicate(1000, sum(rnorm(5)^2))

# Visualizar histograma e curva teórica
hist(X, freq = FALSE, breaks = 40, col = "lightblue", main = "χ²(5)")
curve(dchisq(x, df = 5), add = TRUE, col = "red", lwd = 2)

# Comparar quantis
quantile(X, c(0.5, 0.95, 0.99))
qchisq(c(0.5, 0.95, 0.99), df = 5)


div <-X_par/X_impar
hist(div)
plot(div)

# Gerar 10 valores com distribuição de Cauchy padrão usando inversa
set.seed(123)
U <- runif(10)
X <- tan(pi * (U - 0.5))
X

hist(X)
plot(X)


