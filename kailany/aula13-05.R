##AULA 13/05

#Análise de normalidade
amostra <- rnorm(30)
qqnorm(amostra)
qqline(amostra)

# Gera uma amostra de 1000 observações de uma distribuição uniforme no intervalo [0,1]
amostra2 <- runif(1000)

# Plota a função de autocorrelação (ACF) da amostra uniforme para avaliar dependência entre os valores
acf(amostra2)

# Gera uma amostra de 12 observações da distribuição uniforme no intervalo [0,1]
U <- runif(12)

# Centraliza os dados em torno de zero, transformando o intervalo de [0,1] para [-0.5, 0.5]
Ustar <- U - 0.5

# Cria a função de distribuição empírica (ECDF) dos dados centralizados
Fn <- ecdf(Ustar)

# Plota a ECDF dos valores centralizados
plot(Fn)

# Adiciona ao gráfico uma reta com inclinação 1 e intercepto 0.5, correspondente à CDF da uniforme [-0.5, 0.5]
abline(0.5, 1)

ene <- 1e3
matriz <- matrix(runif(ene*12)- 0.5, ncol=12)
Zstar <- apply(matriz, 1, sum) 
Zstar

Fstar <- ecdf(Zstar)
plot(Fstar)
curve(pnorm, from=-4, to=4, add= )

ks.test(Zstar, "pnorm")

quantis<- qnorm(p= (0:10)/10)
cutStar<- cut(x=Zstar, breaks= quantis)
cutStar
contaStar <- table(cutStar)
contaStar

x2<- ((contaStar -100)^2)/100
x2

#Alternativa
Z2Star<- replicate(ene, sum(runif(12)-0.5))
Z2Star

# Tenho um elipsoide na normal bivariada, fazer esse grafico e depois 
#  calcular quais seriam essas circunferencias que contem 10% da massa


####################### AULA 15/05 #################################################

#nornal p-variada
#Normal univariada
#Normal Bivariada

qchi<- qchisq(0.5, 2)


#Matriz com 1000 valores calcular o produto escalar de cada um dos 
#pontos e verficar qual deles tem valor menor que 1,386294
X <- matrix(Zstar, 500, 2)
distancia<-apply(X**2, 1, sum)

# Matriz X com 500 vetores aleatórios em R² com distribuição normal padrão

mean(distancia<=qchi)

# Produto da matriz por sua transposta: matriz 500 x 500
produto_matrizes <- X %*% t(X)
produto_matrizes

#Transformação de Box-Muller
#https://chatgpt.com/share/682649c0-9a50-800f-936c-42b4b06cb735

#Transformação de Box-Muller: Etapas:

#Método da Inversa
#Metódo da Convolução
#Método da transformação
#Método da aceitação/Rejeição

# TRANSFORMAÇÃO DE BOX-MULLER
# ------------------------------
set.seed(123)  # Reprodutibilidade

# Número de amostras
n <- 10000

# Gerar variáveis U(0,1)
U1 <- runif(n)
U2 <- runif(n)

# Aplicar Box-Muller
Z1 <- sqrt(-2 * log(U1)) * cos(2 * pi * U2)
Z2 <- sqrt(-2 * log(U1)) * sin(2 * pi * U2)

# ------------------------------
# ESTATÍSTICAS DESCRITIVAS
# ------------------------------
cat("Estatísticas básicas:\n")
cat("Z1: média =", mean(Z1), ", desvio padrão =", sd(Z1), "\n")
cat("Z2: média =", mean(Z2), ", desvio padrão =", sd(Z2), "\n")
cat("Correlação entre Z1 e Z2:", cor(Z1, Z2), "\n\n")

# ------------------------------
# HISTOGRAMAS + CURVA NORMAL
# ------------------------------
par(mfrow = c(1, 2))  # Dois gráficos lado a lado

hist(Z1, breaks = 50, freq = FALSE, col = "lightblue", main = "Histograma de Z1", xlab = "Z1")
curve(dnorm(x), add = TRUE, col = "red", lwd = 2)

hist(Z2, breaks = 50, freq = FALSE, col = "lightgreen", main = "Histograma de Z2", xlab = "Z2")
curve(dnorm(x), add = TRUE, col = "red", lwd = 2)

# ------------------------------
# QQ-PLOTS
# ------------------------------
par(mfrow = c(1, 2))  # Dois gráficos lado a lado

qqnorm(Z1, main = "QQ-Plot Z1")
qqline(Z1, col = "red")

qqnorm(Z2, main = "QQ-Plot Z2")
qqline(Z2, col = "red")


#matrizpontos2 <- matrix(pontos2$x, pontos2$y)

#distancias2<- aplly(matrizpontos2, 500, 2))
#mean(distancias2 <= qchisq(p=0.5, df=2))
#mean(diag(matrizpontos2 %*% t(matrizpontos2)))

T <- Z1 / Z2

# Visualizar
hist(T, breaks = 100, col = "lightblue", freq = FALSE, main = "Z1 / Z2 ~ Cauchy(0,1)", xlim = c(-500, 500))
curve(dcauchy(x), col = "red", lwd = 2, add = TRUE) 


