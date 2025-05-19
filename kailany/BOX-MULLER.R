# ------------------------------
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

# ------------------------------
# TESTES DE NORMALIDADE
# ------------------------------
cat("\nTeste de normalidade (Shapiro-Wilk, usando 5000 valores):\n")
print(shapiro.test(Z1[1:5000]))
print(shapiro.test(Z2[1:5000]))

# ------------------------------
# DISPERSÃO (Z1 vs Z2)
# ------------------------------
par(mfrow = c(1,1))  # Reset layout
plot(Z1, Z2, pch = 20, col = rgb(0, 0, 1, 0.2), main = "Dispersão Z1 vs Z2",
     xlab = "Z1", ylab = "Z2")
