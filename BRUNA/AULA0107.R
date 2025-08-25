set.seed(123)
n_amostras <- c(1e3, 5e3, 1e4, 1e5)
repeticoes <- 1000

par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))

for (n in n_amostras) {
  medias <- numeric(repeticoes)
  
  for (i in 1:repeticoes) {
    u <- runif(n)
    medias[i] <- mean(u)
  }
  
  hist(medias, probability = TRUE,
       main = paste("Médias de Unif(0,1)\nn =", n),
       col = "skyblue", xlab = "Média", border = "white")
  
  # Adiciona curva normal teórica
  x <- seq(min(medias), max(medias), length.out = 200)
  lines(x, dnorm(x, mean = 0.5, sd = 1/sqrt(12*n)), col = "red", lwd = 2)
  
  mtext(paste0("Esperado: μ=0.5, σ=", round(1/sqrt(12*n), 5)), side = 3, line = 0.5, cex = 0.75)
}
