# Função para gerar números pseudoaleatórios usando o método congruente linear
gerador_congruente <- function(n, a, b, m, x0) {
  x <- numeric(1e3)
  u <- numeric(1e3)
  
  x[1] <- x0
  
  for (i in 2:n) {
    x[i] <- (a * x[i - 1] + b) %% m
  }
  
  u <- x / m
  return(list(X = x, U = u))
}

# Exemplo de uso
n <-10000  # Quantidade de números a gerar
a <- 6253 # Multiplicador
b <- 0 # Incremento
m <- 2^31      # Módulo (comum para geradores LCG)
x0 <- 2  # Semente inicial

resultado <- gerador_congruente(n, a, b, m, x0)

# Mostrar os resultados
options(max.print = 1e6)  # aumenta para até 1 milhão de elementos

print(data.frame(Xn = resultado$X, Un = resultado$U))
     
sum(duplicated(resultado))

hist(resultado$X, breaks = 100)

boxplot(resultado$X)
















     