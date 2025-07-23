#Busca de raizes

#caso 1 - univariado

#iterativo

#Método de Newton-Raphson

fx = function(x) x^2 - 4
fx1 = function(x) 2 * x

x0 = 3

erro = 1e-9

x1 = x0 - (fx(x0)/fx1(x0))

x1

x2 = x1 - (fx(x1)/fx1(x1))

x2

x3 = x2 - (fx(x2)/fx1(x2))
x3

#implementar o método


# função
f <- function(x) x^2 - 4

# derivada
f_prime <- function(x) 2 * x

# Método de Newton-Raphson
newton_raphson <- function(f, f_prime, x0, tol = 1e-9, max_iter = 100) {
  x <- x0
  for (i in 1:max_iter) {
    fx <- f(x)
    dfx <- f_prime(x)
    
    if (dfx == 0) {
      stop("Derivada nula. Método falhou.")
    }
    
    x_new <- x - fx / dfx
    
    cat(sprintf("Iteração %d: x = %.10f\n", i, x_new))
    
    if (abs(x_new - x) < tol) {
      return(x_new)
    }
    
    x <- x_new
  }
  warning("Número máximo de iterações atingido.")
  return(x)
}

raiz <- newton_raphson(f, f_prime, x0 = 3)
cat("Raiz encontrada:", raiz, "\n")

new_f <- function(x) x^3 + 4

raiz <- newton_raphson(new_f, f_prime, x0 = 3