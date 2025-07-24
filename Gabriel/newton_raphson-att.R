#f_expr <- expression(x^2 + 3*x - 2)

# # Código anterior (comentado para referência)
# # Função para transformar expressão em função de x
# expr_to_fun <- function(expr) {
#   function(x) eval(expr)
# }

# # Convertendo f para função
# f <- expr_to_fun(f_expr)

# # Calculando derivada da expressão
# der_f_expr <- D(f_expr, "x")

# # Convertendo derivada em função
# der_f <- expr_to_fun(der_f_expr)

newt <- function(f_expr, x0, epson = 1e-9, max_iter = 20){
  
  # converte expressão em função
  f <- function(x) eval(f_expr)
  
  # calcula derivada
  der_f_expr <- D(f_expr, "x")
  der_f <- function(x) eval(der_f_expr)
  
  x <- x0
  
  for(i in 1:max_iter){
    fx <- f(x)
    dfx <- der_f(x)
    
    if(dfx == 0){
      stop("Derivada Nula")
    }
    
    x_new <- x - fx/dfx
    cat(sprintf("Iteração %d: x = %.10f\n", i, x_new))
    
    if(abs(x_new - x) < epson){
      return(x_new)
    }
    
    x <- x_new
  }
  
  warning("Número máximo de iterações atingido")
  return(x)
}

raiz <- newt(f_expr, x0 = 1000)
cat("Raiz encontrada: ", raiz, "\n")



# Exemplo 1: função quadrática 
 f_expr <- expression(x^2 + 3*x - 2)
 raiz <- newt(f_expr, x0 = -15001)

# Exemplo 2: função trigonométrica
cat("\n--- Exemplo com função seno ---\n")
g_expr <- expression(sin(x))
raiz_seno <- newt(g_expr, x0 = -1.4999
cat("Raiz encontrada para sin(x): ", raiz_seno, "\n")

# Exemplo 3: função exponencial
cat("\n--- Exemplo com função exponencial ---\n")
h_expr <- expression(exp(x) - 2)
raiz_exp <- newt(h_expr, x0 = -1.5001)
cat("Raiz encontrada para exp(x) - 2: ", raiz_exp, "\n")