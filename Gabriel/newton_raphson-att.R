f_expr <- expression(x^2 + 3*x - 2)

# Função para transformar expressão em função de x
expr_to_fun <- function(expr) {
  function(x) eval(expr)
}

# Convertendo f para função
f <- expr_to_fun(f_expr)

# Calculando derivada da expressão
der_f_expr <- D(f_expr, "x")

# Convertendo derivada em função
der_f <- expr_to_fun(der_f_expr)

newt <- function(f, x0, der_f, epson = 1e-9, max_iter = 20){
  
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

raiz <- newt(f, der_f, x0 = 1000)
cat("Raiz encontrada: ", raiz, "\n")


#####para passar a calcular a derivada dentro da função
# mostrando com um exemplo
# g <- expression(sin(x))
# g[[1]]
# sin(x)
# f <- function(x) {g[[1]]}
# f(0)
# sin(x)