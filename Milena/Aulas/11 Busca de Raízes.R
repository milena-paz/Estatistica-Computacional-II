#### BUSCA DE RAIZES ####
# CASO 1: univariado ----
# Método de Newton-Raphson:
# iterativo:
# Xn = Xn-1 - f(Xn-1)/f'(Xn-1)
# critério de parada:
# |Xn - Xn-1| < epsilon; pra um epsilon muito pequeno
#IMPORTANTE:
# Xo e onde buscar

#exemplo:
# f(x) = x^2 - 4
# EQ: f(x)=0
# Xo = 3
# e = 1E-9

f<- function(x){
  x^2 - 4
}

flinha<- function(x){
  2*x
}

proximo <- function(atual){
  return(atual-f(atual)/flinha(atual))
}

raiz <- function(x0,e=1E-9){
  prox <- proximo(x0)
  if(abs(x0 - prox)< e)
    return(prox)
  return(raiz(prox,e))
}

raiz(3)
