#### BUSCA DE RAIZES ####
# CASO 1: univariado ----
# Método de Newton-Raphson:
# iterativo:
# Xn = Xn-1 - f(Xn-1)/f'(Xn-1)
# critério de parada:
# |Xn - Xn-1| < epsilon; pra um epsilon muito pequeno
#IMPORTANTE DEFINIR:
# Xo é onde irá começar a buscar

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
  if(flinha(atual))
    stop(cat("ERRO: ponto crítico atingido"))
  return(atual-f(atual)/flinha(atual))
}

raiz <- function(x0,e=1E-9){
  prox <- proximo(x0)
  if(abs(x0 - prox)< e)
    return(prox)
  return(raiz(prox,e))
}

raiz(0)

################
# IMPLEMENTAÇÃO DO ALGORITMO DE NEWTON-RAPHSON
# Critérios de parada:
# iterações <= 20
# OU |Xn - Xn-1| < epsilon = 1E-3

#valores de entrada:
# max :  número máximo de iteracoes
# e : epsilon
# x0 : valor inicial

#EQUACOES
#A) X^2+3X-2=0
#B) X^3-6X^2=0
#C) x^3 - 11x - 6 = 0
#D) X^3 - 1=0 SOLUCAO COMPLEXA..
A <- function(x){
  x^2+3*x-2
}

dA <- function(x){
  2*x + 3
}

proximo <- function(atual){
  if(dA(atual)==0)
    stop(cat("ERRO: ponto crítico atingido"))
  return(atual-A(atual)/dA(atual))
}

##SOLUCAO RECURSIVA
buscaRaiz <- function(x0,max=20,e=1E-9){
  prox <- proximo(x0)
  if(abs(x0 - prox)< e | max==1)
    return(prox)
  return(buscaRaiz(prox,max-1,e))
}

x1<-buscaRaiz(x0=-4)
x2<-buscaRaiz(x0=0)
#visualizando a curva
curve(A,from=-4,to=1,col="blue2")
abline(h=0,lty=2,lwd=2)
points(c(x1,x2),A(c(x1,x2)),pch=19,col="red")

##SEGUNDA EQUACAO
B <- function(x){
  x^3-6*x^2+11*x-6
}

dB <- function(x)
  3*x^2 - 12*x + 11

proximo <- function(atual){
  if(dB(atual)==0)
    stop(cat("ERRO: ponto crítico atingido"))
  return(atual-B(atual)/dB(atual))
}

x <- c(buscaRaiz(x0=0),buscaRaiz(x0=1.5),buscaRaiz(x0=1.6))
##VISUALIZANDO A CURVA
curve(B,from=0,to=4,col="blue2")
abline(h=0,lty=2,lwd=2)
points(x,B(x),pch=19,col="red")

##TERCEIRA EQUACAO
C <- function(x){
  x^3 - 11*x -6
}

dC<- function(x){
  3*x^2 - 11
}

proximo <- function(atual){
  if(dC(atual)==0)
    stop(cat("ERRO: ponto crítico atingido"))
  return(atual-C(atual)/dC(atual))
}

x<-c(buscaRaiz(3),buscaRaiz(0),buscaRaiz(-2))

##VISUALIZANDO A CURVA
curve(C,from=-4,to=4,col="blue2")
abline(h=0,lty=2,lwd=2)
points(x,C(x),pch=19,col="red")

## QUARTA EQUACAO

De <- function(x)
  x^3 -1

dDe <- function(x)
  3*x

proximo <- function(atual){
  if(dDe(atual)==0)
    stop(cat("ERRO: ponto crítico atingido"))
  return(atual-De(atual)/dDe(atual))
}

x <- buscaRaiz(x0=2)

#VISUALIZANDO A CURVA
curve(De,from=-2,to=2,col="blue2")
abline(h=0,lty=2,lwd=2)
points(x,De(x),pch=19,col="red")

###########################

#FUNCAO stats::D
f <- expression(x^2+3*x-2)
df <- stats::D(expr=f,name="x")
x <- -1.5
eval(f)
eval(df)

#pode ser usada para usar a funcao como entrada e derivar dentro da funcao (!!)

#EQUACAO: t^t = 49
# t^t - 49=0
f<-expression(t^t-49)
df <- D(f,name="t")
#derivada de acordo com wolfram alpha: x^x + x^x*log(x)
#raiz exata = 3.27803
t <- 3.27803
eval(f)

curve(x^x-49,from=3,to=4,col="blue")
abline(h=0,lty=2,lwd=2)
points(t,eval(f),col="red",pch=19)


#####################################

avalia <- function(expr, val){
  x <- val
  return(eval(expr))
}

newtonR <- function(expr, x0, max=20, e=1E-5){
  #-------------------------------------------#
  #expr: expression() com variavel de nome "x"
  #x0: valor inicial
  #max: numero maximo de iterações
  #e: erro
  #-------------------------------------------#
  df <- D(expr, name="x")
  diff <- e+1
  while(diff > e & max>=1){
    derivada <- avalia(df,x0)
    if(derivada==0){
     cat("Ponto crítico encontrado"); break 
    }
    prox <- x0 - avalia(expr,x0)/derivada
    diff <- abs(x0 - prox)
    x0 <- prox
    max <- max-1
  }
  return(x0)
}

newtonR(f, x0=5,e=1E-9)

######################################

#METODO NEWTON-RHAPSON
#importante:
# -escolha de x0
# - f'(x) e f''(x) não nulas em [a,b] e preservem o sinal
# - convergencia

#Temos que essas coisas nao sao problemas para uma função acumulada de uma
#VA contínua! Podemos então tratar a função quantílica como a solução de uma equaçao:
#Q(p)=F^-1(x); F(x)=p -> F(x) - p = 0 #(!!)
#Assim, podemos usar newton-rhapson

#EXEMPLO:
#quantílica da normal padrão:
# P = 0.975
qnorm(0.975)

f <- function(x){
  pnorm(x) - 0.975
}

proximo <- function(atual){
  if(dnorm(atual)==0)
    stop(cat("ERRO: ponto crítico atingido"))
  return(atual-f(atual)/dnorm(atual))
}

erro <- .Machine$double.eps^0.25
x <- buscaRaiz(1.2,max=20,e=erro)
abs(x-qnorm(0.975))

buscaRaiz.2 <- function(fun,deriv,x0,max,e){
  diff <- e+1
  cont <- 0
  while(diff > e & max-cont>0){
    derivada <- deriv(x0)
    if(derivada==0){
      cat("Ponto crítico encontrado\n"); break 
    }
    prox <- x0 - fun(x0)/derivada
    diff <- abs(x0 - prox)
    x0 <- prox; cont <- cont + 1
  }
  cat(cont," iterações\n")
  return(x0)
}

x2 <- buscaRaiz.2(f, dnorm, as.complex,max=100,erro)
abs(x2-qnorm(0.975))
#x0 problemático:
x2 <- buscaRaiz.2(f, dnorm, -1E10,max=1E3,erro)
abs(x2-qnorm(0.975))
#x0 gerado aleatoriamente
x2 <- buscaRaiz.2(f, dnorm, x0=rnorm(1),max=100,erro)
abs(x2-qnorm(0.975))

####################################################
####  MÉTODO DA BISSECÇÃO ####
#Seja f(x) uma função contínua em [a,b] e com sinais opostos nos extremos (!!)

# - Verifica-se o sinal de f(x) em a e no ponto médio pm = (a+b)/2
#   * Caso seja sinal oposto de f(a): b <- pm
#   * Caso seja o mesmo que de f(a): a <- pm

f <- function(x)
  x^2 + 3*x -2
(xises <- seq(-5,5,1))
f(xises)

bisseccao <- function(f, a, b, e=erro){
  if(f(a)*f(b)>0)
    stop("Múltiplas ou nenhuma raiz no intervalo")
  diff <- e+1
  cont <- 0
  while(diff > e){
    pm <- (a + b)/2
    sinaldif <- f(a) * f(pm) < 0
    a <- a*0^as.numeric(!sinaldif) + pm*0^as.numeric(sinaldif)
    b <- b*0^as.numeric(sinaldif) + pm*0^as.numeric(!sinaldif)
    diff <- abs(a - b)
    cont <- cont+1
  }
  cat(cont," iterações\n")
  return((a+b)/2)
}

bisseccao(f, -4,-3)
bisseccao(f, 0,1)

#FUNCAO UNIROOT
uniroot(f, lower=-1,upper=1)$root
#FUNCOA POLYROOT
raizes<- polyroot(c(-1,0,0,1)) # x^3-1 = 0
raizes**3
v2 <- polyroot(c(1,0,0,1)) # x^3 + 1 =0
v2**3
