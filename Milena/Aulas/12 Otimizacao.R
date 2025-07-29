#### OTIMIZACAO ####
#(MAXIMIZACAO OU MINIMIZACAO DE FUNCOES)


## exemplos de algoritmo (CORTEZ, Modern optimization with R ,2014)
# Busca exaustiva: envolve buscar por todo o espaço de busca
# Busca determinística: hillclimbing, tabu...
# Simulated annealing
# Algoritmo genético
# Colona de formigas
# Nuvem de partículas

## EXEMPLO 1
# Suponha que queiramos achar o máximo da função 
# f(x) = exp(-(x-2)^2)
#FUNCAO OPTIM()
#por padrao ela minimiza, entao pode-se usar -f(x) para maximizar
#de forma:

f <- function(x) -exp(-(x-2)^2)
#metodo padrao (Nelder-Mead)
optim(1,fn=f)
#metodo brent
optim(1,fn=f, method="Brent",lower=0,upper=4)
#metodo CG (usa variada)
df <- function(x) -2*(x-2)*f(x)
optim(par=1,fn=f,gr=df,method="CG")
# O método Nelder-Mead é mais sensível ao valor inicial, mas não usa derivada.

## EXEMPLO 2
#f(x) = sin(cos(x)*x)
f <- function(x) sin(cos(x)*x)
curve(f(x),0,10)
suppressWarnings(
(param <- sapply(c(2,4,6,7,8,9),function(i) optim(i,fn=f)$par))
)
f(param)

## EXEMPLO 2
#f(x) =  log(1+log(x))/log(1+x)
f <- function(x) -log(x+log(x))/log(1+x)
curve(-f(x), from= 0.3, to=50)
-f(2)
-f(5.792299)
optim(2,fn=f)
### NO CASO UNIVARIADO, PODEMOS USAR A FUNCAO optimize{stats} (ou optimise)
optimise(f,interval=c(2,10))


## EXEMPLO 3
#funcao de Rosenbrock
f <- function(x, y) (1 - x)^2 + 100*(y - x^2)^2

x <- seq(-2, 2, by = 0.15)
y <- seq(-1, 3, by = 0.15)
z <- outer(x, y, f)
library(rgl)
plot3d(f, xlim=c(-2,2),ylim=c(-1,3),col=viridis::viridis_pal(option="C"))
persp(rep(x,27),,z,phi = 45, theta = -45, col = "orange", shade = 0.00000001, ticktype = "detailed")
contour(x,y,z=z,col="brown",lwd=2,nlevels=10)
