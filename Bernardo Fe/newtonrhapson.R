newtonrhapson <- function(x,y=0,func,deriv,epsilon=1e-9,qtd=20)
{
  while(abs(func(x)-y)>epsilon|qtd==0)
  {
    x <- x - (func(x)-y)/deriv(x)
    qtd <- qtd-1
  }
  return(x)
}
primeira.func <- function(x) x^2 + 3*x -2
deriv.primeira <- function(x) 2*x + 3
newtonrhapson(x=15,func=primeira.func,deriv= deriv.primeira)

segunda.func <- function(x) x^3 - 6*x^2 +11*x-6
deriv.segunda <- function(x) 3*x^2 -12*x+11

newtonrhapson(x=1,func=segunda.func,deriv= deriv.segunda)
newtonrhapson(x=2,func=segunda.func,deriv= deriv.segunda)
newtonrhapson(x=3,func=segunda.func,deriv= deriv.segunda)
newtonrhapson(x=-15,func=segunda.func,deriv= deriv.segunda)

terceira.func <- function(x) 2*x^3 - 22*x - 12
deriv.terceiro <- function(x) 6*x^2 - 22

newtonrhapson(x=-3,func=terceira.func,deriv= deriv.terceiro)
newtonrhapson(x=-0.562,func=terceira.func,deriv= deriv.terceiro)
newtonrhapson(x=3.562,func=terceira.func,deriv= deriv.terceiro)

quarta.func <- function(x) x^3 - 1
deriv.quarta <- function(x) 3*x^2

newtonrhapson(x=15,y=0,func=quarta.func,deriv.quarta)

f <- expression(x^2 + 3*x -2)
df <- D(expr=f,name='x')
x <- -1.5
eval(f)
eval(df)

quinta.funcao <- function(x) exp(x*log(x))

deriv.quinta <- function(x) exp(x*log(x))*(1+log(x))

newtonrhapson(x=3,y=49,func=quinta.funcao,deriv.quinta,epsilon = 1e-11)

quinta.funcao(3.278032)
