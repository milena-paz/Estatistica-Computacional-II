graficodebarras <- function(valoresgerados,distribuicao,
                            numerodetentativas,cores=c("#aa00aa99","#ffaa3399","#ff0838"),...)
{
  intervalo <- c(min(valoresgerados):max(valoresgerados))
  
  tabela<- table(valoresgerados)/length(valoresgerados)
  
  #gambiarra forte, é só para criar a escala do barplot
  barplot(tabela,
          names.arg=intervalo,col="#ffffff00",border=F
          ,main="densidade dos valores gerados",ylim=c(0,max(tabela)+0.1))
  grid(col=cores[3])
  barplot(tabela,
          names.arg=intervalo,col=cores[1],border=F,add=T)
  
  barplot(distribuicao(intervalo,...),
          names.arg=intervalo,col=cores[2],add=T,border = F)
  legend("topright",legend = c("densidade teórica","densidade empírica"),col=cores,lwd=5)
}

geradorgeometrica <- function(q)
{
  return(log(runif(1))%/%log(1-q) + 1)
}

geradorgeometrica(0.999)

valoresgeradosgeometrica <- replicate(1e3,geradorgeometrica(1/3))

graficodebarras(valoresgeradosgeometrica,dgeom,prob=1/3)

geradorbernoulli <- function(p)
{
  valor <- runif(1)
  if(valor<p)
    return(1)
  return(0)
  
}

geradorbernoulli(1/3)

valoresgeradosbernoulli <- replicate(1e3,geradorbernoulli(1/3))

graficodebarras(valoresgeradosbernoulli,dbinom,size=1,prob=1/3)


geradorbinomial <- function(n,p)
{
  sum(replicate(n,geradorbernoulli(p)))
}

valoresgeradosbinom <- replicate(1e3,geradorbinomial(10,1/3))

tabelabinom <- table(valoresgeradosbinom)/1e3

graficodebarras(valoresgeradosbinom,dbinom,size=10,prob=1/3)


Ei <- dbinom(min(valoresgeradosbinom):max(valoresgeradosbinom),size=10,prob=1/3)

xisquadrado <- sum((tabelabinom - Ei)**2 / Ei)

pchisq(xisquadrado,11,lower.tail = F)
