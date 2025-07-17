#simulando alguns modelos:
#modelo de regressão linear multipla.
#y= \beta_0 + \beta_1 X + \beta_2 1_A + \epsilon (1_A é uma funcao indicadora do grupo A)
#se grupo = A => y= \beta_0 + \beta_1 X + \beta_2
#se grupo = A^c => \beta_0 + \beta_1 X
#H_0: \beta_0=\beta_1=\beta_2=...=\beta_p = 0.
#H_1: pelo menos um \beta_i \not = 0. i= 1,2,3,...,p.
n <- 50
grupo <- rep(c("A","B"),each=n/2)
x <- rnorm(n)
epsilon <- rnorm(n,0,2)
y <- 5 - 2*x + 2*(grupo=="A") + epsilon

mL <- lm(y ~ x + grupo)

plot(0,type="n",xlim=c(-10,10),ylim=c(-2.5,20))
points(x=x[grupo=="A"],y=y[grupo=="A"],col="red",cex=1,pch=19)
points(x[grupo=="B"],y[grupo=="B"],col="blue",cex=1,pch=19)
abline(7.496-1.737,b=-2.453,col="red",lwd=2)
abline(7.496,b=-2.453,col="blue",lwd=2)

mL

summary(mL)$sigma

summary(mL)$fstatistic

1-pf(37.3028,2.0000,47.0000)

estimador.denovo <- function(n=50,x)
{
  grupo <- rep(c("A","B"),each=n/2)
  epsilon <- rnorm(n,0,2)
  y <- 5 - 2*x + 2*(grupo=="A") + epsilon
  
  mL <- lm(y ~ x + grupo)
  
  coeficientes <- mL$coefficients
  
  plot(0,type="n",xlim=c(-10,10),ylim=c(-2.5,20))
  points(x=x[grupo=="A"],y=y[grupo=="A"],col="red",cex=1,pch=19)
  points(x[grupo=="B"],y[grupo=="B"],col="blue",cex=1,pch=19)
  abline(coeficientes[1]-coeficientes[3],b=coeficientes[2],col="red",lwd=2)
  abline(coeficientes[1],b=coeficientes[2],col="blue",lwd=2)
  
  sumario <- summary(mL)
  
  estat.f <- sumario$fstatistic[1]
  
  resultados <- c(coeficientes,sumario$sigma,
                  estat.f)
  
  return(resultados)
}

valores <- replicate(1e3,estimador.denovo(x=x))
valores <- t(valores)

plota.estimados2 <- function(valores)
{
  par(mfrow=c(2,3))
  
  hist(valores[,1],freq=F,main="beta0")
  lines(density(valores[,1]),col="blue",lwd=2)
  abline(v=mean(valores[,1]),col="red",lty=2,lwd=3.5)
  print(quantile(valores[,1],probs=c(0.05,0.95)))
  
  hist(valores[,2],freq=F,main="beta1")
  lines(density(valores[,2]),col="blue",lwd=2)
  abline(v=mean(valores[,2]),col="red",lty=2,lwd=3.5)
  print(quantile(valores[,2],probs=c(0.05,0.95)))
  
  hist(valores[,3],freq=F,main="grupoA")
  lines(density(valores[,3]),col="blue",lwd=2)
  abline(v=mean(valores[,3]),col="red",lty=2,lwd=3.5)
  print(quantile(valores[,3],probs=c(0.05,0.95)))
  
  hist(valores[,4],freq=F,main="sigma")
  lines(density(valores[,4]),col="blue",lwd=2)
  abline(v=mean(valores[,4]),col="red",lty=2,lwd=3.5)
  print(quantile(valores[,4],probs=c(0.05,0.95)))
  
  hist(valores[,5],freq=F,main="estatística")
  lines(density(valores[,5]),col="blue",lwd=2)
  abline(v=mean(valores[,5]),col="red",lty=2,lwd=3.5)
  print(quantile(valores[,5],probs=c(0.05,0.95)))
}
plota.estimados2(valores)
#repetindo tudo para \beta_1 = \beta_2 =0
estimador.denovo2 <- function()
{
  n <- 50
  epsilon <- rnorm(n,0,2)
  y <- 5 + epsilon
  mL <- lm(y ~ x + grupo)
  
  coeficientes <- mL$coefficients
  
  sumario <- summary(mL)
  
  estat.f <- sumario$fstatistic[1]
  
  resultados <- c(coeficientes,sumario$sigma,
                  estat.f)
  
  return(resultados)
}

resultados2 <- replicate(1e3,estimador.denovo2())

resultados2 <- t(resultados2)

plota.estimados2(resultados2)

#busca de raizes, por metodo de Newton Raphson.

f <- function(x) x^2 -4

newtonrap <- function(y,x0,func,f.linha,epsilon=1e-9)
{
  ultimoy <- y
  ultimox <- x0
  verifica <- FALSE
  contador <- 0
  while(!verifica)
  {
    atualx <- ultimox - func(ultimox)/f.linha(ultimox)
    atualy <- func(atualx)
    verifica <- abs(atualy - ultimoy)< epsilon
    ultimoy <- atualy
    ultimox <- atualx
    contador <- contador + 1
  }
  print(paste0("número de contagens:",contador))
  return(atualx)
}
newtonrap(0,-1,f,function(x) 2*x)
