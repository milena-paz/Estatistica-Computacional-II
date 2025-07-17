#regressão linear Y = \beta_0 + \beta_1 X + \epsilon
#onde \epsilon \sim N(0,\sigma^2), sigma ^2 é fixo, ele é um modelo homocedástico.
#E[Y|X=x] = E[\beta_0 + \beta_1 X + \epsilon|X=x]= \beta_0 + \beta_1 x + 0
#Simular : Y=150 - 4x + \epsilon, \epsilon \sim N(0,10)

n <- 50
x1 <- runif(n,20,80)

x2 <- seq(20,80,length.out=n)

x3 <- rnorm(n,50,10)

dados.gerados1 <- regressao.linear(n,x1)

dados.gerados2 <- regressao.linear(n,x2)

dados.gerados3 <- regressao.linear(n,x3)

modelo <- lm(dados.gerados1$y ~ dados.gerados1$x)

modelo$residuals

str(summary(modelo))

summary(modelo)

summary(modelo)$sigma

sd(modelo$residuals)

regressao.linear <- function(x,beta0=150,beta1=-4,sigma=10,n=length(x))
{
  epsilon <- rnorm(n,0,sigma)
  y <- beta0 + beta1*x + epsilon
  plot(x,y)
  modelo.linear <- lm(y~x)
  return(c(modelo.linear$coefficients,summary(modelo.linear)$sigma))
}

n <- 50
x <- rnorm(n,50,10)

estimados <- t(replicate(1e3,regressao.linear(x)))

plota.estimados <- function(valores)
{
  par(mfrow=c(2,2))
  
  hist(valores[,1],freq=F,main="beta0")
  lines(density(valores[,1]),col="blue",lwd=2)
  abline(v=mean(valores[,1]),col="red",lty=2,lwd=3.5)
  print(quantile(valores[,1],probs=c(0.05,0.95)))
  
  hist(valores[,2],freq=F,main="beta1")
  lines(density(valores[,2]),col="blue",lwd=2)
  abline(v=mean(valores[,2]),col="red",lty=2,lwd=3.5)
  print(quantile(valores[,2],probs=c(0.05,0.95)))
  
  hist(valores[,3],freq=F,main="sigma")
  lines(density(valores[,3]),col="blue",lwd=2)
  abline(v=mean(valores[,3]),col="red",lty=2,lwd=3.5)
  print(quantile(valores[,3],probs=c(0.05,0.95)))
}

plota.estimados(estimados)
