## Suavização 

head(faithful)        # mostra as primeiras linhas
summary(faithful)     # resumo estatístico
x <- faithful$waiting

hist(faithful$waiting, breaks = 4, freq = F) # histograma dos tempos de espera
breaks_sturges <- nclass.Sturges(x)
breaks_scott <- nclass.scott(x)
breaks_fd <- nclass.FD(x)

breaks_sturges
breaks_scott
breaks_fd
par(mfrow=c(1,3))
hist(x, breaks=breaks_sturges, main="Sturges")
hist(x, breaks=breaks_scott, main="Scott")
hist(x, breaks=breaks_fd, main="Freedman–Diaconis")

# Dados simulados
x<- c(0,1, 1.1, 1.5, 1.9, 2.8, 2.9, 3.5)
n <- length(x)
xgrid <- seq(from= min(x)-1, to= max(x)+1, by=0.01)
h <- 0.4
cont <- sapply(x, function(a) dnorm((xgrid-a)/h)/(n*h))
plot(xgrid, rowSums(cont), lwd=2, xlab = "x", ylab = expression(hat(f)(x)))
rug(x, lwd = 2)
out <- apply(cont, 2, function(b), lines(xgrid, b, lty=2))

fhat <- kde(x=iris[,3])
plot(fhat,cont=50)



