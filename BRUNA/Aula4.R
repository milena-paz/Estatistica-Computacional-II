amostra <- rnorm(30)
qqnorm(amostra)
qqline(amostra)


amostra2 <- runif(1000)
acf(amostra2)

U <- runif(12)
U.star <- U-0.5
Fn <- ecdf(U.star)
plot(Fn)
abline(0.5,1)
ene <- 1e3
matriz <-matrix(runif(ene*12)-0.5, ncol = 12)
z.star <- apply(matriz, 1, sum)
f.star <- ecdf(z.star)
plot(f.star)
curve(pnorm,from = 4,to=4, add = T)
ks.test(z.star, "pnorm")
qnorm(p=(1:10/10))
quantis <- qnorm(p=(1:10/10))
cut.star <- cut(x=z.star, breaks = quantis)
conta.star <- table(cut.star)
conta.star 
1- pchisq(2.58, df=10)













