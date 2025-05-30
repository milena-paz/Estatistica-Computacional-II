#População Normal, media=10 e variancia=2.
#Gerar 1000 amostras para:
amostra <- list()
ene <- c(20,50,100,200,500)
for(n in ene)
  amostra[[paste0("n",n)]] <- replicate(1E3, rnorm(n,10,sqrt(2)) )

#Para cada amostra gerada, calcular a media amostral
medias<- lapply(amostra,apply, 2, mean)

#Fazer um histograma p/ cada n, estatisticas descritivas e teste da normalidade
par(mfrow=c(3,2))
mapply(hist,medias, freq=F, breaks=25, main=paste("n=",c(20,30,50,200,500)),
       xlab= expression(bar(X)))

dpXbarTeoricos <- sqrt(2*(1/c(20,30,50,200,500)))
dpMedias <- sapply(medias, sd)
abs(dpMedias- dpXbarTeoricos)

sapply(c(20,30,50,200,500), function(i){
  ks.test(medias[[paste0("n",i)]], pnorm, mean=10, sd=sqrt(2/i) )
})
