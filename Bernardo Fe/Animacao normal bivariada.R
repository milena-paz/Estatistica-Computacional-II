library(gifski)
source("funcoes rng.R")

pontos <- geradornormalbivariada(1e5,12,meu=F,intervalo=c(-sqrt(3),sqrt(3)))
 
animation <- function(n,frames,spf,cores,...)
{
  for(i in 1:n)
  {
    plot(pontos,asp=1)
    parametric.draw(c(0,2*pi),circle(raio.quantil(0.99,...)),...)
    legend(legend = ,x=-20,y=8.5)
  }
}

animation <- function(n=0.999,frames,spf,...)
{
  op <- par(no.readonly = T)
  par(bg="#e0e0e0")
  
  for(i in 1:ceiling(1/spf))
    plot(pontos,asp=1,pch=".",cex=1.5)
  
  for(j in 1:frames)
  {
    plot(pontos,asp=1,pch=".",cex=1.5)
    parametric.draw(c(0,2*pi),circle(raio.quantil(j*n/frames,...)),col="#fd558f",lwd=5)
    legend(x=-15,y=12,legend = c(paste("raio:",raio.quantil(j*n/frames,...)),
                                  paste("quantil teorico:",j*n/frames),
                                  paste("quantil empirico:",quantilempirico(pontos,raio.quantil(j*n/frames,...))/(1e5/2))),cex=1.4)
  }
  for(a in 1:ceiling(1/spf))
  {
    plot(pontos,asp=1,pch=".",cex=1.5)
    parametric.draw(c(0,2*pi),circle(raio.quantil(j*n/frames,...)),col="#fd558f",lwd=5)
    legend(x=-15,y=12,legend = c(paste("raio:",raio.quantil(n,...)),
                                 paste("quantil teorico:",j*n/frames),
                                 paste("quantil empirico:",quantilempirico(pontos,raio.quantil(n,...))/(1e5/2))),cex=1.4)
  }
  for(k in 1:ceiling(1.5/spf))
  {
    plot(pontos,asp=1,pch=".",cex=1.5)
    
    parametric.draw(c(0,2*pi),circle(raio.quantil(0.99,...)),col="#ffba00",lwd=2)
    parametric.draw(c(0,2*pi),circle(raio.quantil(0.95,...)),col="#fb0044",lwd=2)
    parametric.draw(c(0,2*pi),circle(raio.quantil(0.50,...)),col="#0070c3",lwd=2)
    
    legend(x=-15,y=12,legend = c("99%","95%","50%"),col=c("#ffba00","#fb0044","#0070c3"),lty=c(1,1,1),cex=1.4,lwd=c(4,4,4))
  }
  par(op)
}
save_gif(
  animation(n=0.9999,frames=60,spf=1/10,desvio=2*sqrt(3)),
  gif_file = "animacao.gif",
  width = 800,
  height = 800,
  delay = 1/10,
  loop = TRUE,
  progress = TRUE)