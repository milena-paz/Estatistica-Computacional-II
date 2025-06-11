library(gifski)
source("funcoes rng.R")
  
matriz.cov <- criasigmabivariado(5,1,-0.8)

medias <- c(1,3)

autov <- eigen(matriz.cov)

pontos <- gerabivariadadependente(1e4,medias,c(5,1),-0.8)

plot(pontos)

rotacao <- angulo(c(1,0),autov$vectors[1,])

parametric.draw(c(0,2*pi),elipse(elipse.a(0.95,autov$values),elipse.b(0.95,autov$values),
                                 rotacao,medias[1],medias[2]),col="red",lwd=3)

xcentrado <- scale(pontos,center=T,scale=F)

inversa <- solve(cov(pontos))

score <- diag(xcentrado%*%inversa%*%t(xcentrado))

animation2 <- function(n=0.999,frames,spf)
{
  op <- par(no.readonly = T)
  par(bg="#e0e0e0")
  
  for(i in 1:ceiling(1/spf))
    plot(pontos,asp=1,pch=".",cex=3)
  
  for(j in 1:frames)
  {
    plot(pontos,asp=1,pch=".",cex=2)
    parametric.draw(c(0,2*pi),elipse(elipse.a(j*n/frames,autov$values),elipse.b(j*n/frames,autov$values),
                                     rotacao,medias[1],medias[2]),col="#fd558f",lwd=5)
    
    legend("topright",legend = c(paste("A:",elipse.a(j*n/frames,autov$values),"B:",elipse.b(j*n/frames,autov$values)),
                                 paste("quantil teorico:",j*n/frames),
                                 paste("quantil empirico:",mean(score<qchisq(j*n/frames,2)))),cex=1.4)
  }
  for(a in 1:ceiling(3/spf))
  {
    plot(pontos,asp=1,pch=".",cex=2)
    parametric.draw(c(0,2*pi),elipse(elipse.a(j*n/frames,autov$values),elipse.b(j*n/frames,autov$values),
                                     rotacao,medias[1],medias[2]),col="#fd558f",lwd=5)
    legend("topright",legend = c(paste("A:",elipse.a(j*n/frames,autov$values),"B:",elipse.b(j*n/frames,autov$values)),
                                 paste("quantil teorico:",j*n/frames),
                                 paste("quantil empirico:",mean(score<qchisq(j*n/frames,2)))),cex=1.4)
  }
  for(k in 1:ceiling(3.5/spf))
  {
    plot(pontos,asp=1,pch=".",cex=2)
    
    parametric.draw(c(0,2*pi),elipse(elipse.a(0.99,autov$values),elipse.b(0.99,autov$values),
                                     rotacao,medias[1],medias[2]),col="#ffba00",lwd=5)
    parametric.draw(c(0,2*pi),elipse(elipse.a(0.95,autov$values),elipse.b(0.95,autov$values),
                                     rotacao,medias[1],medias[2]),col="#fb0044",lwd=5)
    parametric.draw(c(0,2*pi),elipse(elipse.a(0.5,autov$values),elipse.b(0.5,autov$values),
                                     rotacao,medias[1],medias[2]),col="#0070c3",lwd=5)
    
    legend("topright",legend = c("99%","95%","50%"),col=c("#ffba00","#fb0044","#0070c3"),lty=c(1,1,1),cex=1.4,lwd=c(4,4,4))
  }
  par(op)
}

save_gif(
  animation2(n=0.9999,frames=60,spf=1/15),
  gif_file = "animacao2.gif",
  width = 800,
  height = 800,
  delay = 1/15,
  loop = TRUE,
  progress = TRUE)
