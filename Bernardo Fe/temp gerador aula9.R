#Normal p-variada

dnormal.pvariada <- function(x,cov.mat,medias)
{
  exp(-t(x-medias)*cov.mat*(x-medias))/((2*pi)^(length(medias)/2)*(det(cov.mat))^(1/2))
}

gerabivariadadependente <- function(n,medias,sigma,rho)
{
valoresx2 <- rnorm(n,mean=medias[2],sd=sqrt(sigma[2]))

valoresx1 <- rnorm(n,mean=(medias[1]+
             rho*sqrt(sigma[2]/sigma[1])*(valoresx2-medias[2])),sd=sqrt(sigma[1]*(1-(rho)^2))
                   
pontos <- matrix(c(valoresx1,valoresx2),nrow=n,byrow = F))
                   
print(apply(pontos,2,mean))

print(cov(pontos))

plot(pontos)

return(pontos)
}

coisa <- gerabivariadadependente(1e3,c(1,3),c(2,1),-0.8)

#????????????????????
#eu já apaguei o ks.test
#mas o fantasma dele assombra a função
#não consigo terminar a aula ou debugar meus erros da função
coisa

xcentrado <- scale(ponto,center=T,scale=F)

inversa <- solve(cov(ponto))

scores <- diag(xcentrado%*%inversa%*%t(xcentrado))
