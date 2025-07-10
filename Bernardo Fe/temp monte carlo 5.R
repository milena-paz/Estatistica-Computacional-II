#Problema da ruína do jogador
#Jogador tem prob. P de ganhar R$1 real e (1-P) de perder R$1
#suponha que o jogador começou com i R$ e que jogadas sucessivas sejam independetes

#Qual é a probabilidade dele alcançar R$N antes de perder tudo

#Qual o número médio de jogadas até perder tudo

apostador <- function(i,p,n,ganho=1,contador=0)
{
  if(i>=n)
  {
    return(c(i,contador))
  }
  if(i<=0)
  {
    return(c(i,contador))
  }
  return(apostador(i + ganho*(2*rbinom(1,1,p)-1),p,n,contador+1))
}

apostador(3,0.4,7,1)

replicate(1e2,apostador(3,0.4,7,1))[1,]

sum(replicate(1e4,apostador(3,0.4,7,1))==7)

medias.apostador <- function(func,i,p,n,ganho=1,qtd)
{
  mean(replicate(qtd,func(i,p,n,ganho))[1,]>=n)
}

medias.apostador(3,0.4,7,1e4)

ate.a.falha.apostador <- function(func,i,p,n,ganho,qtd,chegar=F)
{
  #chegar=T é pra calcular chegar em n
  valores <- replicate(qtd,func(i,p,n,ganho))
  mean(valores[2,(valores[1,]>=n)*chegar])
}

ate.a.falha.apostador(apostador,3,0.4,7,1,1e4,T)

ate.a.falha.apostador(apostador,3,0.4,7,1,1e4,F)

ate.a.falha.apostador(apostador,3,0.2,7,1,1e4,T)

ate.a.falha.apostador(apostador,3,0.2,7,1,1e4,F)

ate.a.falha.apostador(apostador,3,0.5,7,1,1e4,T)

ate.a.falha.apostador(apostador,3,0.5,7,1,1e4,F)

medias.apostador(apostador,10,0.4,30,ganho=1,1e4)

medias.apostador(apostador,10,0.4,30,ganho=1,1e4)

apostador.burro <- function(i,p,n,ganho=1,contador=0)
{
  if(i<=0|rbinom(1,1,0.5))
    return(c(i,contador))
  i <- i + ganho*(2*rbinom(1,1,p)-1)
  return(apostador.burro(i,p,n,ganho,contador +1))
}

apostador.burro(10,0.4,30)

ate.a.falha.apostador(apostador.burro,3,0.4,7,1,1e4,T)

ate.a.falha.apostador(apostador.burro,3,0.4,7,1,1e4,F)

ate.a.falha.apostador(apostador.burro,3,0.2,7,1,1e4,T)

ate.a.falha.apostador(apostador.burro,3,0.2,7,1,1e4,F)

ate.a.falha.apostador(apostador.burro,3,0.5,7,1,1e4,T)

ate.a.falha.apostador(apostador.burro,3,0.5,7,1,1e4,F)

medias.apostador(apostador.burro,3,0.4,7,ganho=1,1e4)

medias.apostador(apostador.burro,3,0.4,7,ganho=1,1e4)

apostador.refeito <- function(i,p,n,contador=numeric(length(0:n)))
{
  if(contador[n+1]>=1)
  {
    return(contador)
  }
  if(contador[1]>=1)
  {
    return(contador)
  }
  i <- i + 2*rbinom(1,1,p) -1
  contador[i+1] <- contador[i+1] + 1
  apostador.refeito(i,p,n,contador)
}

apostador.refeito(3,0.4,7)

# P de jogdor iniciar em 3 e atingir 1 antes de N

valores <- replicate(1e4,apostador.refeito(3,0.4,7))

chegaram.em.n <- valores[,valores[8,]>=1]

mean(chegaram.em.n[2,]>=1)

#Ficou muito incompleto daqui pra cima


