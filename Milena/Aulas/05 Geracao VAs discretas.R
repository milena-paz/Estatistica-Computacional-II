## GERACAO DE VARIAVEIS ALEATORIAS DISCRETAS

# X ~ Geometrica(p)
# X: número de tentativas até o primeiro sucesso
#p(x)= P(X=x) = p(1-p)^(x-1), suporte: {1,2,3,...}
#P(x)=P(X<=j)=1-(1-p)^(j-1)
#OBS: no R, usa-se uma outra forma para o suporte {0,1,...}, onde Y = X - 1 ~ Geom(p)


#### ALGORITMO DO MÉTODO DA INVERSA (GEOMÉTRICA) ####
##1- Gerar U ~ Unif(0,1)
# Temos que:  1 - (1-p)^(j-1) <= U < 1 - (1-p)^j
# então:    (1-p)^j < 1 - U <= (1-p)^(j-1)
##2- Defina: X = min{j: (1-p) < 1-U}
#             = min{j: j*log(1-p) < log(1-U)}
#             = min{j: j > log(1-U)/log(1-p)}
#             = trunc( log(1-U)/log(1-p) ) + 1
# U é uniforme(0,1) entao:
#           X = trunc( log(U)/log(1-p) ) + 1

p<- 1/3
set.seed(666)
U <- runif(1E3)
X <- log(U) %/% log(1-p) + 1

#probs empiricas
table(X)/1E3
#teoricos
dgeom(0:17,1/3) #porque o R usa a outra parametrizaçao é 0:17 e nao 1:18

# Xlinha <- trunc(log(U)/log(1-p)) + 1
# all.equal(X,Xlinha)

##### ALGORITMO DA GERAÇÃO DE UMA BERNOULLI/BINOMIAL ####
#alvo: X ~ Bern(p)
## 1- Gerar U ~ Unif(0,1)
## 2- X = 1 , se U <= p
#  OU X = 0 , c.c.

#Para gerar UMA Binomial, basta repetir esse processo para n valores:
p <- 1/3
geraBinom<- function(n,p){
  U <- runif(n)
  return(sum(as.numeric(U<=p)))
}
X<-replicate(1E3,geraBinom(10,p))

#probs empiricas
table(X)/1E3
#teoricas
dbinom(0:10,10,p)

#TESTE QUI-QUADRADO
#H0: X é binomial
#H1: X não é binomial
teoricas<-dbinom(0:10,10,p) * 1E3
tabela <- c(table(X), rep(0,times=11-length(unique(X))))
#estatistica do teste
X2 <- sum((tabela-teoricas)^2/teoricas)

#p-valor
1 - pchisq(X2,df=11)

#### OUTRO MÉTODO PARA GERAR BINOMIAL ####
#procedimento e explicação no material:
#https://bessegato.github.io/disciplinas/EST066/02.5_geracao_discretas.html


#ESTUDAR GERAÇÃO DE POISSON