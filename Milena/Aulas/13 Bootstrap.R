#### Bootstrap ####
# É uma abordagem não-paramétrica de estimação.
#Consistencia de estimador:
# aproximar-se do verdadeiro valor do parâmetro quando o tamanho amostral cresce
#
#A estimativa bootstrap não é consistente no sentido probabilístico.
# Por exemplo, a estimação da média quando a distribuição não tem
#variância finita (TCL invalido), ou quando se estima o máximo e o mínimo.
# Em geral, sua consistência depende da valized do Teorema Central do Limite.

#AMOSTRA BOOTSTRAP:
# - Com reposição da amostra original
# - A Fn, acumulada empírica, desempenha o papel da FDA
# - Fn*: função de distribuição bootstrap, desempenha o papel de Fn

# Função distribuição empírica de amostra original
original <- c(7, 5, 3, 9, 6)
Fn <- ecdf(original)
summary(Fn)
knots(Fn)
plot(Fn, ylim = c(0, 1.1) , main = "")
text(knots(Fn), 1:5/5, knots(Fn), cex = 0.8, pos = 3)

# geração de amostra bootstrap
set.seed(666)
amostra.boot <- sample(original, 5, replace = T)
mean(amostra.boot)

# geração de outra amostra bootstrap
(amostra.boot <- sample(original, 5, replace = T))
mean(amostra.boot)

#Aproximação Monte Carlo da distribuição bootstrap
u0s.boot2 <- replicate(1000, mean(sample(original, 5, replace = T)))
mean(u0s.boot2)
hist(u0s.boot2, freq = F, ylab = "Densidade",main = "Aproximação Monte Carlo")
lines(density(u0s.boot2), col = "blue")

## PROCEDIMENTO: ############################################################
# 1- Geração de amostras bootstrap com reposição a partir da distribuição
#empírica dos dados originais;
# 2- Cálculo da estatística T(Fn*) que estima T(F)
# 3- Repetem-se os outros passos B vezes (B grande).
#############################################################################


#### EXEMPLO ####
#Correção de Viés
set.seed(666)
# amostra pequena
n <- 25
# amostra original
x <- rnorm(n)
head(x)
# verdadeiro valor do parâmetro
theta <- 1
# EMV da variância (variância amostral não corrigida)
theta.hat <- function(x) var(x) * (n - 1)/n
# estimativa amostral do parâmetro
(sigma2.hat <- theta.hat(x))
# estimativa parâmetro, erro amostral, vies esperado
c(sig2.amost = theta.hat(x), erro.amost = theta.hat(x) - theta, bias.esp = - 1/n)

# Aproximação Monte Carlo para o viés
# quantidade de reamostras bootstrap
N <- 5000
# vetor com as estimativas bootstrap de EMV de sigma2
vetor <- replicate(N, theta.hat(sample(x, n, replace = TRUE)))
# estimação de theta.estrela
theta.star <- mean(vetor)
# estimativa bootstrap da variância e estimativa do viés
c(theta.star, theta.star - theta.hat(x))

#### EXEMPLO 2 ####
data(bootstrap,law)

#objetivos: estimar o erro padrão por bootstrap da correlação entre LSAT e GPA
n<-dim(law)[1]
#amostras bootstrap
B<- 1E3
cor.boot <- function(){
  ind <- sample(nrow(law),15,replace=T)
  return(with(law[ind,], cor(LSAT,GPA)))
}
correl <- replicate(B, cor.boot())
sd(correl)
##
#usando a função boot() {boot}
r <- function(x,i)
  cor(x[i,1],x[i,2])
obj <- boot::boot(data=law,statistic=r, R= 2E3)
obj

#intervalos de confiança para a correlação 

boot.est <- boot(law, r,R=2E3)
#BCA
boot.ci(boot.est,conf=0.95,type="bca")
rboott <- function(x){ cor(law[x,1],law[x,2]) }
boott(1:nrow(law),theta=rboott, nbootsd=100, nboott=1000)$confpoints

#### Intervalos de Confiança Bootstrap ####
# Não são exatos, seu nível de confiança é menor que o nível de conf. nominal.
# Mas se o estimador bootstrap for consistente, o IC também será.

## Método Percentil (de Efrom) ##
# 

surimi <- c(41.28, 45.16, 34.75, 40.76, 43.61, 39.05, 41.20, 41.02, 41.33,
            40.61, 40.49, 41.77, 42.07, 44.83, 29.12, 45.59, 41.95, 45.78,
            42.89, 40.42, 49.31, 44.01, 34.87, 38.60, 39.63, 38.52, 38.52,
            43.95, 49.08, 50.52, 43.85, 40.64, 45.86, 41.25, 50.35, 45.18,
            39.67, 43.89, 43.89, 42.16)
summary(surimi)
#teste de normalidade de shapiro-wilk
(surimi.test <- shapiro.test(surimi))
qqnorm(surimi);qqline(surimi)
text(1, 35, cex = 0.8,
     paste0(surimi.test$method,"\np = ", round(surimi.test$p.value, 3)))
# IC t com 95% - assumindo normalidade (tamanho amostral)
t.test(surimi)$conf.int[1:2]

## Método Percentil t de Efrom ##

# Estatística bootstrap T.star = (theta.star - theta_n)/S.star
# T.star ~ t_(n-1)
#Em casos gerais theta.star precisaria de aproximação Monte Carlo para gerar o IC

## Procedimento:
# Para cada uma de N amostras bootstrap há uma estimativa theta.star


#Exemplo Surimi
qt(p=c(0.025,0.975),df=39)
u0 <- mean(surimi)
dp<-sd(surimi)
IC <- qt(p=c(0.025,0.975),df=39)*(dp/sqrt(40))+u0
IC
n<- length(surimi)
# qute de amostras bootstrap
B <- 1000
Sh <- dp/sqrt(n)
# cálculo média e estatística t por amostra bootstrap
  est.boot <- function(x) {list(med = mean(x), te = sqrt(n)*(mean(x)-u0)/sd(x))}
matriz <- replicate(B, est.boot(sample(surimi, n, replace = T)))
amostras.boot <- matrix(unlist(matriz),ncol = 2, byrow=T)
colnames(amostras.boot) <- c("theta", "t")
theta.star <- amostras.boot[, "theta"]
t.star <- amostras.boot[, "t"]
# compara média amostral com as médias bootstrap
  c(u0, mean(theta.star))
# compara erro padrão da amostra com erro padrão bootstrap
  c(Sh, sd(theta.star))
# aparenta simetria e normalidade?
  summary(t.star)
#q-q plot
qqnorm(t.star); qqline(t.star)
#percentis de t bootstrap 2.5% e 97.5%
quantile(t.star,probs=c(0.025,0.975))
#quantis de t com n-1 graus de liberdade 
qt(p=c(0.025,0.975),df=39)
#Intervalo de confiança bootstrap percentil t 
quantile(t.star,probs=c(0.025,0.975))*(dp/sqrt(40))+u0
#Intervalo de confianca t de student
qt(p=c(0.025,0.975),df=39)*(dp/sqrt(40))+u0

# usando o pacote bootstrap
set.seed(666)
library("bootstrap")
# função para erro padrão da amostra bootstrap
sdmean <- function(x, ...) {sqrt(var(x)/length(x))}
# função para cálculo IC bootstrap
boott(surimi, theta = mean, sdfun = sdmean, nboott = 1000,
      perc = c(0.025,0.975) #bootstrap percentile t
      )$confpoints

## Bootstrap iterado ##

# double bootstrap - "bootstrap" package
set.seed(666)
# IC bootstrap 95% - percentile t c/ bootstrap aninhado
boott(surimi, theta = mean, nbootsd = 100, nboott = 1000,
      perc = c(0.025,0.975))$confpoints

# correção de viés - bootstrap BCa e ABC
library("boot")
set.seed(666)
# IC bootstrap BCa
  # estimação dados os dados x e o conjunto de índices i
  fboot <- function(x, i) mean(x[i])
# gera as estimativas bootstrap
  bs <- boot(surimi, fboot, R = 1000)
# IC 95% bootstrap BCa
  boot.ci(bs, type = "bca" , conf = 0.95)
  
# IC bootstrap ABC
# usa média pondereada
fabc <- function(x, w) w %*% x
# IC 95% bootstrap ABC
abc.ci(surimi, fabc, conf = 0.95)

