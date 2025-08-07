#Boot strap pt 2 método percentil não é bom para amostras pequenas ou moderadas
#ou para dist. assimetricas ou de cauda pesada.

surimi <- c(41.28, 45.16, 34.75, 40.76, 43.61, 39.05, 41.20, 41.02, 41.33,
            40.61, 40.49, 41.77, 42.07, 44.83, 29.12, 45.59, 41.95, 45.78,
            42.89, 40.42, 49.31, 44.01, 34.87, 38.60, 39.63, 38.52, 38.52,
            43.95, 49.08, 50.52, 43.85, 40.64, 45.86, 41.25, 50.35, 45.18,
            39.67, 43.89, 43.89, 42.16)

summary(surimi)

qqnorm(surimi)
qqline(surimi)
c(variancia = var(surimi), desvio = sd(surimi))
surimi.test <- shapiro.test(surimi)
surimi.test

text(1, 35, cex = 0.8,
     paste0(surimi.test$method,"\np = ", round(surimi.test$p.value, 3)))
# IC t com 95% - assumindo normalidade (tamanho amostral)
t.test(surimi)$conf.int[1:2]

hist(surimi,freq=F)

qt(p=c(0.025,0.975),df=40-1)

n <- length(surimi)
u0 <- mean(surimi)
Sh <- sd(surimi)/sqrt(n)

# qute de amostras bootstrap
B <- 1000
 # cálculo média e estatística t por amostra bootstrap
est.boot <- function(x) {list(med = mean(x), te = sqrt(n)*(mean(x)-u0)/sd(x))}
matriz <- replicate(B, est.boot(sample(surimi, n, replace = T)))
amostras.boot <- matrix(unlist(matriz),ncol = 2, byrow=T)
colnames(amostras.boot) <- c("theta", "t")
theta.star <- amostras.boot[, "theta"]
t.star <- amostras.boot[, "t"]
 # compara média amostral com as médias bootstrap
c(u0, mean(theta.star))
# comapara erro padrão da amostra com erro padrão bootstrap
c(Sh, sd(theta.star))
 # aparenta simetria e normalidade?
summary(t.star)

quantile(t.star,probs=c(0.025,0.975))

u0 + quantile(t.star,probs=c(0.025,0.975))*Sh

-(40.96005-43.46946)/2

library("bootstrap")

sdmean <- function(x,...) (sqrt(var(x)/length(x)))

boott(surimi,theta=mean,sdfun=sdmean,nboott=1e3,perc=c(0.025,0.975))


library("boot")
# correção de viés - bootstrap BCa e ABC
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

law

cor(law$LSAT,law$GPA)

funcaoboot <- function(x,i) cor(x[i,1],x[i,2])

coisa <- boot(law, funcaoboot, R = 1000)
boot.ci(coisa, type = "bca" , conf = 0.95)

rboott <- function(i) cor(law[i,1],law[i,2])

boott(1:nrow(law),theta=rboott,nbootsd = 100,nboott=1000,perc=c(0.025,0.975))

