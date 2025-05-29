#CONVOLUCOES* continuaçao
set.seed(666)
Z.star <- replicate(1E3,sum(runif(12)-0.5))

x<- sample(1:1000,size=500)
y<- seq(1,1000,1)[!(1:1000 %in% x)]

matriz <- matrix(c(Z.star[x],Z.star[y]),nrow=500)
MDist<-apply(matriz**2,1,FUN=sum)
all.equal(diag(matriz%*%t(matriz)), MDist)
#o esperado é 0.5
mean(MDist <= qchisq(p=0.5,df=2))
#.95
mean(MDist <= qchisq(p=0.95,df=2))
#.99
mean(MDist <= qchisq(p=0.99,df=2))
## TRANSFORMAÇÃO DE BOX-MULLER ----
#U1 E U2 IID U1 ~ Unif(0,1)
#entao se: Z1 = sqrt(-2*log(U1))*cos(2*pi*U2)  e Z2= sqrt(-2*log(U1))*sin(2*pi*U2)
#Z1 e Z2 são normais padrão N(0,1)

##provando
#Z1^2 + Z2^2 = -2*log(U1)
#portanto U1= exp( (-1/2)*(Z1^2+Z2^2) )
#Z2/Z1=tg(2*pi*U2) -> U2=arctg(Z2/Z1)/(2*pi)

############ CALCULAR JACOBIANO E TERMINAR A TRANSFORMACAO, ENCONTRANDO AS MARGINAIS
##1a ALTERNATIVA ----
#APLICANDO BOX-MULLER NAS UNIFORMES
set.seed(109)
U1<- runif(1E3)
U2<- runif(1E3)
a<- sqrt(-2*log(U1))
b<- 2*pi*U2
Z1 <- a*cos(b)
Z2 <- a*sin(b)

ks.test(Z1,pnorm)
ks.test(Z2,pnorm)

matriz <- matrix(c(Z1,Z2),ncol=2)
MDist<-apply(matriz**2,1,FUN=sum)

for (p in c(0.5,.95,.99)) print(mean(MDist <= qchisq(p=p,df=2)))

##NORMAL PADRAO -> CAUCHY ----
#Z1/Z2
Cauchy <- Z.star[x]/Z.star[y]
#METODO DA INVERSA
Cauchy2 <- tan(pi*(U1-1/2))

hist(Cauchy2, freq=F, xlim=c(-500,500),breaks=100)

##2a ALTERNATIVA P/ GERAR PAR DE NORMAIS INDEPENDENTES ----
#1- Gerar U1 e U2 independentes uniformes(0,1)
#2- fazer V1 = 2*U1-1 e V2 =2*U2 -1, V1,V2iid uniformes(-1,1)
#3- S= V1² + V2², S~Unif(0,1)
### SE S>1, RETORNAR AO PASSO 1
#4- Z1 = sqrt(-2*log(S)/S)*V1 e Z2 = sqrt(-2*log(S)/S)*V2
##  temos que Z1 e Z2 são normais independentes quando V1,V2
##  é um ponto aleatõrio no circulo unitario
set.seed(666)
S<- 2
while(S>1){
  U <- runif(2)
  V <- 2*U-1
  S <- sum(V**2)
}
Z1<- sqrt(-2*log(S)/S)*V[1]
Z2 <- sqrt(-2*log(S)/S)*V[2]

gera <- function(){
  S<- 2
  cont<-0
  while(S>1){
    U <- runif(2)
    V <- 2*U-1
    S <- sum(V**2)
    cont<- cont+1
  }#fim loop
  Z<- sqrt(-2*log(S)/S)*V
  return(c("Z"=Z,"Tentativas"=cont))
} # fim funcao 
set.seed(666)
X<- replicate(1E3,gera())
1/mean(X[3,]) #deve se aproximar de pi/4
### histograma das Z geradas e a densidade normal padrao
hist(X[-3,],freq=F)
curve(dnorm,add=T,col="blue",lty=2,lwd=2)
ks.test(X[-3,],pnorm)
qqnorm(X[-3,])
qqline(X[-3,],col="blue",lwd=2)


# Q-Q PLOT ----
##exemplos exp, cauchy, t de student
amostra<- rexp(100)
qqnorm(amostra)
qqline(amostra,lwd=2,col="blue")

amostra<- rcauchy(100)
qqnorm(amostra)
qqline(amostra, lwd=2,col="blue")

amostra<- rt(100,3)
qqnorm(amostra)
qqline(amostra, lwd=2,col="blue")

set.seed(666)
X <- rnorm(10)
##QQplot usa as estatisticas de ordem de uma amostra e seus quantis
#em relação ao valor teorico (no caso de QQnorm, o quantil normal)
q.amostra <- X
q.teorico <- qnorm(rank(X)/10, mean(X), sd(X))

plot(x=q.teorico,y=q.amostra,main="QQplot normal",xlab="Quantis teóricos",
     ylab="Quantis Amostrais")


########## AULA 27/05 ######################
set.seed(666)
amostra<- rnorm(10)
rank(amostra)/length(amostra)

probs <- (rank(amostra)-0.5)/length(amostra)
probs

q.amostra<-amostra
q.teorico<- qnorm(probs, mean(q.amostra), sd(q.amostra))
q.teorico

plot(x=q.teorico, y= q.amostra, main="Q-Q plot Normal")
lines(x = qnorm(c(0.25, 0.75), mean(q.amostra), sd(q.amostra)), y = quantile(q.amostra, c(0.25, 0.75)))

amostra.exp<- rexp(50)
probs1<- (rank(amostra.exp)-0.5)/length(amostra.exp)

q.amostra1 <- amostra.exp
q.teorico1 <- qexp(probs1, rate= 1/ mean(amostra.exp))
lines(x=qexp(c(0.25,0.75), rate=1/mean(amostra.exp)), y=quantile(q.amostra1, c(0.25,0.75)))


############ GRÁFICO DA FUNÇÃO #######################

$$
  f(x) =
  \begin{cases}
\frac{2}{\sqrt{2\pi}} e^{-\frac{x^2}{2}} & \text{se } x \ge 0 \\
0 & \text{se } x < 0
\end{cases}
$$
  
2/(sqrt(2*pi)) * exp(1/2)


# 1. Definir a função de densidade alvo f_X(x)
f_X <- function(x) {
  ifelse(x < 0,
         0,
         (2 / sqrt(2 * pi)) * exp(-(x^2) / 2)
  )
}

# 2. Definir a função de densidade candidata g_Y(x)
# (Nota: no seu texto original g_Y(y) tinha condição "se x>=0",
# assumimos que a condição é sobre a variável da função, ou seja, "se y>=0" ou "se x>=0" para g_Y(x))
g_Y <- function(x) {
  ifelse(x < 0,
         0,
         exp(-x)
  )
}

# 3. Calcular a constante M
M <- (2 / sqrt(2 * pi)) * exp(1/2)
# print(paste("Valor de M:", M)) # Aprox. 1.315481

# 4. Definir a função candidata ajustada M * g_Y(x)
Mg_Y <- function(x) {
  M * g_Y(x)
}

# 5. Plotar a função alvo f_X(x)
# Determinar o limite superior para o eixo y.
# Como Mg_Y(x) deve ser >= f_X(x), M será o principal determinante do limite superior.
y_lim_max_plot <- M + 0.1 # Adiciona uma pequena margem visual

curve(f_X,
      from = 0,   # Ambas as funções são 0 para x < 0, então começamos de 0.
      to = 5,     # Intervalo onde as funções têm valores significativos.
      n = 500,    # Número de pontos para uma curva suave.
      xlab = "x",
      ylab = "Densidade",
      main = "Método de Aceitação/Rejeição: f_X(x) vs M*g_Y(x)",
      col = "blue", # Cor para f_X(x)
      lwd = 2,
      ylim = c(0, y_lim_max_plot) # Define os limites do eixo y
)

# Adicionar uma linha horizontal em y=0 para referência
abline(h = 0, col = "gray")

# Adicionar uma grade para melhor visualização (opcional)
grid()

# 6. Adicionar a função candidata ajustada M*g_Y(x) ao gráfico
curve(Mg_Y,
      from = 0,   # Mesmo intervalo
      to = 5,
      n = 500,
      add = TRUE,   # Adicionar ao gráfico existente
      col = "red",  # Cor para M*g_Y(x)
      lwd = 2,
      lty = 2)      # Estilo de linha tracejada para a função envelope

# 7. Adicionar uma legenda
# Usando expression() para melhor formatação das fórmulas na legenda
legend_texts <- c(
  expression(f[X](x) == frac(2, sqrt(2*pi)) * e^{-x^2/2}),
  expression(M %.% g[Y](x) == M %.% e^{-x})
)

legend("topright",
       legend = legend_texts,
       col = c("blue", "red"),
       lwd = 2,
       lty = c(1, 2), # Linha sólida para f_X(x), tracejada para M*g_Y(x)
       bty = "n",     # Sem caixa ao redor da legenda (opcional)
       cex = 0.8)     # Tamanho do texto da legenda (opcional, ajuste conforme necessário)



############################
# 1. Função de densidade alvo (renomeada para f_X_pdf para clareza)
f_X_pdf <- function(x_param) { # Argumento renomeado para evitar conflito com 'X' interno
  ifelse(x_param < 0,
         0,
         (2 / sqrt(2 * pi)) * exp(-(x_param^2) / 2)
  )
}

# 2. Constante M (renomeada para M_constante para clareza)
M_constante <- (2 / sqrt(2 * pi)) * exp(1/2)

# 3. Função para gerar UMA amostra, usando os nomes de variáveis do seu pseudocódigo
geradordeamostra <- function() {
  # Este laço while(TRUE) implementa a lógica "continuar tentando até aceitar".
  # Corresponde ao seu "while(f.X <= Y)" no sentido de que
  # o laço continua se a condição de rejeição (f.X <= Y) for satisfeita.
  while (TRUE) {
    # Linha do seu pseudocódigo: U = runif(1)
    U <- runif(1)
    
    # Linha do seu pseudocódigo: V = runif(1)
    V <- runif(1)
    
    # Linha do seu pseudocódigo: X = -log(U)
    # Este 'X' é o valor candidato gerado a partir da distribuição g_Y.
    X <- -log(U)
    
    # 'f.X' no seu pseudocódigo representa f_X_pdf(X). Calculamos isso:
    f_X_avaliado_em_X <- f_X_pdf(X)
    
    # Linha do seu pseudocódigo: Y = V * exp(-X) * M
    # Este 'Y' é o valor de comparação V * M_constante * g_Y(X).
    # Lembre-se que g_Y(X) é exp(-X) para a exponencial padrão.
    Y <- V * M_constante * exp(-X)
    
    # A condição do seu laço é "while(f.X <= Y)", que significa:
    # CONTINUE o laço se f_X_avaliado_em_X <= Y (condição de REJEIÇÃO).
    # O laço PARA (e X é aceito) se f_X_avaliado_em_X > Y (condição de ACEITAÇÃO).
    
    # Verificamos a condição de ACEITAÇÃO para sair do laço:
    if (f_X_avaliado_em_X > Y) {
      return(X) # X é aceito, então retornamos este valor.
    }
    # Se 'f_X_avaliado_em_X > Y' for falso, isso implica 'f_X_avaliado_em_X <= Y'.
    # Nesse caso, a amostra é rejeitada, o 'if' não é executado,
    # e o laço 'while(TRUE)' continua para a próxima tentativa.
  }
}

# --- Gerar N amostras usando a função com nomes de variáveis exatos ---
num_amostras_exatas <- 10000
amostras_geradas_exatas <- numeric(num_amostras_exatas)

for (i in 1:num_amostras_exatas) {
  amostras_geradas_exatas[i] <- geradordeamostra()
}

# --- Visualizar os resultados (o código de plotagem é o mesmo) ---
hist(amostras_geradas_exatas,
     breaks = 50,
     probability = TRUE,
     xlab = "x",
     ylab = "Densidade",
     main = paste("Amostras (Variáveis do Usuário, N=", num_amostras_exatas, ")"),
     xlim = c(0, max(c(amostras_geradas_exatas, 5), na.rm = TRUE))) # Limite do eixo x

# Sobrepor a densidade teórica f_X_pdf(x)
curve(f_X_pdf, # Usar o nome correto da função PDF aqui
      from = 0,
      to = max(c(amostras_geradas_exatas, 5), na.rm = TRUE),
      add = TRUE,
      col = "red",
      lwd = 2)

legend("topright",
       legend = c("Densidade Empírica", "Densidade Teórica f_X(x)"),
       fill = c(NA, "red"),
       border = c("black", NA),
       lty = c(NA, 1),
       lwd = c(NA, 2),
       col = c("black", "red"),
       bty = "n")

dbeta(0.2, shape1=2, shape2=5)
curve(dbeta(x, shape1 = 2, shape2 = 5),
      from = 0,  # Limite inferior para x
      to = 1,    # Limite superior para x
      n = 400,   # Número de pontos para desenhar a curva (para suavidade)
      xlab = "x",
      ylab = "Densidade f(x)",
      main = "Função Densidade de Probabilidade Beta(alpha=2, beta=5)",
      col = "steelblue",
      lwd = 2)
grid() # 
