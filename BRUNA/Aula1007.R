# Problema da ruína do jogador

set.seed(666)
p<- 0.4 # Probabilidade de ganhar
q <- 1-p
i <- 3  # 3 dólares
n <- 7 # 7 dólares
n_sim <-100 # num de simulação

simula_jogos <- function(i, n, p){
  passo <- 0
  fortuna <-i
  while (fortuna > 0 && fortuna <n) {
    fortuna <- fortuna + sample(c(1,-1), size= 1, prob = c(p, 1-p))
    passo <- passo+1
    
  }
  return(c(fortuna==n, passo))
}
  resultados <- replicate(n_sim, simula_jogos(i, n, p))
  vitorias <- resultados [1,]
  passos <- resultados [2, ]
  derrota <-1-vitorias
  mean(passos)
  mean(vitorias)  
  mean(derrota)
##-----------------------------------------------------------------------------------
  
  set.seed(666)
  
  # Parâmetros
  p <- 0.4           # Probabilidade de ganhar
  i <- 2            # Fortuna inicial
  N <- 6             # Fortuna alvo
  n_sim <- 10000     # Simulações
  
  # Função para simular 1 jogo com moeda honesta
  jogo_com_moeda <- function(i, N, p) {
    fortuna <- i
    passos <- 0
    terminou <- FALSE
    resultado <- NA
    
    while (!terminou) {
      # Moeda honesta: 1 = cara (joga), 0 = coroa (para)
      moeda <- sample(c(0, 1), size = 1)
      
      if (moeda == 0) {
        # Jogador decidiu parar
        resultado <- "parou"
        terminou <- TRUE
      } else {
        # Jogador continua o jogo
        jogada <- sample(c(1, -1), size = 1, prob = c(p, 1 - p))
        fortuna <- fortuna + jogada
        passos <- passos + 1
        
        if (fortuna <= 0) {
          resultado <- "faliu"
          terminou <- TRUE
        } else if (fortuna >= N) {
          resultado <- "vitoria"
          terminou <- TRUE
        }
      }
    }
    
    return(c(resultado, fortuna, passos))
  }
  
  # Simulação
  resultados <- replicate(n_sim, jogo_com_moeda(i, N, p))
  
  
  
  
  # Organizar os resultados
  tabela <- table(resultados[1, ])
  cat("\nFrequência dos resultados:\n")
  print(tabela)
  
  # Estatísticas
  valores_finais <- as.numeric(resultados[2, ])
  cat("\nMédia da fortuna final: ", round(mean(valores_finais), 2), "\n")
  cat("Média de jogadas até parar/falir/vencer: ", round(mean(as.numeric(resultados[3, ])), 2), "\n")
  
  #-------------------------------------------------
  
  
  # Função que simula uma trajetória até atingir 2 ou 4
  simular_caminho <- function() {
    estado <- 3  # começa em 3
    
    while (estado != 2 && estado != 4) {
      passo <- sample(c(-1, 1), size = 1, prob = c(p, 1-p))  # moeda honesta
      estado <- estado + passo
    }
    
    return(estado == 2)  # retorna TRUE se parou em 2, FALSE se em 4
  }
  
  # Simulando várias vezes
  set.seed(42)
  n_simulacoes <- 10000
  resultados <- replicate(n_simulacoes, simular_caminho())
  
  # Estimando a probabilidade de atingir 2 antes de 4
  probabilidade <- mean(resultados)
  cat("Probabilidade estimada de atingir $2 antes de $4:", probabilidade, "\n")
  
  
  
  
  
  