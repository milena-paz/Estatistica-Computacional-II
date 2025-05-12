gen <- function(x.0 = 2, A = 103, B = 17,m = 10, inter = 5){
  
  u.0 <- x.0/m
  x.vet <- numeric(inter)
  u.vet <- numeric(inter)
  x = x.0
  x.vet[1] <- x.0
  u.vet[1] <- u.0
  
  for(i in 1:inter){
    xn <- (A*x + B)%%m
    un <- xn/m
    
    x.vet[i] <- xn # arrumar outro jeito
    u.vet[i] <- un # arrumar outro jeito
    
    x = xn
    
  }
  return(list("u" = u.vet,
              "x" = x.vet))
}
set.seed(666)

g1 <- gen(x.0 = 2, A =65539, B = 0, m = 2E31, inter = 1E4)

g2 <- gen(x.0 = 2, A =1664525, B = 1013904223, m = 2E32, inter = 1E4)

g3 <- runif(1E4)



set.seed(666)
vetor <- runif(2E4)

plot_grid <- function(vetor){
  if (length(vetor) %% 2 != 0) stop("O vetor deve ter comprimento par.")
  
  n <- length(vetor) / 2 
  impar <- (1:n)*2 - 1
  par   <- (1:n)*2
  
  X <- vetor[impar]
  Y <- vetor[par]
  
  cutX <- cut(x = X, breaks = (0:10)/10, include.lowest = TRUE)
  cutY <- cut(x = Y, breaks = (0:10)/10, include.lowest = TRUE)
  
  tabela <- table(cutX, cutY)
  esperado <- n / 100
  max_dist <- max(abs(tabela - esperado))
  
  #divide as cores de acordo com a média.
  get_cor <- function(freq, max_dist){
    if(freq == esperado){
      return("white")
    } else if(freq < esperado){
      prop <- (esperado - freq) / max_dist
      rgb <- grDevices::colorRamp(c("white", "#E69F00"))(prop)
    } else {
      prop <- (freq - esperado) / max_dist
      rgb <- grDevices::colorRamp(c("white", "#0072B2"))(prop)
    }
    grDevices::rgb(rgb[1]/255, rgb[2]/255, rgb[3]/255)
  }
  

  plot(X, Y, pch = ".", col = "white", xlab = "X", ylab = "Y", xlim = c(0,1), ylim = c(0,1))
  abline(h = (0:10)/10, v = (0:10)/10, col = "gray20")
  axis(1, at = (0:10)/10)
  axis(2, at = (0:10)/10)
  box()
  
  for (i in 0:9){
    for (j in 0:9){
      freq <- tabela[i + 1, j + 1]
      cor <- get_cor(freq, max_dist)
      
      rect(xleft = i/10, ybottom = j/10, xright = (i+1)/10, ytop = (j+1)/10,
           col = cor, border = "gray40")
      
      text(x = i/10 + 0.05, y = j/10 + 0.05, labels = freq, cex = 0.85, font = 2)
    }
  }
}
plot_grid(vetor)
plot_grid(g3)
