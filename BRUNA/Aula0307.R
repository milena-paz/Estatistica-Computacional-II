library(MASS)
area()

#simulção discreta

matriz <- matrix( c(1,1,0,0,1,1,0,1,1,0,0,0,0,1,0,0,0,1,0,0,1,0,0,0,1), nrow = 5, ncol = 5, byrow = FALSE)
matriz

matriz2<- matriz*matriz
matriz2

matriz3 <- matrix(c(6,3,2,2,5,3,1,2,2,2,0,0,0,1,0,0,0,1,0,0,5,2,1,1,4), nrow = 5, ncol = 5, byrow = FALSE)
matriz3

p1<- 0.99
p2<- p1
p3<- p1
p4<- p1
p5<- p1
c1 <- rbinom(n=1, size = 1, prob = p1)
c2 <- rbinom(n=1, size = 1, prob = p2)
c3 <- rbinom(n=1, size = 1, prob = p3)
c4 <- rbinom(n=1, size = 1, prob = p4)
c5 <- rbinom(n=1, size = 1, prob = p5)
matrizLUDR <-matrix(c(0,0,0,0,c1,0,c3,0,c4,c3,0,0,0,c2,c5,1), nrow = 4, ncol = 4, byrow = FALSE)
matrizLUDR
matrizLUDR <-matrix(c(0,0,0,0,c1,0,c3,0,c4,c3,0,0,0,0,0,1), nrow = 4, ncol = 4, byrow = FALSE)
conf <- c(0.99,0.99,0.99,0.99,0.99)
sapply(conf, function(x) rbinom(n=1, size=1, prob = x))
matrix(0, ncol = 4, nrow = 4)
matriz [1, 2:3] 
matriz[2, 3:4]
matriz[3, c(2,4)]
matriz[4,4]
passos <- matriz %*%matriz%*%matriz














