dUs <- function(x) 12*(x-0.5)**2
pUs <- function(x) 4*(x-0.5)**3+0.5
##qU1 <- function (p-0.5)**(1/3)+0.5
qUs <- function(p) (p-0.5)**(1/3) + 0.5
dUs(c(0,0.5,1))
pUs (c(0,0.5,1))
qUs (c(0,0.5,1))

(as.complex(0.5))**1/3

qUs <- function(p) sign(p-0.5)* abs(p-0.5)**(1/3)+0.5
qUs (c(0, 0.5, 1))

polyroot(c(-1/2,0,0,1))

u <- runif(1e4)
Xus <- qUs(u)

hist (Xus, freq = F, breaks = 50, ylim = c(0,3))
t.test(x=Xus, y=NULL, mu=0.5)

mean (Xus>=0.5)
qUs (c(0.25,0.5,0.75))

mean(Xus<0.103)
quantidade  (Xus, c(0.5))

sum(Xus>=)





