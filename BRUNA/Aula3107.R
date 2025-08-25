efe <- function(x,y) 0.6*exp(-((x-5)**2+(y-2)**2)/2)
x <- seq(-2, 2, by = 0.15)
y <- seq(-1, 3, by = 0.15)
z <- outer(x, y, efe)

# gráfico da superfície
persp(x, y, z, phi = 45, theta = 45, col = "yellow", shade = 0.00000001, ticktype = "detailed")

# Instale a versão estável do CRAN com dependências
install.packages("rgl", dependencies = TRUE)

# Se falhar, tente instalar manualmente as dependências
install.packages(c("magrittr", "htmltools", "knitr", "webshot"))

contour(x,y,z, lwd = 2, nlevels = 10)
rgl::persp3d(efe, xlim= c(0,8), ylim=c(0,8))


