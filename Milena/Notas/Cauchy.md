
# Prova de que $X_1/X_2$ é Cauchy

Sejam $X_1$ e $X_2$ variáveis aleatórias normais padrão independentes. A sua conjunta, então, é dada por:

$$
f_{X_1,X_2}(x_1,x_2)=
\frac {\exp \{-(x_1^2+x_2^2)/2\}}{2 \pi}  \text I_{\mathbb{R}}(x_1) I_{\mathbb{R}}(x_2)
$$

Para realizarmos a transformação $Y=X_1/X_2$, escolhemos uma auxiliar $T= X_2$. Dessa forma,
temos o coeficiente Jacobiano:

$$
\begin{aligned}
& J(x_1,x_2) = \begin{vmatrix}
      x_2^{-1} & x_1x_2^{-2} \\
      0 & 1
    \end{vmatrix}
    =x_2^{-1}
\\
& J(y,t)=t^{-1}
\end{aligned}
$$

Assim, temos que a conjunta de Y e T é dada por:

$$
\begin{aligned}
  g_{Y,T}(y,t) & =f_{X_1,X_2}(yt,t)|J|^{-1} I_{\mathbb{R}}(y)I_{\mathbb{R}}(t)=
  \frac {\exp \{-(y^2t^2+t^2)/2\}}{2 \pi}  I_{\mathbb{R}}(y) I_{\mathbb{R}}(t)
  \\
  & =\frac {\exp \{-t^2(y^2+1)/2\}}{2 \pi}  I_{\mathbb{R}}(y) I_{\mathbb{R}}(t)
\end{aligned}
$$

e, portanto, a marginal de Y é obtida da seguinte forma:

$$
\begin{aligned}
  g_Y(y) & =\int_{-\infty}^{+\infty}
  \frac {|t|\exp \{-t^2(y^2+1)/2\}}{2 \pi}  I_{\mathbb{R}}(y) dt =
  \frac{I_{\mathbb{R}}(y)}{2\pi}\int_{-\infty}^{+\infty}
  \exp \{-t^2(y^2+1)/2\}|t|  dt
  \\
  & =\frac{I_{\mathbb{R}}(y)}{2\pi(y^2+1)}\int_{-\infty}^{+\infty}
  \exp \{-t^2(y^2+1)/2\}(y^2+1)|t|  dt
\end{aligned}
$$

 , onde o integrando é uma função par, portanto:

$$
\begin{aligned}
  g_Y(y) & = \frac{I_{\mathbb{R}}(y)}{2\pi(y^2+1)}2\int_{0}^{+\infty}
  \exp \{-t^2(y^2+1)/2\}(y^2+1)|t|  dt
\end{aligned}
$$

Fazendo simples substituição com $u=t^2(y^2+1)/2$ e $du=t(y^2+1)dt$, temos:

$$
  g_Y(y) = \frac{I_{\mathbb{R}}(y)}{\pi(y^2+1)}\int_{0}^{+\infty} \exp \{-u\}du=
  \frac{I_{\mathbb{R}}(y)}{\pi(y^2+1)}( -e^{-u} |^{ +\infty }_{0} )
$$

$$
=\frac{I_{\mathbb{R}}(y)}{\pi(y^2+1)}(0 +1) = \frac{1}{\pi(y^2+1)}I_{\mathbb{R}}(y)
$$

Que é a densidade de uma **Cauchy(0,1)**, portanto $Y \sim \text{Cauchy}(0,1)$
