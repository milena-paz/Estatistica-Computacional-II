# Repositório da turma A de EST066 (2025.1)
## Para os contribuidores:
É recomendado **criar uma pasta só para vocês** (e com nome) para auxiliar na organização do repositório.
Nela, vocês podem compartilhar suas notas de aula, trabalhos e exercícios.
Se quiserem, podem usar o repositório para outras disciplinas, desde que esteja devidamente organizado.
Aproveitem! :)

## Gerador 
A base de gerar numeros aleatorios consiste em gerar uma sequencia pseudoaleatoria, e depois transformar ela em uma Uniforme(0,1).
Para a sequencia pseudoaleatoria pode se usar esse algoritmo:

$$
\Huge x_{n+1} = (Ax_n+B)\mod p
$$

(mod é o operador modular, resto da divisão, o "%%")

- Gerador 1
  - A= 65539 , B=0 , $x_0$ =2 , p= $2^{31}$
- Gerador 2
  - A=1664525 , B=1013904223 , $x_0$ = 2 , p= $2^{32}$
- Gerador 3
  - runif() 

Para fazer o qualquer termo virar uma Uniforme(0,1) é só dividir por p.
Entao para gerar n valores "aleatorios" que seguem uma uniforme(0,1) e so colocar condicoes iniciais para A,B,P, $x_0$ e calcular todos os $x$ ate $x_n$

## Inversa

Caso voce tenha uma distribuicao $f_x(x)$ e consiga achar a funcao quantilica dela (a funcao inversa da acumulada), voce pode colocar
os valores da Uniforme(0,1) la e gerar numeros aleatorios da sua distribuicao $f_x(x)$, [video "provando" isso](https://youtu.be/Z9fODwmed6M?si=_ZirfDZpZn9JuwY-&t=1280)
(e pra assistir so entre 21:19 ate 22:03, mas tem que pausar MUITO no meio do caminho pra acompanhar).

$$
\Huge U \sim Unif(0,1), \space F^{-1}_{X}(U) \sim X
$$

Um exemplo: $\Huge X \sim exp(\lambda), \space F_X(x)=1-e^{-\lambda x}, \space F^{-1}_X(x)= -\frac{ln(1-x)}{\lambda}$.
Entao para gerar n valores aleatorios de uma $exp(\lambda)$ eu preciso gerar n valores de Uniformes(0,1) e colocar
eles um a um na funcao $F^{-1}_X(x)= -\frac{ln(1-x)}{\lambda}$

## Box muller

Crie dois valores de Uniforme(0,1):

$$
\Huge U_1,U_2 \sim Unif(0,1)
$$
 
O metodo do Box Muller diz que se as uniformes forem colocadas nessa formula, $\large Z_1$ e $\large Z_2$ vao seguir uma normal bivariada.

$$
\Huge Z_1 = \sqrt{-2\ln(U_1)} \cos(2\pi U_2) \text{,} \quad Z_2 = \sqrt{-2\ln(U_1)} \sin(2\pi U_2)
$$

## Aceitacao e rejeicao

Imagine que temos duas F.D.P. no mesmo dominio, voce quer gerar valores de $f_x(x)$ usando $g_y(y)$.
Primeiro voce comeca achando o valor de M que satisfaca a equacao 

$$
\Huge \frac{f_x(x)}{g_y(x)} \leq M, \space \forall x
$$

Mas queremos achar o valor mais eficiente de M, e para isso temos que maximizar aquela fracao, no site do lupercio tem um otimo
[exemplo](https://bessegato.github.io/disciplinas/EST066/04_geracao_rejeicao.html) dele fazendo isso.

Depois de achar o valor de M, geramos um valor aleatorio de $Y$ e um valor aleatorio de uma uniforme(0,1) $U$.
Colocamos o Valor aleatorio $Y$ na $f_x(x)$ e na $g_y(y)$ e entao vemos se isso respeita a seguinte desigualdade:

$$
\Huge U \leq \frac{f_X(Y)}{Mg_Y(Y)}
$$

Se ela for verdade "aceite" o ponto, se ela nao for "rejeite" ele, repita isso denovo e denovo, gera outro U e outro Y e veja se aceita ou rejeita,
no final os pontos aceitos vao seguir a distribuicao de $X$.
