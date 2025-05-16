# Repositório da turma A de EST066 (2025.1)
## Para os contribuidores:
É recomendado **criar uma pasta só para vocês** (e com nome) para auxiliar na organização do repositório.
Nela, vocês podem compartilhar suas notas de aula, trabalhos e exercícios.
Se quiserem, podem usar o repositório para outras disciplinas, desde que esteja devidamente organizado.
Aproveitem! :)
## Box muller
dado que: 

$$
\Huge U_1,U_2 \sim Unif(0,1)
$$

e que:

$$
\Huge Z_1 = \sqrt{-2ln(U_1)} cos(2\pi U_2),Z_2 = \sqrt{-2ln(U_1)} sen(2\pi U_2)
$$

entao: $\large Z_1$ e $\large Z_2$ vao seguir uma normal bivariada

## Gerador 
O gerador que está sendo usado é esse:

$$
\Huge x_{n+1} = (Ax_n+B)\quad mod \quad p
$$

(mod é o operador modular, resto da divisão, o "%%")

Para fazer o qualquer termo virar uma Uniforme(0,1) é só dividir por p.

E em sala, eles foram classificados com esses valores:

- Gerador 1
  - A= 65539 , B=0 , $x_0$ =2 , p= $2^{31}$
- Gerador 2
  - A=1664525 , B=1013904223 , $x_0$ = 2 , p= $2^{32}$
- Gerador 3
  - runif() 
