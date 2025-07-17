library("sound")
#Meu windows teve problema com usar o comando play(), minha alternativa foi
#salvar o som com saveSample() e ouvir ele no pc.

#O pacote sound tem esses 4 tipos de ondas para usar:
plot.Sample(Sine(2,3))
plot.Sample(Square(2,3),lwd=2)
plot.Sample(Sawtooth(2,3),lwd=2)

#Noise significa exatamente o que parece, pode lagar um pouco.
plot.Sample(Noise(0.5),lwd=2)

#Elas podem ser somadas, subtraidas e multiplicadas:
plot.Sample(Sawtooth(2,3) + Sine(2,3),lwd=2)

#Note que nesse caso as ondas ficam estritamente no negativo.
plot.Sample(Sawtooth(2,3)*Sine(2,3),lwd=2)

#para resolver isso temos center:
plot.Sample(center(Sawtooth(2,3)*Sine(2,3)),lwd=2)

#Caso o som fique baixo temos o comando normalize para reajustar a amplitude:
plot.Sample(normalize(center(Sawtooth(2,3)*Sine(2,3)),1),lwd=2)

#As informacoes sobre a amplitude do som ficam guardadas em uma matriz em: sample$sound
Sine(2,3)$sound[,1:15]
#Caso seu audio for mono, aquilo vira uma matriz 1xn, caso ele seja stereo 2xn
Sine(3,4)
dim(Sine(3,4)$sound)
Sine(3,4,channels=2)
dim(Sine(3,4,channels=2)$sound)

#Sabendo que as amplitudes estao em uma matriz eu posso aplicar 
#funcoes vetorizadas nelas, note que assim nao funciona:
pmax(0.3,Sine(2,3))

#As unicas operacoes que funcionam assim "de um jeito seco" sao soma e multiplicacao.

#Mas assim funciona:
exemplo <- Sine(2,3)
plot.Sample(exemplo,lwd=2)
exemplo$sound[1,] <- pmax(0.3,exemplo$sound) 
#Temos que mandar para a dimensao 1 ja que "exemplo" tem tipo mono e nao stereo.
plot.Sample(exemplo,lwd=2)

#Existe a funcao stereo para criar audios com valores diferentes em cada canal.
exemplo2 <- stereo(Sine(3,4),Sine(12,4))
plot.Sample(exemplo2)

#E com essas coisas eu tive a ideia de fazer uma funcao que oscila qualquer audio entre
#esquerda e direita no fone de ouvido.

#Usando loadSample() para carregar meu audio de teste
gatorra <- normalize(center(loadSample("gatorra cortada.wav")),1)

#audio grande, pode lagar.
plot.Sample(gatorra)

#A ideia por tras desse sistema de oscilar o audio seria eu multiplicar meu audio
#por um seno no canal da direita e por um seno negativo no canal da esquerda.
#Isso vai forcar ele a oscilar entre 100% de volume e "-100%" de volume.

direita <- Sine(3,4)
esquerda <- -Sine(3,4)
plot.Sample(stereo(esquerda,direita))

#Como "-100%" de volume nao faz sentido algum, eu vou usar uma funcao que limita
#o audio para ficar acima de 0.

esquerda$sound[1,] <- pmax(0,esquerda$sound[1,])
direita$sound[1,] <- pmax(0,direita$sound[1,])

plot.Sample(stereo(esquerda,direita))
#lembrando que eu vou multiplicar meu audio por essas funcoes, eu estou usando
#o plot.Sample para mostrar como elas ficam nos dois canais, mas meu audio final
#nao vai ficar assim.

oscila <- function(som,quantidade=1,minimo=0.1)
{
  tamanho <- length(som$sound) - 1
  
  som.direita <- som*pmax(minimo,sin(quantidade*2*pi*c(0:tamanho)/tamanho))
  som.esquerda <- som*pmax(minimo,-sin(quantidade*2*pi*c(0:tamanho)/tamanho))
  
  return(stereo(som.esquerda,som.direita))
}

#Eu adicionei um minimo, para o audio nao ficar completamente mudo quando for para o outro lado
oscilando <- normalize(oscila(gatorra,2,0.1),1)
plot.Sample(oscilando)

#SaveSample() faz exatamente o que parece que ele faz.
saveSample(oscilando,"gatorra oscila.wav",overwrite=T)

#Agora pretendo montar um acorde, as notas do acorde eu tirei da internet,
#e as frequencias das notas tambem.
notas <- list(a=Sine(147,8),b=Sine(220,6),c=Sine(294,4))
silencio <- list(b=Silence(2),c=Silence(4))

#A frequencia por ser 220 deixa impossivel de ver.
plot.Sample(notas$b)
notas$b <- appendSample(silencio$b,notas$b)

#Mas depois de concatenar um audio mudo ela fica assim:
plot.Sample(notas$b)

notas$c <- appendSample(silencio$c,notas$c)
#NUNCA ESQUECA DE USAR O NORMALISE DEPOIS DE SOMAR, O AUDIO FICA MUITO BARULHENTO
#COM AMPLITUDES MAIORES QUE 1.
acorde <- normalize(sum(notas$a,notas$b,notas$c),1)

saveSample(acorde,"acorde simples.wav",overwrite=T)
