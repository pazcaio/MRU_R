#Vetor de Espaço
S = c(0.1, 0.2, 0.3, 0.4)

#Criando vetores // 5º
Ponto_1 = c(4.650555, 4.31058, 4.63686)
Ponto_2 = c(9.28645, 8.59093, 8.93680)
Ponto_3 = c(13.70052, 12.79661, 13.19727)
Ponto_4 = c(16.46835, 16.98921, 17.64644)


pontos1 = data.frame(cbind(Ponto_1, Ponto_2, Ponto_3, Ponto_4))
View(pontos1)

#Criando vetores // 10º
Ponto_5 = c(2.08080, 2.70174, 2.13267)
Ponto_6 = c(4.27449, 4.83587, 4.32795)
Ponto_7 = c(6.40377, 6.94123, 6.30745)
Ponto_8 = c(8.47190, 9.09295, 8.48273)
pontos2 = data.frame(cbind(Ponto_5, Ponto_6, Ponto_7, Ponto_8))
View(pontos2)

#Criando veotores // 15º
Ponto_9 = c(1.36078, 1.69315, 1.40947)
Ponto_10 = c(3.00525, 3.19411, 2.99530)
Ponto_11 = c(4.47142, 4.76817, 4.44318)
Ponto_12 = c(5.92063, 6.24295, 6.12256)
pontos3 = data.frame(cbind(Ponto_9, Ponto_10, Ponto_11, Ponto_12))
View(pontos3)

#Dispersão dos dados através de boxplots
boxplot(pontos1, col = as.factor(pontos1))
boxplot(pontos2, col = as.factor(pontos1))
boxplot(pontos3, col = as.factor(pontos1))

#Medias das lista em dataframes
media1 = data.frame(Media = (apply(pontos1, 2, mean)))
View(media1)

media2 = data.frame(Media = (apply(pontos2, 2, mean)))
View(media2)

media3 = data.frame(Media = (apply(pontos3, 2, mean)))
View(media3)

#Criando data.frame do Espaço e Tempo

mediadf = data.frame(media1, media2, media3)
colnames(mediadf) = c('Media_5_g', 'Media_10_g', 'Media_15_g')
View(mediadf)

S_T = data.frame(cbind(S, mediadf$Media_5_g, mediadf$Media_10_g, mediadf$Media_15_g))
S_T
colnames(S_T) = c('S', 'Media_5_g', 'Media_10_g', 'Media_15_g')
S_T

#Grafico Espaço Tempo.

camada1 = geom_line(data = S_T, 
                    aes(x = Media_5_g, y = S),
                    col = 'red')
camada2 = geom_line(3, 3, data = S_T, 
                    aes(x = Media_10_g, y = S),
                    col = 'blue')
camada3 = geom_line(3, 3, data = S_T,
                    aes(x = Media_15_g, y = S),
                    col = 'green')
ggplot() + camada1 + camada2 + camada3 + camada4 +
  xlab('Tempo')+
  labs(c('S', 'Media_5_g', 'Media_10_g', 'Media_15_g'))+
  theme(legend.position = 'top')

#Calculando a velocidade média (função Horária):

for(i in 1: 4){
  calc1 = S_T$S[1] / S_T[1,]
}
for(i in 1: 4){
  calc2 = S_T$S[2] / S_T[2,]
}
for(i in 1: 4){
  calc3 = S_T$S[3] / S_T[3,]
}
for(i in 1: 4){
  calc4 = S_T$S[4] / S_T[4,]
}
vm = data.frame(rbind(calc1, calc2, calc3, calc4))
View(vm)
colnames(vm) = c('ID', 'Veocidade_Med_5', 'Velocidade_Med_10', 'Velocidade_Med_15')
vm$ID = NULL
#Gráfico da Função Horária

vm = data.frame(cbind(S_T$S, vm))


camada5 = geom_line(data = vm,
                    aes(x = vm$S_T.S, y = vm$Veocidade_Med_5),
                    col = 'blue')
camada6 = geom_line(data = vm,
                    aes(x = vm$S_T.S, y = vm$Velocidade_Med_10),
                    col = 'red')
camada7 = geom_line(data = vm,
                    aes(x = vm$S_T.S, y = vm$Velocidade_Med_15),
                    col = 'green')
ggplot() + camada5 + camada6 + camada7 + xlab('Tempo') + ylab('Velocidade_Media')
