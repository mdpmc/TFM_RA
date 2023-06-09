#Carga de los paquetes
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(NLP)
library(tm)

#Carga de los textos los cuentos como una cadena de caracteres-Primera Etapa

primeraetapa_cadena<-scan("C:/Users/mdpma/Desktop/tfm/primeraetapa.txt", what="character")

#División de la cadena de caracteres en un listado de palabras o unigramas (tokens)-Primera Etapa

primeraetapa_palabras<-get_tokens(primeraetapa_cadena)
print(primeraetapa_palabras)

#Ver cuantos tokens hay en el texto-Primera Etapa

length(primeraetapa_palabras)

#Obtener sentimientos-Primera Etapa

sentimientos_primeraetapa<-get_nrc_sentiment(primeraetapa_palabras, lang="spanish")
head(sentimientos_primeraetapa)

#Obtener el resumen de los valores obtenidos-Primera Etapa

summary(sentimientos_primeraetapa)

#Crear gráfico de barras-Primer Periodo

barplot(
  colSums(prop.table(sentimientos_primeraetapa[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Cuentos de Rosario de Acuña hasta 1883",
  sub = "Análisis realizado por Pilar Marco",
  xlab="emociones", ylab = NULL)

#Hacer un recuento de palabras por emoción-Primera Etapa

#Tristeza

palabras_tristeza<-primeraetapa_palabras[sentimientos_primeraetapa$sadness> 0]
head(palabras_tristeza)

palabras_tristeza_orden<-sort(table(unlist(palabras_tristeza)), decreasing = TRUE)
head(palabras_tristeza_orden, n = 12)

length(palabras_tristeza_orden)

#Confianza

palabras_confianza<-primeraetapa_palabras[sentimientos_primeraetapa$trust> 0]
head(palabras_confianza)

palabras_confianza_orden<-sort(table(unlist(palabras_confianza)), decreasing = TRUE)
head(palabras_confianza_orden, n = 12)

length(palabras_confianza_orden)

#Visualizar la evolución de los sentimientos en el texto

sentimientos_primeraetapa_valencia<-(sentimientos_primeraetapa$negative *-1)+sentimientos_primeraetapa$positive

simple_plot(sentimientos_primeraetapa_valencia)


