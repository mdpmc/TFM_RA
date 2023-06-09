#Carga de los paquetes
library(syuzhet)
library(RColorBrewer)
library(wordcloud)
library(NLP)
library(tm)

#Carga de los textos los cuentos como una cadena de caracteres-Segunda Etapa

segundaetapa_cadena<-scan("C:/Users/mdpma/Desktop/tfm/segundaetapa.txt", what="character")

#División de la cadena de caracteres en un listado de palabras o unigramas (tokens)-Segunda Etapa

segundaetapa_palabras<-get_tokens(segundaetapa_cadena)
print(segundaetapa_palabras)

#Ver cuantos tokens hay en el texto-Segunta Etapa

length(segundaetapa_palabras)

#Obtener sentimientos-Segunda Etapa

sentimientos_segundaetapa<-get_nrc_sentiment(segundaetapa_palabras, lang="spanish")
head(sentimientos_segundaetapa)

#Obtener el resumen de los valores obtenidos-Segunda Etapa

summary(sentimientos_segundaetapa)

#Crear gráfico de barras-Segundo Periodo

barplot(
  colSums(prop.table(sentimientos_segundaetapa[, 1:8])),
  space = 0.2,
  horiz = FALSE,
  las = 1,
  cex.names = 0.7,
  col = brewer.pal(n = 8, name = "Set3"),
  main = "Cuentos de Rosario de Acuña desde 1884",
  sub = "Análisis realizado por Pilar Marco",
  xlab="emociones", ylab = NULL)


#Alegría

palabras_alegria_segundaetapa<-segundaetapa_palabras[sentimientos_segundaetapa$joy> 0]
head(palabras_alegria_segundaetapa)

palabras_alegria_orden_segundaetapa<-sort(table(unlist(palabras_alegria_segundaetapa)), decreasing = TRUE)
head(palabras_alegria_orden_segundaetapa, n = 12)

length(palabras_alegria_orden_segundaetapa)

#Confianza

palabras_confianza_segundaetapa<-segundaetapa_palabras[sentimientos_segundaetapa$trust> 0]
head(palabras_confianza_segundaetapa)

palabras_confianza_orden_segundaetapa<-sort(table(unlist(palabras_confianza_segundaetapa)), decreasing = TRUE)
head(palabras_confianza_orden_segundaetapa, n = 12)

length(palabras_confianza_orden_segundaetapa)

#Visualizar la evolución de los sentimientos en el texto

sentimientos_segundaetapa_valencia<-(sentimientos_segundaetapa$negative *-1)+sentimientos_segundaetapa$positive

simple_plot(sentimientos_segundaetapa_valencia)







