library(utf8)
library(stringr)
library(spacyr)
library(tensorflow)
library(keras)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

library(tm)

# --------------------------------------------------

# En primer lugar cargamos los libros
reinaRoja <- readLines("Recursos/8-ReinaRoja.txt", encoding = "UTF-8")
lobaNegra <- readLines("Recursos/9-LobaNegra.txt", encoding = "UTF-8")
reyBlanco <- readLines("Recursos/10-Rey-blanco.txt", encoding = "UTF-8")


# --------------------------------------------------
# Ahora vamos a seleccionar sólo el texto de los libros, eliminando prólogos:

# Reina Roja empieza con la frase: 
primeraLineaRR <- grep(pattern = "Antonia Scott sólo se permite pensar en el suicidio tres minutos al día.",
     reinaRoja,
     fixed = TRUE) 

# Loba Negra empieza con la frase: 
primeraLineaLN <- grep(pattern = "Antonia Scott nunca se ha enfrentado a una decisión tan difícil.",
     lobaNegra,
     fixed = TRUE) 

# Rey Blanco empieza con la frase: 
primeraLineaRB <- grep(pattern = "Antonia Scott no tiene ni siquiera tres minutos",
     reyBlanco,
     fixed = TRUE) 

# --------------------------------------------------
#Eliminamos todo lo que sobra del inicio
reinaRoja <- reinaRoja[-c(1:(primeraLineaRR[1]-1))]
lobaNegra <- lobaNegra[-c(1:(primeraLineaLN)-1)]
reyBlanco <- reyBlanco[-c(1:(primeraLineaRB)-1)]

# --------------------------------------------------
# Comprobamos que no existan caracteres no reconocibles
reinaRoja[!utf8_valid(reinaRoja)]
lobaNegra[!utf8_valid(lobaNegra)]
reyBlanco[!utf8_valid(reyBlanco)]

# --------------------------------------------------
#Eliminamos las líneas que están vacías (respresentadas por ""), se puede hacer también con gsub, pero esta es otra forma
vaciosRR <- which(reinaRoja=="")
reinaRoja <- reinaRoja[-c(vaciosRR)]

vaciosLN <- which(lobaNegra=="")
lobaNegra <- lobaNegra[-c(vaciosLN)]

vaciosRB <- which(reyBlanco=="")
reyBlanco <- reyBlanco[-c(vaciosRB)]

# --------------------------------------------------
# Eliminamos caracteres de formato que no interesan
reinaRoja <- gsub("[\t]{1,}", "", reinaRoja)
lobaNegra <- gsub("[\t]{1,}", "", lobaNegra)
reyBlanco <- gsub("[\t]{1,}", "", reyBlanco)

# --------------------------------------------------
# Substituimos varios espacios por uno sólo
reinaRoja <- gsub("[ ]{2,}", "", reinaRoja)
lobaNegra <- gsub("[ ]{2,}", "", lobaNegra)
reyBlanco <- gsub("[ ]{2,}", "", reyBlanco)

# --------------------------------------------------
#Se utilizará la librería Spacyr para realizar algúnos análisis en los libros
 spacy_install() # Ejecutar la primera vez
# spacy_download_langmodel("es")
spacy_initialize(model="es_core_news_sm")


# --------------------------------------------------
# Se contabilizan el número de frases de cada libro 
frasesRR <- spacy_tokenize(reinaRoja, what = "sentence")
v_frasesRR <- unlist(frasesRR)
sum(v_frasesRR=="") # Hay dos frases vacías
v_frasesRR <- v_frasesRR[-which(v_frasesRR=="")]
numFrasesRR <- length(v_frasesRR) # 10765 frases

frasesLN <- spacy_tokenize(lobaNegra, what = "sentence")
v_frasesLN <- unlist(frasesLN)
sum(v_frasesLN=="") # Hay una frase vacía
v_frasesLN <- v_frasesLN[-which(v_frasesLN=="")]
numFrasesLN <- length(v_frasesLN) # 10949 frases

frasesRB <- spacy_tokenize(reyBlanco, what = "sentence")
v_frasesRB <- unlist(frasesRB)
sum(v_frasesRB=="") # Hay 21 frases vacías
v_frasesRB <- v_frasesRB[-which(v_frasesRB=="")]
numFrasesRB <- length(v_frasesRB) # 9564 frases


# --------------------------------------------------
# Se imprime un histograma de las frases de cada libro
hist(nchar(v_frasesRR),
     main = "Histograma del tamaño de frases de Reina Roja",
     xlab = "Tamaño de la frase (número de caracteres)", ylab = "Ocurrencias"
)
hist(nchar(v_frasesLN),
     main = "Histograma del tamaño de frases de Loba Negra",
     xlab = "Tamaño de la frase (número de caracteres)", ylab = "Ocurrencias"
)
hist(nchar(v_frasesRB),
     main = "Histograma del tamaño de frases de Rey Blanco",
     xlab = "Tamaño de la frase (número de caracteres)", ylab = "Ocurrencias"
)


# --------------------------------------------------
# Ahora nos vamos al número de palabras
tokensRR <- spacy_tokenize(reinaRoja)
v_tokensRR <- unlist(tokensRR)
length(v_tokensRR)  # 130540 palabras
length(unique(v_tokensRR)) #13901 palabras diferentes 
head(sort(table(v_tokensRR), decreasing = TRUE), n = 25)  # Antonia 713, Jon no aparece


tokensLN <- spacy_tokenize(lobaNegra)
v_tokensLN <- unlist(tokensLN)
length(v_tokensLN)  # 121991 palabras
length(unique(v_tokensLN))  # 14023 palabras diferentes
head(sort(table(v_tokensLN), decreasing = TRUE), n = 25)  # Antonia 683, Jon 640


tokensRB <- spacy_tokenize(reyBlanco)
v_tokensRB <- unlist(tokensRB)
length(v_tokensRB)  # 118831 palabras
length(unique(v_tokensRB))  # 13127 palabras diferentes
head(sort(table(v_tokensRB), decreasing = TRUE), n = 25)  # Antonia 894, Jon 660

# --------------------------------------------------
# Cuando se termina con spacy se finaliza correctamente
spacy_finalize()



# Se realiza una limpieza severa y se hace el mapa de palabras


# Reina Roja
reinaRoja <- gsub("—", "", reinaRoja) # Eliminamos los guiones
corpusRR <- iconv(reinaRoja)
corpusRR <- Corpus(VectorSource(corpusRR))
corpusRR <- tm_map(corpusRR, tolower) # Se pasa todo el texto a minúsculas
inspect(corpusRR[1:5])

corpusRR <- tm_map(corpusRR, removePunctuation) # Quitamos los puntos y comas
inspect(corpusRR[1:5])

corpusRR <- tm_map(corpusRR, removeNumbers) # Se eliminan los números
inspect(corpusRR[1:5])

cleansetRR <- tm_map(corpusRR, removeWords, stopwords('spanish')) # Se eliminan palabras como Que, De, La, Al...
inspect(cleansetRR[1:5])

library(wordcloud)
tdmRR <- TermDocumentMatrix(cleansetRR)
tdmRR <- as.matrix(tdmRR)
wRR <- sort(rowSums(tdmRR), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(wRR),
          freq = wRR,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

# Loba Negra
lobaNegra <- gsub("—", "", lobaNegra) # Eliminamos los guiones
corpusLN <- iconv(lobaNegra)
corpusLN <- Corpus(VectorSource(corpusLN))
corpusLN <- tm_map(corpusLN, tolower) # Se pasa todo el texto a minúsculas
corpusLN <- tm_map(corpusLN, removePunctuation) # Quitamos los puntos y comas
corpusLN <- tm_map(corpusLN, removeNumbers) # Se eliminan los números
cleansetLN <- tm_map(corpusLN, removeWords, stopwords('spanish')) # Se eliminan palabras como Que, De, La, Al...

tdmLN <- TermDocumentMatrix(cleansetLN)
tdmLN <- as.matrix(tdmLN)
wLN <- sort(rowSums(tdmLN), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(wLN),
          freq = wLN,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

# Rey Blanco
reyBlanco <- gsub("—", "", reyBlanco) # Eliminamos los guiones
corpusRB <- iconv(reyBlanco)
corpusRB <- Corpus(VectorSource(corpusRB))
corpusRB <- tm_map(corpusRB, tolower) # Se pasa todo el texto a minúsculas
corpusRB <- tm_map(corpusRB, removePunctuation) # Quitamos los puntos y comas
corpusRB <- tm_map(corpusRB, removeNumbers) # Se eliminan los números
cleansetRB <- tm_map(corpusRB, removeWords, stopwords('spanish')) # Se eliminan palabras como Que, De, La, Al...
inspect(cleansetRB[1:5])

tdmRB<- TermDocumentMatrix(cleansetRB)
tdmRB <- as.matrix(tdmRB)
wRB <- sort(rowSums(tdmRB), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(wRB),
          freq = wRB,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)


# --------------------------------------------------
# Analizando los sentimientos de las frases
s <- get_nrc_sentiment(reinaRoja)

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Número',
        main = 'Sentimientos de frases en Reina Roja')


s2 <- get_nrc_sentiment(lobaNegra)

barplot(colSums(s2),
        las = 2,
        col = rainbow(10),
        ylab = 'Número',
        main = 'Sentimientos de frases en Loba Negra')


s3 <- get_nrc_sentiment(reyBlanco)

barplot(colSums(s3),
        las = 2,
        col = rainbow(10),
        ylab = 'Número',
        main = 'Sentimientos de frases en Rey Blanco')
