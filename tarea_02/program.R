# Nos ubicamos en el directorio donde se encuentran los archivos ...
setwd("/home/randy/Escritorio/Semestre/patrones/tareas/tarea_02")
getwd()

# Incluimos paquetes necesarios ...
library("tm")

#Pon todos los textos como archivos separados en "midir"
mistextos <- Corpus( DirSource ("data") )
names(mistextos)
length(mistextos)

matriz <- DocumentTermMatrix( 
    mistextos, 
    
    control=list( 
        minWordLength=2, minDocFreq=5, stemming = TRUE,
        removePunctuation = TRUE, removeNumbers = TRUE, 
        stemming = TRUE
    )
)

#Para sacar las palabras que ocurren mas de 1000 veces:
findFreqTerms(matriz, lowfreq=1000)
# Cantidad de palabbras que tienen frecuencia mayor a 1000
length(findFreqTerms(matriz, lowfreq=1000))

bows <- as.matrix(matriz[, findFreqTerms(matriz, lowfreq=1000)])
data <- data.frame(bows)
nrow(data)
ncol(data)

############################################################################
############################################################################

# Hacemos PCA con los datos normalizados y centrados
pca = prcomp(data , center = TRUE, scale = TRUE)
summary(pca)

xlim <- range(pca$x[,1])
plot(pca$x[,1:2], xlim = xlim, ylim = xlim)

# Ambos ...
points(
    pca$x[c("The-Royal-Book-of-Oz_Baum_Thompson_1.txt"),1], 
    pca$x[c("The-Royal-Book-of-Oz_Baum_Thompson_1.txt"),2], 
    col="green"
)

# Baum
points(
    pca$x[c("The-Wonderful-Wizard-of-Oz_Baum_5.txt"),1], 
    pca$x[c("The-Wonderful-Wizard-of-Oz_Baum_5.txt"),2], 
    col="red"
)

# Thompson
points(
    pca$x[c("The-Wishing-Horse-of-Oz_Thompson_5.txt"),1], 
    pca$x[c("The-Wishing-Horse-of-Oz_Thompson_5.txt"),2], 
    col="blue"
)
