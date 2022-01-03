# 0. Nos ubicamos en el directorio donde se encuentran los archivos ...
setwd("/home/randy/Escritorio/Semestre/patrones/tareas/tarea_03")
getwd()

############################################################################
################## Lectura de datos y preprocesamiento #####################
############################################################################

X <- read.table("deport.dat")
# Guardamos los paises y los removemos del dataframe
country <- X[,9]
X <- X[,1:8]
# A cada fila le asignamos como nombre su pais correspondiente
rownames(X) <- country
# Graficamos ... Observamos dependencia lineal en cada pareja de varables
plot(X)

head(X)
##########################################################################
##########################################################################

# Calculamos las distancias entre cada pareja de puntos
D <- dist(X, method = "euclid")
D <- as.matrix(D)

##########################################################################
##########################################################################

# Generamos matriz para centrar
# Generamos matriz identidad y un vector de unos
identity = diag(n+n)
ones = rep(1, n+n)
# Calculamos matriz para centrar
C = identity - (1 / (n+n)) * (ones %*% t(ones))

# Centramos los datos
Xc = C %*% X

##########################################################################
##########################################################################

# Hacemos PCA con los datos normalizados y centrados
pca = prcomp(X , center = TRUE, scale = TRUE)
class(pca$x[1:2])
xlim <- range(pca$x[,1])
plot(pca$x[,1:2], xlim = xlim, ylim = xlim)
text(pca$x[,1:2], labels=rownames(X),data=pca$x[,1:2], cex=0.9, font=2)

#################################################################################
#################################################################################
#################################################################################
library("kohonen")

set.seed(123)
s <- som(scale(X), grid = somgrid(5, 5, "hexagonal"))
plot(s, main="SOM")
plot(s,type="count") #  poner en cada celda número de obs.
plot(s,type="mapping",label=substring(row.names(X),1,3))
plot(s,type="property",property=X[,1]) # graficar valores de una variables (puede ser que una celda con tiene observaciones)

##########################################################################
##########################################################################
##########################################################################
library(tsne)
# Siempre se uso esta semilla
set.seed(123)
tsne_reduction <- tsne(X, k = 2, perplexity = 15, epoch = 200)
plot(tsne_reduction, main="T-SNE")
text(tsne_reduction, labels=rownames(X),data=tsne_reduction, cex=0.9, font=2, main="T-SNE")

##########################################################################
##########################################################################
##########################################################################
library(vegan)

names(isomap_reduction$points)
Dv <- vegdist(X)
isomap_reduction <- isomap(Dv, k=6)
plot(isomap_reduction, main="ISOMAP")

#dis <- vegdist(X)
#tr <- spantree(dis)
#pl <- ordiplot(cmdscale(dis), main="CMDSCALE")
#lines(tr, pl, col="red")
#plot(isomap( dis, k=4), main="ISOMAP")

##########################################################################
##########################################################################
##########################################################################

library("lle")
#set.seed(123)
lle_reduction <- lle( X=X, m=2, k=40)# k=10, 20, 30
#str(lle_reduction)
plot(as.matrix(lle_reduction$Y), main="LLE", xlab=expression(y[1]), ylab=expression(y[2]) )
text(as.matrix(lle_reduction$Y), labels=rownames(X),data=as.matrix(lle_reduction$Y), cex=0.9, font=2)