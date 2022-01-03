# Nos ubicamos en el directorio donde se encuentran los archivos ...
setwd("/home/randy/Escritorio/Semestre/patrones/tareas/tarea_03")
getwd()

##########################################################################
##########################Generacion de datos#############################
##########################################################################

# 1. Definimos numero de observaciones a generar para cada distribucion 
n <- 30

# 2. Generamos datos.

# 2.1 Generamos un conjunto de angulos para un semicirculo
thetas <- seq(from = 0.0, to = pi, by = ((pi - 0.0)/(n-1)))
# 2.2 Generamos un semicirculo centrado en (0.0, 0.0)
x_1 <- sapply(thetas, cos)
y_1 <- sapply(thetas, sin)
# 2.3 Generamos un semicirculo centrado en (1.0, 0.5)
x_2 <- sapply(thetas, cos) + 1
y_2 <- -sapply(thetas, sin) + 0.5
# 2.4 Concatenamos las observaciones y transformamos a dataframe
X <- c(x_1, x_2, y_1, y_2)
dim(X) <- c(n+n, 2)
colnames(X) <- c("X_1", "X_2")
X <- as.data.frame(X)

# 2.5 Observamos los datos
plot(X)

##########################################################################
#############################       PCA       ############################
##########################################################################

# Hacemos PCA con los datos normalizados y centrados
pca = prcomp(X , center = TRUE, scale = TRUE)
# Observamos la varianza explicada en cada componente ... con las primeras dos ya tenemos 93%
summary(pca)
print(pca)

xlim <- range(pca$x[,1])
plot(pca$x[,1:2], xlim = xlim, ylim = xlim)
points(pca$x[1:n,], col="red")
points(pca$x[(n+1):(n+n),], col="blue")

##########################################################################
############################       KPCA      #############################
##########################################################################

# 3. Generamos matriz para centrar

# 3.1 Generamos matriz identidad y un vector de unos
identity = diag(n+n)
ones = rep(1, n+n)
# 3.2 Calculamos matriz para centrar
C = identity - (1 / (n+n)) * (ones %*% t(ones))

##########################################################################
##########################################################################

# 4. Calculo de Kernel

# 4.1 Calculamos las distancias entre cada pareja de puntos
D <- dist(X, method = "euclid")
D <- as.matrix(D)
# 4.2 Definimos amplitud y la funcion 
sigma = 0.05 # 100.0
gaussian <- function(x){
    s=sigma
    return (exp( (-x*x)/s ))
}
# 4.3 Calculamos el kernel
K <- sapply(D, gaussian)
dim(K) <- c(n+n, n+n)
# 4.4 Centramos kernel
K <- C %*% K %*% C

##########################################################################
##########################################################################
##########################################################################
# Calculo de componentes principales a traves de SVD de K
K.svd <- svd(K)
C1 <- K.svd$u[, 1]
C2 <- K.svd$u[, 2]
components <- c(C1, C2)
dim(components) <- c(n+n, 2)

scores <- K %*% components
plot(scores)
points(scores[1:n,], col="red")
points(scores[(n+1):(n+n),], col="blue")
##########################################################################
##########################################################################
##########################################################################
library(tsne)

set.seed(123)
tsne_reduction <- tsne(X, k = 2, perplexity = 25, epoch = 100)
plot(tsne_reduction)
points(tsne_reduction[1:n,], col="red")
points(tsne_reduction[(n+1):(n+n),], col="blue")

