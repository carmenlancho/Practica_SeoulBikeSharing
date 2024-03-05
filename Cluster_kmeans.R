library(tidyverse)  #  manipulación de datos
library(cluster)    # algoritmos de clustering 
library(factoextra) # algoritmos de clustering & visualización

head(USArrests)

df <- USArrests


# escalado de todas las variables
df <- scale(df)

head(df)
# Calculamos la distancia euclidea
distance <- get_dist(df) 
# Visualizacion de la matriz de distancias
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Ejecutamos el algoritmo kmeans
set.seed(123)
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

k2$cluster

k2 



# Visualizacion
fviz_cluster(k2, data = df)

#############################################################
#####                 NUMERO DE CLUSTERS                #####
#############################################################

#### METODO DEL CODO
# Reproducible
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")

k4 <- kmeans(df, centers = 4, nstart = 25)

fviz_cluster(k4, data = df)

#### SILHOUETTE
fviz_nbclust(df, kmeans, method = "silhouette")

library(cluster)
sil <- silhouette(k2$cluster, dist(df))
fviz_silhouette(sil)


#### GAP
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,K.max = 10, B = 50)

print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

# k= 3
k3 <- kmeans(df, centers = 3, nstart = 25)
fviz_cluster(k3, data = df)


### LIBRERIA NbClust
library("NbClust")
nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

library(parameters)

n_clust <- n_clusters(as.data.frame(df),
                      package = c("easystats", "NbClust", "mclust"),
                      standardize = FALSE)
n_clust
plot(n_clust)



############### ESTUDIO DE LOS CLUSTERS
USArrests %>%
  mutate(Cluster = k4$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


