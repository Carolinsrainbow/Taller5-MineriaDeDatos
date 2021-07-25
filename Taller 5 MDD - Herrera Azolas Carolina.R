####Taller 5

## Incorporación de los datos a la biblioteca 

datos <- read.csv("/Users/user/Desktop/SouthGermanCredit.csv",sep=";", header=TRUE)

# Instalamos los paquetes necesarios, en caso que no los tengamos instalados

install.packages("cluster")
install.packages("factoextra")
install.packages("tidyverse")

# Cargamos las librerias que utilizaremos

library(cluster)
library(factoextra)
library(tidyverse)

#Visualización general 
head(datos)

## Resumen estadístico
summary(datos)

## Cantidad de datos 
length(datos$personal)

# Instalamos los paquetes necesarios, en caso que no los tengamos instaladas
install.packages("class")

# Cargamos las librerias que utilizaremos
library(class)

######### PRIMER CASO

## Elegimos las variables para el análisis y las que son categóricas se normalizan

datos_adaptados <- as.data.frame(cbind(
  historial_delay = (datos$history == 0),
  historial_critic = (datos$history == 1),
  historial_new =(datos$history ==2),
  historial_clean = (datos$history == 3),
  savings = datos$savings,
  employed = datos$employed,
  job = datos$job,
  foreign = datos$foreign,
  credit = datos$credit,
  persons=datos$persons))
# Normalizamos los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }

## datos_norm <- as.data.frame(lapply(datos_small, normalize))
datos_norm1 <- as.data.frame(lapply(datos_adaptados, normalize))

## Elegimos el K

clusters <- c(1,2,3,4,5,6,7,8,9,10)
codo <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 1:10) { 
  codo[k] <- kmeans(datos_norm1,
                    centers = k,
                    nstart = 25)$tot.withinss
}

plot(clusters, codo, type = "l")

##Obtemos K=2! #Let'sDoIt

## Aplicamos silueta
silueta <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 2:10) { 
  modelo_aux <- kmeans(datos_norm1,
                       centers = k,
                       nstart = 25)
  silueta_aux <- silhouette(modelo_aux$cluster, dist(datos_norm1))
  silueta[k] <- mean(silueta_aux[, 3])
}

## Gráficamos silueta
plot(clusters, silueta, type = "l")

## 10 es el peak pero no se respalda con el gráfico anterior

# K Means con K = 10

k10 <- kmeans(datos_norm1,
              centers = 10,
              nstart = 25)

## Agregamos la columna cluster a la tabla original de variables elegidas
datos_adaptados$cluster <- k10$cluster


view(datos_adaptados
     %>% group_by(cluster)
     %>% summarise_all(mean))

# K Means con K = 6

k6 <- kmeans(datos_norm1,
             centers = 6,
             nstart = 25)

## Agregamos la columna cluster a la tabla original de variables elegidas
datos_adaptados$cluster <- k6$cluster


view(datos_adaptados
     %>% group_by(cluster)
     %>% summarise_all(mean))

# K Means con K = 2

k2 <- kmeans(datos_norm1,
             centers = 2,
             nstart = 25)

## Agregamos la columna cluster a la tabla original de variables elegidas
datos_adaptados$cluster <- k2$cluster


view(datos_adaptados
     %>% group_by(cluster)
     %>% summarise_all(mean))



##### Caso 2

datos_adaptados2 <- as.data.frame(cbind(savings = datos$savings,
                                       employed = datos$employed,
                                       job = datos$job,
                                       foreign = datos$foreign,
                                       credit = datos$credit))

# Normalizamos los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
datos_norm2 <- as.data.frame(lapply(datos_adaptados, normalize))

## Elegimos el K

clusters2 <- c(1,2,3,4,5,6,7,8,9,10)
codo <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 1:10) { 
  codo[k] <- kmeans(datos_norm2,
                    centers = k,
                    nstart = 25)$tot.withinss
}

plot(clusters, codo, type = "l")

## K=2


## Aplicamos silueta
silueta2 <- c(0,0,0,0,0,0,0,0,0,0)
for (k in 2:10) { 
  modelo_aux2 <- kmeans(datos_norm2,
                       centers = k,
                       nstart = 25)
  silueta_aux2 <- silhouette(modelo_aux2$cluster, dist(datos_norm2))
  silueta[k] <- mean(silueta_aux2[, 3])
}

## Gráficamos silueta
plot(clusters2, silueta2, type = "l")


