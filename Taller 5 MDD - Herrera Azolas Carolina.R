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