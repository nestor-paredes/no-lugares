#paquetes 

library(installr)
library(readr)
library(dplyr)
library(tidyverse)
library(ggthemes)
library(GGally)
library(corrplot)
library(Hmisc)
library(egg)
library(tidyr)
library(viridis)
library(clipr)
library(patchwork)
library(FactoMineR)
library(factoextra)
library(Factoshiny)

#Cargando datos y seleccionando variables


no_lugares <- read_csv("Datos/nolugares.csv")

no_lug <- no_lugares %>% 
  select(i_d, tipo_2, period, area,
         com_dir, asesor_ext, com_tec, 
         coordinadxr, secret, particip, 
         financia, colab,invest, 
         docenc, difus, interdis, 
         intercam, vincula, espacio, 
         editorial, coord, contrata)

rm(no_lugares) #eliminando dataframe original 

glimpse(no_lug) #verificando datos 
         
#Ejecutando ACM

nolugares.acm <- MCA(no_lug, excl = 1, quali.sup = 2:4, quanti.sup = 1 )

# Resultados 

nolugares.acm  

    #Creando indices con las coordenadas de las prineras dimensiones 
    
    coord <- nolugares.acm$ind$coord[,1:2] #estrayendo dimensiones (coordenadas)
    
    coord.dataframe <- as.data.frame(coord) %>%  #asignando un id
      mutate(i_d = row_number())
    
    no_lug_coordenadas <- no_lug %>% 
      left_join(coord.dataframe, by = "i_d") #uniendo dimensiones
    
    rm(coord, coord.dataframe) #eliminando objetos de más 
    
    #Creando índice categórico de la dim 1
    
    coor_dim.1 <- nolugares.acm$ind$coord[,1]  #exrayendo coordenadas dim 1
    
    dim.1.cat <- cut(coor_dim.1, #creando los niveles 
                      breaks = quantile(coor_dim.1, probs = c(0, 1/3, 2/3, 1)), 
                      labels = c("Alto", "Medio", "Bajo"), 
                      include.lowest = TRUE)
    
    dim.1.niveles <- as.data.frame(dim.1.cat) %>% #creando dataframe y asignando id
      mutate(i_d = row_number())
    dim.1.niveles
    
    no_lug_ind_cat <- no_lug_coordenadas %>%  #uniendo indice categórico
      left_join(dim.1.niveles)
    
    rm(coor_dim.1, dim.1.cat, dim.1.niveles) #eliminando objetos de más 
    
    
    table(no_lug_ind_cat$tipo_2, no_lug_ind_cat$dim.1.cat) #comprobando 
#
