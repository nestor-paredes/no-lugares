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

#rm(no_lugares) #eliminando dataframe original 

glimpse(no_lug) #verificando datos 
         
#Ejecutando ACM

nolugares.acm <- MCA(no_lug, quali.sup = 2:4, quanti.sup = 1 )

# Resultados 

nolugares.acm  
nolugares.acm$var$contrib #contribuciones de las modalidades 
mat_disyun <- nolugares.acm$call$Xtot #matríz disyuntiva 
rm(mat_disyun)

nolugares.acm$var$coord

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
    
    rm(coor_dim.1, dim.1.cat, dim.1.niveles, no_lug_coordenadas) #eliminando objetos de más 
    
    
    table(no_lug_ind_cat$tipo_2, no_lug_ind_cat$dim.1.cat) #comprobando 
    #Las cátedras con niveles bajos, lo mismo para el c3, programas medios y altos, seminarios medios y altos 

    #Creando índice categórico de la dim 2, usando cut simple 
    
    coor_dim.2 <- nolugares.acm$ind$coord[,2]  #exrayendo coordenadas dim 2
    dim.2.cat <- cut(coor_dim.2, breaks = 3, labels = c("Bajo", "Medio", "Alto"))
    dim.2.niveles <- as.data.frame(dim.2.cat) %>% 
      mutate(i_d = row_number())
    
    no_lug_index_cat <- no_lug_ind_cat %>% 
      left_join(dim.2.niveles)
    
    no_lug_index_cat

    rm(coor_dim.2, dim.2.cat, dim.2.niveles, no_lug_ind_cat)
    
    #índice ponderado
    #¡¡esto es un artefacto que no tiene mucho sentido!!
    coord.1.2 <- nolugares.acm$ind$coord[,1:2]
    coord.1.2 
    ind.ponderado <- nolugares.acm$ind$coord[,1:2] %*% c(.19, .14) #producto matricial coords * % inercia p/dim, 
    ind.ponderado
    
    ind.ponderado.cat <- cut(ind.ponderado, #creando los niveles 
                     breaks = quantile(ind.ponderado, probs = c(0, 1/3, 2/3, 1)), 
                     labels = c("Bajo", "Medio", "Alto"), 
                     include.lowest = TRUE)
    
    ind.ponderado.niveles <- as.data.frame(ind.ponderado.cat) %>% #creando dataframe y asignando id
      mutate(i_d = row_number())
    
    no_lug_ind_gen <- no_lug_index_cat %>%  #uniendo indice categórico
      left_join(ind.ponderado.niveles)
    
    nolugares.ind.ponderado.acm <- MCA(no_lug_ind_gen, quali.sup = c(2,3,4,25,26,27) , quanti.sup = c(1,23,24))
    
    plot(nolugares.ind.ponderado.acm , invisible=c("var"), autoLab = "yes",cex=1, habillage = "ind.ponderado.cat", col.quali.sup = "blue",
         selectMod = c("CATEDRA", "SEMINARIO", "PROGRAMA", "CENTRO")) 
    