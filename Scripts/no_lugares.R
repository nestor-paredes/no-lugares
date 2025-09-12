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

nolugares.base <- read_csv("Datos/nolugares.csv")

nolugares.1 <- nolugares.base %>% 
  select(tipo_2, period, area,
         com_dir, asesor_ext, com_tec, 
         coordinadxr, secret, particip, 
         financia, colab,invest, 
         docenc, difus, interdis, 
         intercam, vincula, espacio, 
         editorial, coord)

#rm(no_lugares) #eliminando dataframe original 

glimpse(nolugares.1) #verificando datos 
         
#Ejecutando ACM

#nolugares.acm <- MCA(nolugares.2, quali.sup = 2:4, quanti.sup = 1 )

nolugares.acm <- MCA(nolugares.1, quali.sup = c(1,2,3)) 

# Resultados 
nolugares.acm
mat_disyun <- nolugares.acm$call$Xtot #matriz disyuntiva 
mat_disyun
rm(mat_disyun)

nolugares.acm$var


#Eigenvalues
nolugares.acm$eig
nolugares.acm$eig[,1] #Extrayendo % acumulados 

write_clip(round(nolugares.acm$eig,2)) 

barplot(nolugares.acm$eig[,2], main = "Proporción de aporte de los valores propios")

#Guardando gráfico 
png("C:/Users/Néstor/Dropbox/Archivo R/Proyectos/no-lugares/Gráficos/valores_raiz.png", 
    width = 800, height = 600)
barplot(nolugares.acm$eig[,2], main = "Proporción de aporte de los valores propios")
dev.off()    
       
#Contribuciones 
contrib <- nolugares.acm$var$contrib[,1:3]
round(contrib,3)
write_clip(round(contrib,2)) #copiando a portapapeles 


contrib <- nolugares.acm$ind$contrib[,1:2]
round(contrib,3)
write_clip(round(contrib,2))


#Coordenadas 
coord <- nolugares.acm$var$coord[, 1:2]
round(coord,2)
write_clip(round(coord,2))

#Coordenada individuos 
coord <- nolugares.acm$ind$coord[, 1:2]
round(coord,2)
write_clip(round(coord,2))

#Cos2
cos2 <- nolugares.acm$var$cos2[, 1:3]
round(cos2,3)
write_clip(round(cos2,3))

rm (contrib, coords)
#Aporte de los individuos
nolugares.acm$ind

#########################
#Seleccionando dimensiones 

#############
#Dim 1 y Dim 2
plot(nolugares.acm , invisible=c("ind"), axes=c(1,2), 
     autoLab = "yes", cex=.7,  col.var = "black", 
     col.quali.sup = "blue", title = "A) Dimensiones 1 y 2") 

#Zoom al origen 
plot(nolugares.acm , invisible=c("ind", "quali.sup"), axes=c(1,2), 
     xlim = c(-.5,1.5), ylim = c(-.5,.8), autoLab = "yes", 
     cex=.9,  col.var = "black", unselect = .7, 
     selectMod = c("intercamb","no_intercam", "espac", "no_espac",
                   "no_vincula", "vincula", "com_dir", "coord", 
                   "investiga","no_investiga", "colab","no_colab", 
                   "edito", "no_edito", "interdisc", "docen", "coord", 
                   "no_docen", "no_coord", "coordinadxr", "difusión")) 

#Las modalidades teóricamente relevantes 
plot(nolugares.acm , invisible=c("ind"), axes=c(1,2),  autoLab = "yes", 
     cex=.9,  col.var = "black", col.quali.sup = "blue", unselect = .7, 
     selectMod = c("mixta", "mix_int", "UNAM", "1 a 5", "16 a 20","11 a 15", 
                   "6 a 10", "cero","asesor_ext","com_dir", "coordinadxr",
                   "com_tec", "secretaria", "intercamb", "espac", "vincula", 
                   "contrata", "difusión", "coord", "interdisc", "edito", 
                   "docen", "colab", "investiga", "CATEDRA", "SEMINARIO", 
                   "PROGRAMA", "CENTRO", "1991-2005","2006-2010", 
                   "2011-2015","2016-2020", "2021-2024", "humanidades",
                   "ciencias")) 

#Espacio factorial de los individuos dim 1 y 2
plot(nolugares.acm, choix = c("ind"), axes=c(1,2),invisible=c("var"), 
     col.quali.sup = "blue", autoLab = "yes",cex=.9)


#############
#Dim 1 y Dim 3
plot(nolugares.acm , invisible=c("ind"), axes=c(1,3), 
     autoLab = "yes", cex=.7,  col.var = "black", 
     col.quali.sup = "blue", title = "A) Dimensiones 1 y 3") 

#Las modalidades teóricamente relevantes 
plot(nolugares.acm , invisible=c("ind"), axes=c(1,3),  autoLab = "yes", 
     cex=.9,  col.var = "black", col.quali.sup = "blue", unselect = .7, 
     selectMod = c("mixta", "mix_int", "UNAM", "1 a 5", "16 a 20","11 a 15", 
                   "6 a 10", "cero","asesor_ext","com_dir", "coordinadxr",
                   "com_tec", "secretaria", "intercamb", "espac", "vincula", 
                   "contrata", "difusión", "coord", "interdisc", "edito", 
                   "docen", "colab", "investiga", "CATEDRA", "SEMINARIO", 
                   "PROGRAMA", "CENTRO", "1991-2005","2006-2010", 
                   "2011-2015","2016-2020", "2021-2024", "humanidades",
                   "ciencias")) 

#Espacio factorial de los individuos dim 1 y 3
plot(nolugares.acm, choix = c("ind"), axes=c(1,3),invisible=c("var"), 
     col.quali.sup = "blue", autoLab = "yes",cex=.9)

#############
#Dim 2 y Dim 3
plot(nolugares.acm , invisible=c("ind"), axes=c(2,3), 
     autoLab = "yes", cex=.7,  col.var = "black", 
     col.quali.sup = "blue", title = "A) Dimensiones 2 y 3") 

#Las modalidades teóricamente relevantes 
plot(nolugares.acm , invisible=c("ind"), axes=c(2,3),  autoLab = "yes", 
     cex=.9,  col.var = "black", col.quali.sup = "blue", unselect = .7, 
     selectMod = c("mixta", "mix_int", "UNAM", "1 a 5", "16 a 20","11 a 15", 
                   "6 a 10", "cero","asesor_ext","com_dir", "coordinadxr",
                   "com_tec", "secretaria", "intercamb", "espac", "vincula", 
                   "contrata", "difusión", "coord", "interdisc", "edito", 
                   "docen", "colab", "investiga", "CATEDRA", "SEMINARIO", 
                   "PROGRAMA", "CENTRO", "1991-2005","2006-2010", 
                   "2011-2015","2016-2020", "2021-2024", "humanidades",
                   "ciencias")) 

#Espacio factorial de los individuos dim 2 y 3
plot(nolugares.acm, choix = c("ind"), axes=c(2,3),invisible=c("var"), 
     col.quali.sup = "blue", autoLab = "yes",cex=.9)

###########################
#Clasificación 

nolugares.hc <-HCPC(nolugares.acm,nb.clust=6,consol=FALSE,graph=T)
nolugares.hc<-HCPC(nolugares.acm,nb.clust=6,consol=FALSE,graph=T, method = "ward")


nolugares.hc$call$t$nb.clust
no_lug_8.HCPC$call$t$nb.clust




plot(nolugares.acm, invisible=c("var", "quali.sup"), autoLab = "yes",cex=1, 
           unselect = .7, select = c("8","4","21", "9", "58",
                                     "27", "22", "53", "57", "48",
                                     "50", "47"), title = "Ciencias")






#Modalidades relevantes para la dim 1
plot(nolugares.acm , invisible=c("ind"), axes=c(1,2), autoLab = "yes", 
     cex=.9, col.var = "black", unselect = .7,  col.quali.sup = "blue", 
     title = "Modalidades con mayor aporte de inercia y calidad a la Dimensión 1",
     selectMod = c("intercamb","no_intercam", "mixta", "UNAM", "mix_int", "1 a 5", 
                   "11 a 15", "16 a 20","espac", "no_espac","no_vincula", "vincula", 
                   "no_colab"))

#Modalidades relevantes para la dim 2
plot(nolugares.acm , invisible=c("ind"), axes=c(1,2), autoLab = "yes", 
     cex=.9, col.var = "black", unselect = .7,  col.quali.sup = "blue",
     title = "a) Dimensión 2: Modalidades con mayor aporte de inercia y calidad",
     selectMod = c("asesor_ext","no_com_tec", "secretaría","com_tec","docen",
                   "no_secretaría", "coord", "no_interdisc", "no_docen", 
                   "no_asesor_ex", "interdisc", "no_vincula"))




#Combinaciones de dimensiones 1 a 3, variables suplementarias
p1 <- plot(nolugares.acm , invisible=c("ind"), axes=c(1,2), autoLab = "yes", 
           cex=.7, col.var = "black", unselect = .7,  col.quali.sup = "blue",
           selectMod = c("CATEDRA", "SEMINARIO", "PROGRAMA", "CENTRo", 
                         "1991-2005", "2006-2010", "2011-2015","2016-2020", 
                         "2021-2024", "ciencias", "humanidades" ))
p1

p2 <- plot(nolugares.acm , invisible=c("ind"), axes=c(1,3), autoLab = "yes", 
           cex=.7, col.var = "black", unselect = .7,  col.quali.sup = "blue",
           selectMod = c("CATEDRA", "SEMINARIO", "PROGRAMA", "CENTRo", 
                         "1991-2005", "2006-2010", "2011-2015","2016-2020", 
                         "2021-2024", "ciencias", "humanidades" ))
p2

p3 <- plot(nolugares.acm , invisible=c("ind"), axes=c(2,3), autoLab = "yes", 
           cex=.7, col.var = "black", unselect = .7,  col.quali.sup = "blue",
           selectMod = c("CATEDRA", "SEMINARIO", "PROGRAMA", "CENTRo", 
                         "1991-2005", "2006-2010", "2011-2015","2016-2020", 
                         "2021-2024", "ciencias", "humanidades" ))
p3

#Agrupando los gráficos
p1 + p2 + p3
p1 / (p2 + p3)
rm(p1, p2, p3)













#Creando indices con las coordenadas de las primeras dimensiones 

coord <- nolugares.acm$ind$coord[,1:2] #extrayendo dimensiones (coordenadas)

coord.dataframe <- as.data.frame(coord) %>%  #asignando un id
  mutate(i_d = row_number())

nolugares <- nolugares.1 %>% 
  left_join(coord.dataframe, by = "i_d") #uniendo dimensiones

rm(coord, coord.dataframe) #eliminando objetos de más 

#Creando índice categórico de la dim 1

coor.dim.1 <- nolugares.acm$ind$coord[,1]  #extrayendo coordenadas dim 1

dim.1.cat <- cut(coor.dim.1, #creando los niveles 
                 breaks = quantile(coor.dim.1, probs = c(0, 1/3, 2/3, 1)), 
                 labels = c("Alto", "Medio", "Bajo"), 
                 include.lowest = TRUE)

dim.1.niveles <- as.data.frame(dim.1.cat) %>% #creando dataframe y asignando id
  mutate(i_d = row_number())

dim.1.niveles

nolugares <- nolugares %>%  #uniendo indice categórico
  left_join(dim.1.niveles, by = "i_d")

rm(coor.dim.1,dim.1.cat,dim.1.niveles)

#Las cátedras con niveles bajos, lo mismo para el c3, programas medios y altos, seminarios medios y altos 
table(nolugares$tipo_2, nolugares$dim.1.cat) #comprobando 

#Creando índice categórico de la dim 2, usando cut simple 

coor_dim.2 <- nolugares.acm$ind$coord[,2]  #extrayendo coordenadas dim 2

dim.2.cat <- cut(coor_dim.2, breaks = 3, labels = c("Bajo", "Medio", "Alto"))

dim.2.niveles <- as.data.frame(dim.2.cat) %>% 
  mutate(i_d = row_number())

nolugares <- nolugares %>% 
  left_join(dim.2.niveles, by = "i_d")

nolugares

rm(coor_dim.2, dim.2.cat, dim.2.niveles)

#índice ponderado
#¡¡esto es un artefacto que no tiene mucho sentido!!

coord.1.2 <- nolugares.acm$ind$coord[,1:2]
coord.1.2 
ind.ponderado <- nolugares.acm$ind$coord[,1:2] %*% c(.19, .14) #producto matricial coords * % inercia p/dim, se puede/debe normaliar tambien
ind.ponderado

ind.ponderado.cat <- cut(ind.ponderado, #creando los niveles 
                         breaks = quantile(ind.ponderado, probs = c(0, 1/3, 2/3, 1)), 
                         labels = c("Bajo", "Medio", "Alto"), 
                         include.lowest = TRUE)

ind.ponderado.niveles <- as.data.frame(ind.ponderado.cat) %>% #creando dataframe y asignando id
  mutate(i_d = row_number())

nolugares <- nolugares %>%  #uniendo indice categórico
  left_join(ind.ponderado.niveles, by = "i_d")

nolugares.ind.ponderado.acm <- MCA(nolugares, 
                                   quali.sup = c(2,3,4,25,26),
                                   quanti.sup = c(1,23,24))

plot(nolugares.ind.ponderado.acm , invisible=c("var"), 
     autoLab = "yes",cex=1, habillage = "ind.ponderado.cat", 
     col.quali.sup = "blue", 
     selectMod = c("CATEDRA", "SEMINARIO", "PROGRAMA", "CENTRO"))


#######################
#Explorando opciones de selección 

#Explorando la varianza de las dos dimensiones para ver el aporte de los individuos 
nolugares.acm$ind$contrib
nolugares.acm$ind$contrib [,c(1,2)]
coords <- nolugares.acm$ind$coord[, 1:2]
coords

#El valor arrojado nos dice qué tan extendido o disperso está el individuo en el espacio factorial
#Indica qué individuos  tienen alta dispersión o que no se concentran en una dimensión específica, 
#si las modalidades tienen baja varianza quiere decir que están bien representadas en el plano factorial. 
#Varianza alta indica mayor dispersión en el espacio: no se concentra en una dimensión, varianza baja: alineación a una dimensión 


#Esto no es lo mismo que los individuos que menos o más inercia aportan, están relacionados pero no son lo mismo 
apply(coords, 1, var) 
var.dim.1.2 <-apply(coords, 1, var) #calcula la varianza para cada fila de este objeto
var.dim.1.2
menos_var <- names(sort(variabilidad)[1:10])
menos_var

var.dim.1.2 <- as.data.frame(var.dim.1.2) %>%  #integrando a la base de datos 
  mutate(i_d = row_number())

nolugares <- nolugares %>% 
  left_join(var.dim.1.2, by = "i_d")

rm(var.dim.1.2, menos_var, variabilidad)

#######Revisar
    
    ine_dim.1 <- no_lug_8.acm$ind$contrib [,1]
    ine_dim.1
    sort(ine_dim.1)[1:10]
    sort(ine_dim.1)[48:57]
    p1 <- plot(no_lug_8.acm, invisible=c("var", "quali.sup"), autoLab = "yes",cex=1.5,  
               unselect = .7, select = c("55","9","2", "33", "4",
                                         "11","14","6", "5", "19"), 
               title = "A. casos c/menor inercia Dim 1") 
    
    p1
    
    p3 <- plot(no_lug_8.acm, invisible=c("var", "quali.sup"), autoLab = "yes",cex=1.5,  
               unselect = .7, select = c("51","37","42", "44", "35",
                                         "40","43","41", "45", "46"), 
               title = "C. casos c/mayor inercia Dim 1") 
    
    p3
    
    ine_dim.2 <- no_lug_8.acm$ind$contrib [,2]
    sort(ine_dim.2)[1:10]
    sort(ine_dim.2)[47:57]
    
    
    p2 <- plot(no_lug_8.acm, invisible=c("var", "quali.sup"), autoLab = "yes",cex=1.5,  
               unselect = .7, select = c("9","46","38", "15", "57",
                                         "17","5","6", "45", "42"), title = "B. casos c/menor inercia Dim 2") 
    
    p4 <- plot(no_lug_8.acm, invisible=c("var", "quali.sup"), autoLab = "yes",cex=1.5,  
               unselect = .7, select = c("32","13","49", "25", "20",
                                         "23","29","24", "30", "55"), title = "D. casos c/mayor inercia Dim 2") 
    (p1 | p2) / (p3 | p4) 
    
    coords
    
    inercia <-rowSums(coords^2)
    inercia
    id_inercia.a <- names(sort(inercia)[1:10])
    id_inercia.b <- names(sort(inercia)[48:57])
    #ind_mayor_inercia <- names(sort(inercia_ind, decreasing = TRUE)[1:5])
    id_inercia.a 
    id_inercia.b 
    
    p5 <- plot(no_lug_8.acm, invisible=c("var", "quali.sup"), autoLab = "yes",cex=1.5,  
               unselect = .7, select = c("9","6","5", "15", "33",
                                         "57","14","11", "2", "3"), title = "A. Casos c/menor inercia Dim 1y 2") 
    
    p5
    
    p6 <- plot(no_lug_8.acm, invisible=c("var", "quali.sup"), autoLab = "yes",cex=1.5,  
               unselect = .7, select = c("58","30","24", "47", "43",
                                         "40","35","41", "45", "46"), title = "B. Casos c/mayor inercia Dim 1y 2") 
    p6
    p5+p6
    
    plot(no_lug_8.acm , invisible=c("ind"), axes=c(1,2), autoLab = "yes", cex=.7,  col.var = "black", col.quali.sup = "blue", title = "A) Dimensiones 1 y 2", select = "contrib 10")
    
    
