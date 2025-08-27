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

no_lugares <- read.csv("Datos/nolugares.csv")

no_lug <- no_lugares [ c("area","tipo_2", "com_dir", 
                         "asesor_ext", "com_tec","coordinadxr", 
                         "secret", "particip","financia",
                         "period", "colab", "invest",
                         "docenc", "difus", "interdis",
                         "intercam", "vincula", "espacio", 
                         "editorial","coord","contrata")]
#Ejecutando ACM

nolugares.acm <- MCA(no_lug, quali.sup = c("period","tipo_2","area"))
