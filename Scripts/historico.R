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

historico <- read_csv("Datos/no_lugares_hist.csv")

summary(historico)
glimpse(historico)

library(ggplot2)
library(dplyr)

historico %>%
  filter(!is.na(Año_de_emergencia), Año_de_emergencia != "s/f") %>%
  count(Año_de_emergencia, Tipo_NL) %>%
  pivot_wider(names_from = Año_de_emergencia, values_from = n, values_fill = 0)


historico %>%
  filter(!is.na(Año_de_emergencia), Año_de_emergencia != "s/f") %>%
  ggplot(aes(x = Año_de_emergencia, fill = Tipo_NL)) +
  geom_bar(position = "stack") +
  labs(
    title = "Total de registros por año según tipo de no-lugar",
    x = "Año de emergencia",
    y = "Total de registros",
    fill = "Tipo"
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 6),
    panel.grid = element_blank()
  )

historico$tiene_fecha <- ifelse(historico$Año_de_emergencia == "s/f", "Sin fecha", "Con fecha")
historico$tiene_fecha <- as.factor(historico$tiene_fecha)



historico %>%
  mutate(tiene_fecha = ifelse(Año_de_emergencia == "s/f", "Sin fecha", "Con fecha")) %>%
  ggplot(aes(x = Tipo_NL, fill = tiene_fecha)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proporción de registros sin fecha por tipo de no-lugar",
    x = "Tipo de no-lugar",
    y = "Proporción",
    fill = ""
  ) +
  theme_grey()  +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )


historico %>%
  mutate(tiene_fecha = ifelse(Año_de_emergencia == "s/f", "Sin fecha", "Con fecha")) %>%
  group_by(Tipo_NL) %>%
  mutate(prop_sf = mean(tiene_fecha == "Sin fecha")) %>%
  ungroup() %>%
  mutate(Tipo_NL = reorder(Tipo_NL, prop_sf)) %>%
  ggplot(aes(x = Tipo_NL, fill = tiene_fecha)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proporción de no lugares sin fecha",
    y = "Proporción",
    x = "Tipo de no-lugar",
    fill = ""
  ) +
  theme_grey() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid = element_blank()
  )

